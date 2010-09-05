(*pp camlp4o -I $PIQI_ROOT/camlp4 pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


module C = Piqi_common
open C


module Idtable = Piqi_db.Idtable


(* start in boot_mode by default, it will be switched off later (see below) *)
let boot_mode = ref true


(* processing hooks to be run at the end of Piqi module load & processing *)
let processing_hooks = ref []

let add_processing_hook (f :T.piqi -> unit) =
  (* run the hook on the boot module if it has been loaded already *)
  (match !boot_piqi with
    | None -> ()
    | Some x -> f x
  );
  processing_hooks := f :: !processing_hooks


let add_piqdef idtable (piqdef:T.piqdef) = 
  let name = piqdef_name piqdef in
  debug "add_piqdef: %s\n" name;
  if Idtable.mem idtable name
  then
    let prev_def = Idtable.find idtable name in
    error piqdef
      ("duplicate type definition " ^ quote name ^ "\n" ^
       error_string prev_def "first defined here")
  else
    Idtable.add idtable name piqdef


let add_piqdefs idtable defs =
  List.fold_left add_piqdef idtable defs


let add_imported_piqdef idtable (piqdef:T.piqdef) =
  let open Import in
  let import = 
    match get_parent piqdef with
      | `import x -> x
      | _ -> assert false
  in
  (* while adding imported defs to the idtable, transform definition's names to
   * contain module's namespace *)
  let name = some_of import.name ^ "/" ^ piqdef_name piqdef in
  debug "add_imported_piqdef: %s\n" name;
  Idtable.add idtable name piqdef


let add_imported_piqdefs idtable defs =
  List.fold_left add_imported_piqdef idtable defs


let find_def idtable name =
  try Idtable.find idtable name
  with Not_found ->
    error name ("unknown type " ^ quote name)


let resolve_typeref map t =
  match t with
    | `name name ->
        let def = find_def map name in
        (def: T.piqdef :> T.typeref)
    | _ -> t (* already resolved *)


(* XXX: is there a way to avoid code duplicaton here? *)
let resolve_field_typeref map f =
  let open F in
  match f.typeref with
    | None -> () (* flag *)
    | Some t ->
        f.typeref <- Some (resolve_typeref map t)


let resolve_option_typeref map o =
  let open O in
  match o.typeref with
    | None -> ()
    | Some t ->
        o.typeref <- Some (resolve_typeref map t)


let resolve_typerefs map = function
  | `record r ->
      List.iter (resolve_field_typeref map) r.R#field
  | `variant v ->
      List.iter (resolve_option_typeref map) v.V#option
  | `alias a ->
      a.A#typeref <- resolve_typeref map a.A#typeref
  | `list l ->
      l.L#typeref <- resolve_typeref map l.L#typeref
  | _ -> ()


let check_name x =
  if not (Piqi_name.is_valid_name x)
  then error x ("invalid name: " ^ quote x)
  else ()


let check_scoped_name x =
  if not (Piqi_name.is_valid_scoped_name x)
  then error x ("invalid scoped name: " ^ quote x)
  else ()


let check_opt_name = function
  | None -> ()
  | Some x -> check_name x


let check_dup_names names =  
  match find_dups names with
    | None -> ()
    | Some name ->
        error name ("duplicate name: " ^ name)


let check_typeref obj (t:T.typeref) =
  match t with 
    | `name x -> check_scoped_name x
    | #T.piqdef ->
        error obj "use of type definition as type is prohibited"
    | _ -> ()


let error_noboot obj s =
  if !boot_mode || !Config.noboot
  then ()
  else error obj s


let check_no_builtin_type obj t =
  match t with 
    | #T.piqdef | `name _ -> check_typeref obj t
    | _ ->
        error_noboot obj "use of built-in types is allowed only from boot files or when running in \"noboot\" mode"


let check_field f =
  let open Field in
  begin
    begin
    check_opt_name f.name;
    match f.name, f.typeref with
      | None, None ->
          error f "name or type must be specified for a field"
      | _, Some t ->
          check_no_builtin_type f t
      | Some _, None -> (* flag *)
          begin
            (if f.mode <> `optional
            then error f "flags must be optional");

            (if f.default <> None
            then error f "flags may not specify default")
          end
    end;

    if f.default <> None && f.mode <> `optional
    then
      error f.default "default values may only be specified for optional fields"
  end


let check_record r =
  let name = r.R#name in
  let fields = r.R#field in
  if List.length fields = 0
  then warning r ("record " ^ quote name ^ " doesn't specify any fields");
  List.iter check_field fields


let check_option o =
  let open Option in
  begin
    check_opt_name o.name;
    match o.name, o.typeref with
      | _, Some t ->
          check_no_builtin_type o t
      | None, None ->
          error o "name or type must be specified for an option"
      | _ -> ()
  end


let check_variant v =
  (* TODO: check for co-variant loops *)
  let name = v.V#name in
  let options = v.V#option in
  if options = []
  then error v ("variant " ^ quote name ^ " doesn't specify any options");
  List.iter check_option options


let check_enum_option x =
  let open O in
  begin
    (* consts name must be defined and types must not *)
    if x.name = None
    then error x ("enum options must specify name");

    if x.typeref <> None
    then error x ("enum options must not specify type");

    check_opt_name x.name;
  end


let check_enum e =
  let name = e.E#name in
  let options = e.E#option in
  if options = []
  then error e ("enum " ^ quote name ^ " doesn't specify any options");
  List.iter check_enum_option options


let check_wire_type a wt =
  let t = unalias (`alias a) in
  match wt with
    | `varint | `zigzag_varint | `fixed32 | `fixed64 
    | `signed_varint | `signed_fixed32 | `signed_fixed64 when t = `int -> ()
    | `fixed32 | `fixed64 when t = `float -> ()
    | _ ->
        error a ("wire type " ^ quote (Piqi_wire.wire_type_name wt) ^
                 " is incompatible with piq type " ^ quote (piqi_typename t))


let check_alias a =
  let open A in
  begin
    check_typeref a a.typeref;
    (*
    check_no_builtin_type a a.typeref;
    *)
  end


let check_list l =
  let open L in
  begin
    check_no_builtin_type l l.typeref
  end


let check_def def =
  check_name (piqdef_name def);
  match def with
    | `record x -> check_record x
    | `variant x -> check_variant x
    | `enum x -> check_enum x
    | `alias x -> check_alias x
    | `list x -> check_list x


let check_resolved_alias a = 
  (* TODO: check for alias loops *)
  let open A in
  begin
    (* check for wire-types compatibility with piq types *)
    match a.wire_type with
      | None -> ()
      | Some x ->
          check_wire_type a x
    (* XXX: prohibit wire type overrides in upper-level aliases? (currently they
     * are silently ignored *)
  end


let check_resolved_def def =
  match def with
    | `record x ->
        let names = List.map (fun x -> name_of_field x) x.R#field in
        check_dup_names names
    | `variant x | `enum x ->
        let names = List.map (fun x -> name_of_option x) x.V#option in
        check_dup_names names
    | `alias x ->
        check_resolved_alias x
    | _ -> ()


let check_extension x =
  let open Extend in
  begin
    if x.name = []
    then error x ("extension doesn't specify any names");

    if x.quote = []
    then error x ("extension doesn't specify any extensions");

    List.iter
      (fun x ->
        if Piqi_name.has_parent x
        then
          error x "extensions of imported defintions are not supported yet")
      x.name;

    List.iter check_scoped_name x.name;
  end


let debug_loc prefix =
  debug "%s out count = %d, in count = %d\n" prefix !Piqloc.ocount !Piqloc.icount


let resolve_field_default idtable x =
  let open F in
  match x.default, x.typeref with
    | None, _ -> () (* no default *)
    | Some {T.Any.binobj = Some _ }, _ ->
        (* nothing to do -- object is already resolved *)
        ()
    | Some ({T.Any.ast = Some ast } as default), Some typeref ->
        let piqtype = C.piqtype typeref in
        let piqobj = Piqobj_of_piq.parse_obj piqtype ast in

        debug_loc "resolve_field_default(0)";

        (* NOTE: fixing (preserving) location counters which get skewed during
         * parsing defaults *)
        let ocount = !Piqloc.ocount in

        (* XXX: pack default to contain only binary representation of
         * the value, i.e. don't include object typename *)
        let binobj = Piqobj_to_wire.gen_binobj piqobj ~named:false in

        (* XXX: or just rather do Piqloc.ocount := !Piqloc.icount? *)
        Piqloc.icount := !Piqloc.icount + (!Piqloc.ocount - ocount);

        debug_loc "resolve_field_default(1)";

        default.T.Any.binobj <- Some binobj
    | _, None -> () (* there is no default for a flag *)
    | _ ->
        assert false (* either binobj or ast have to be defined *)


let resolve_defaults idtable = function
  | `record x ->
      List.iter (resolve_field_default idtable) x.R#field
  | _ -> ()


let copy_obj (x:'a) :'a =
  Obj.obj (Obj.dup (Obj.repr x))
let copy_obj x = reference copy_obj x


let copy_obj_list l = List.map copy_obj l
let copy_obj_list l = reference copy_obj_list l


let copy_variant x = V#{ x with option = copy_obj_list x.option }
let copy_record x = R#{ x with field = copy_obj_list x.field }
let copy_variant = reference copy_variant
let copy_record = reference copy_record


let copy_def (x:T.piqdef) =
  match x with
    | `record x -> `record (reference copy_record x)
    | `variant x -> `variant (reference copy_variant x)
    | `enum x -> `enum (reference copy_variant x)
    | `alias x -> `alias (copy_obj x)
    | `list x -> `list (copy_obj x)

let copy_def = reference copy_def (* preserve location information *)


let copy_defs defs = List.map copy_def defs


let copy_imports l = List.map copy_obj l


let resolve_defs idtable (defs:T.piqdef list) =
  (*
  (* a fresh copy of defs is needed, since we can't alter the original ones:
   * we need to resolve types & assign codes in order to resolve_defaults *)
  *)

  (* check definitions validity *)
  List.iter check_def defs;

  (* add definitions to the map: def name -> def *)
  let idtable = add_piqdefs idtable defs in

  (* resolve type references using the map *)
  List.iter (resolve_typerefs idtable) defs;

  (* check records, variants, enums for duplicate field/option names; check wire
   * types in aliases *)
  List.iter check_resolved_def defs;

  (* assign wire codes; if the are unassigned *)
  Piqi_wire.add_codes defs;

  (* resolve defaults ANY to OBJ using field types *)
  List.iter (resolve_defaults idtable) defs;

  (* return updated idtable *)
  idtable


let check_defs idtable defs =
  ignore (resolve_defs idtable (copy_defs defs))


(*
 * booting code; preparing the initial statically defined piqi defintions
 *)
let boot () = 
  let idtable = Idtable.empty in
  ignore (resolve_defs idtable T.piqdef_list)

let _ = boot ()

let is_piqi_def = function
  | `record { Record.name = "piqi" } -> true
  | _ -> false

let is_piqdef_def = function
  | `variant {V.name = "piqdef"} -> true
  | _ -> false

let _find_def f =
  let x = List.find f T.piqdef_list in
  (x :> T.piqtype)

let piqi_def = _find_def is_piqi_def

let piqdef_def = _find_def is_piqdef_def

let _ =
  begin
    (* turn boot mode off *)
    boot_mode := false;

    (* reset wire location counters *)
    Piqloc.icount := 0;
    Piqloc.ocount := 0;
  end


(* find piqi file in search paths given its (relative) splitted name *)
let find_piqi_file modname =
  let name = Piqi_file.make_os_path modname in (* revert slashes on Windows *)
  let found_dir = ref "" and found_name = ref "" in
  let check_file dir ext =
    let name = name ^ ext in
    let file_name = Filename.concat dir name in
    let res = Sys.file_exists file_name in
    if res then (found_dir := dir; found_name := name);
    res
  in
  if List.exists (fun dir ->
      if check_file dir ".piqi"
      then true
      else check_file dir ".proto.piqi") !Piqi_config.paths
  then
    !found_dir, !found_name
  else
    error modname ("piqi file is not found in path: " ^ quote name)


let find_piqi modname =
  (* NOTE: now supporting only local namespace *)
  let dir, fname = find_piqi_file modname in
  Filename.concat dir fname


let read_piqi_common fname piq_parser :T.ast =
  (* don't expand abbreviations until we construct the containing object *)
  let res = Piq_parser.read_all piq_parser ~expand_abbr:false in
  match res with
    | [] ->
        (* XXX: warning? *)
        piqi_error ("piqi file is empty: " ^ fname)
    | _ ->
        (* wrapping items in list to make them contents of "piqi" record *)
        let res = `list res in
        let startloc = (fname, 1, 1) in (* start location *)
        let ast = Piqloc.addlocret startloc res in
        (* now expand abbreviations *)
        Piq_parser.expand ast


let read_piqi_channel fname ch :T.ast =
  (* XXX: handle read errors *)
  let piq_parser = Piq_parser.init_from_channel fname ch in
  read_piqi_common fname piq_parser


let read_piqi_string fname content :T.ast =
  let piq_parser = Piq_parser.init_from_string fname content in
  read_piqi_common fname piq_parser


let open_piqi fname =
  try Pervasives.open_in fname
  with Sys_error s ->
    piqi_error ("error opening piqi file: " ^ s)


let read_piqi_file fname :T.ast =
  let ch = open_piqi fname in
  let res =
    try read_piqi_channel fname ch
    with x -> (* try ... after *)
      Pervasives.close_in ch;
      raise x
  in
  Pervasives.close_in ch;
  res


let check_modname x =
  if Piqi_name.is_valid_modname x
  then ()
  else error x ("invalid piqi module name: " ^ x)


let check_assign_module_name ?modname fname (piqi:T.piqi) =
  let open P in
  match piqi.modname, modname with
    | Some x, Some x' ->
        check_modname x;
        (* check that the requested module name corresponds to the module name
         * defined in the file *)
        if x <> x'
        then
          error piqi
            ("module loaded as " ^ quote x' ^ 
             " has different name " ^ quote x)
        else ()
    | Some x, None -> (* name is already defined for the module *)
        check_modname x
    | None, Some x -> 
        (* XXX: use separate "resolved_modename" field? *)
        piqi.modname <- modname
    | None, None ->
        (* basename + chop all extensions + underscores to dashes *)
        let basename = Piqi_file.basename fname in
        let name = Piqi_name.make_local_name basename in
        if Piqi_name.is_valid_name name
        then piqi.modname <- Some name
        else error piqi "piqi module name can not be derived from the file name"


(* XXX: demand explicit import name to avoid potential problems after renaming
 * imported module name? *)
let assign_import_name x piqi =
  let open Import in
  match x.name with
    | Some x -> (* import name is already defined *)
        check_name x
    | None ->
        (* derive import name from the original module's name *)
        let full_name = some_of piqi.P#modname in
        let name = Piqi_name.get_local_name full_name in
        x.name <- Some name


let mlobj_to_ast piqtype wire_generator mlobj =
  debug_loc "mlobj_to_ast(0)";
  let binobj = Piqirun_gen.gen_binobj wire_generator mlobj in
  debug_loc "mlobj_to_ast(1.5)";

  (* dont' resolve defaults when reading wire;
   * preseve the original setting *)
  let saved_resolve_defaults = !Piqobj_of_wire.resolve_defaults in
  Piqobj_of_wire.resolve_defaults := false;

  let piqobj = Piqobj_of_wire.parse_binobj ~piqtype binobj in

  Piqobj_of_wire.resolve_defaults := saved_resolve_defaults;

  let ast = Piqobj_to_piq.gen_obj piqobj in

  debug_loc "mlobj_to_ast(1)";
  ast


let mlobj_of_ast piqtype wire_parser ast =
  debug_loc "mlobj_of_ast(0)";

  (*
  (* initialize starting location code *)
  let max_count = max !T.icount !T.ocount in
  T.icount := max_count;
  T.ocount := max_count;
  *)

  (* We have to resolve defaults while reading piqi in order to provide correct
   * location bindings. It is not possible to "fix" skewed location bindings
   * in piqtype.ml after default values get parsed. We rather decided to fix
   * location bindings here -- see resolve_defaults function for details *)
  let saved_resolve_defaults = !Piqobj_of_piq.resolve_defaults in
  Piqobj_of_piq.resolve_defaults := true;

  (* XXX: find a better way to set this option *)
  Piqobj_of_piq.delay_unknown_warnings := true;

  let piqobj = Piqobj_of_piq.parse_obj piqtype ast in
  let binobj = Piqobj_to_wire.gen_binobj piqobj ~named:false in
  let _name, t = Piqirun_parser.parse_binobj binobj in
  let mlobj = wire_parser t in

  Piqobj_of_piq.delay_unknown_warnings := false;

  Piqobj_of_piq.resolve_defaults := saved_resolve_defaults;

  debug_loc "mlobj_of_ast(1)";
  mlobj


let parse_piqi ast =
  (* XXX: handle errors *)
  debug "parse_piqi(0)\n";
  (* use prepared static "piqi" definition to parse the ast *)
  let res = mlobj_of_ast piqi_def T.parse_piqi ast in
  debug "parse_piqi(1)\n";
  res


(* NOTE: naive, non-tail recursive. Remove duplicates from the list using
 * reference equality, preserves the initial order *)
let rec uniqq = function
  | [] -> []
  | h::t ->
      let t = uniqq t in
      if List.memq h t then t else h :: t


(* leave the first of the duplicate elements in the list instead of the last *)
let uniqq l =
  List.rev (uniqq (List.rev l))


let rec uniq = function
  | [] -> []
  | h::t ->
      let t = uniq t in
      if List.mem h t then t else h :: t


(* leave the first of the duplicate elements in the list instead of the last *)
let uniq l =
  List.rev (uniq (List.rev l))


(* get the list of unique piqi includes by traversing recursively through
 * include tree; the input piqi module will be put as the last element of the
 * list *)
let get_includes piqi =
  (* get the list of all included modules *)
  let l = flatmap (fun x ->
    let res = x.P#included_piqi in
    if res = [] (* the module is loaded, but hasn't been processed yet *)
    then error x "included piqi modules form a loop"
    else res) piqi.P#included_piqi in

  (* remove duplicates -- one module may be included from many modules *)
  let l = uniqq l in

  (* simple check for includes loop; provides very limited diagnostic *)
  if List.memq piqi l
  then error piqi "included piqi modules form a loop";

  (* put the original (input) piqi module at the last position of the list *)
  l @ [piqi]


let get_piqdefs modules =
  flatmap (fun x -> x.P#piqdef) modules


let get_extensions modules =
  flatmap (fun x -> x.P#extend) modules


let get_imports modules =
  flatmap (fun x -> x.P#resolved_import) modules


let get_custom_fields modules =
  let l = flatmap (fun x -> x.P#custom_field) modules in
  uniq l


let is_unknown_field custom_fields x =
  match x with
    | `named {T.Named.name = name} | `name name ->
        if List.mem name custom_fields
        then false (* field is a custom field, i.e. "known" *)
        else true
    | _ -> true


let check_unknown_fields ?prepend unknown_fields custom_fields =
  let unknown_fields =
    List.filter (is_unknown_field custom_fields) unknown_fields
  in

  let warn x =
    (* call the function for printing prepending warning message *)
    (match prepend with
      | Some f -> f ()
      | None -> ());
    Piqobj_of_piq.warn_unknown_field x
  in

  (* print warnings *)
  List.iter warn unknown_fields


let check_imports piqi =
  let rec check_dups = function
    | [] -> ()
    | h::t ->
        begin
          if List.exists (fun x -> h.Import#name = x.Import#name) t
          then error h ("duplicate import name " ^ quote (some_of h.Import#name));

          if List.exists (fun x -> h.Import#piqi == x.Import#piqi) t
          then warning h ("duplicate import module " ^ quote h.Import#modname);

          check_dups t
        end
  in
  check_dups piqi.P#resolved_import;

  (* get the list of all imported modules *)
  let l = List.map (fun x -> some_of x.Import#piqi) piqi.P#resolved_import in
  (* simple check for import loops; provides very limited diagnostic *)
  (* NOTE: import loops disallowed in Protobuf as well *)
  if List.memq piqi l
  then error piqi "imported piqi modules form a loop"


(* resolve extension names to correspondent piqdefs *)
let process_extension idtable x =
  let open Extend in
  let names = x.name in
  List.map (fun name -> (find_def idtable name, x)) names


(* From (key, value) list extract values for the specified key, and return the
 * list of extracted values and the remaining tuples; the order of both values
 * and remaining items is preserved *)
(* NOTE: not tail recursive *)
(* NOTE: using reference-based eq function (==) *)
let takeout key l =
  let rec aux values rem = function
    | [] -> List.rev values, List.rev rem
    | (key', value)::t when key' == key ->
        aux (value::values) rem t
    | h::t ->
        aux values (h::rem) t
  in aux [] [] l
    

(* group unsorted (key, value) pairs by their first element (key) and return
 * the list of groups containing (key, list of values) for each group *)
(* NOTE: using reference-based eq function (==) *)
let group_pairs l =
  let rec aux groups = function
    | [] -> List.rev groups
    | (key, value)::t ->
        let values, rem = takeout key t in
        aux ((key, value::values)::groups) rem
  in aux [] l


let apply_extensions piqdef extensions custom_fields =
  let trace' = !Piqloc.trace in
  (* Piqloc.trace := false; *)
  debug "apply_extensions(0)\n";
  let piqdef_ast = mlobj_to_ast piqdef_def T.gen_piqdef piqdef in
  let extension_entries =
    List.concat (List.map (fun x -> x.Extend#quote) extensions)
  in
  let extension_asts = List.map (fun x -> some_of x.Any#ast) extension_entries in
  let extended_piqdef_ast =
    match piqdef_ast with
     | `named ({T.Named.value = ((`list l) as _ref)} as x) ->
         let v = `list (l @ extension_asts) in

         let v = Piq_parser.piq_addrefret _ref v in

         let res = `named {x with T.Named.value = v} in

         ignore (Piq_parser.piq_addrefret x res);

         res

     | _ ->
         (* extensions can only be applied to named containers and all of
          * piqdefs are named containers *)
         assert false
  in
  let context_str = "while applying extensions to this definition ..." in
  debug "apply_extensions(1)\n";
  let extended_piqdef =
    try
      mlobj_of_ast piqdef_def T.parse_piqdef extended_piqdef_ast
    with (C.Error _) as e ->
      (* TODO, XXX: one error line is printed now, another (original) error
       * later -- it is inconsistent *)
      (
        prerr_endline (C.error_string piqdef context_str);
        (* re-raise the original exception after printing some context info *)
        raise e
      )
  in
  debug "apply_extensions(2)\n";
  Piqloc.trace := trace';

  (* get unparsed extension fields fields *)
  let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
  check_unknown_fields unknown_fields custom_fields
    ~prepend:(fun () -> C.warning piqdef context_str);

  extended_piqdef


(* expand extensions, i.e. extend exising definitions with extensions *)
let expand_extensions defs extensions custom_fields =
  let idtable = Idtable.empty in
  let idtable = add_piqdefs idtable defs in
  (* get a list of (piqdef, extensions) pairs from all extensiosn by resolving
   * extension names to piqdefs *)
  let l = flatmap (process_extension idtable) extensions in
  (* group the list of extensions by piqdef obtaining the list of
   * (piqdef, [extension]) pairs *)
  let groups = group_pairs l in
  let extended_defs =
    List.map (fun (piqdef, extensions) ->
      apply_extensions piqdef extensions custom_fields) groups
  in
  let involved_defs = List.map (fun (def, _) -> def) groups in
  let untouched_defs =
    List.filter (fun x -> not (List.memq x involved_defs)) defs
  in
  untouched_defs @ extended_defs


let get_imported_defs imports =
  let aux x = 
    let piqi = some_of x.Import#piqi in
    (* in order to avoid conflict between local defs and also defs imported
     * several times, creating a copy of defs *)
    let imported_defs = copy_defs piqi.P#resolved_piqdef in
    (* set parent namespace for imported definitions *)
    List.iter (fun def -> set_parent def (`import x)) imported_defs;
    imported_defs
  in
  flatmap aux imports


(* do include & extension expansion for the loaded piqi using extensions from
 * all included piqi modules *)
let rec process_piqi (piqi: T.piqi) =
  (* get unparsed fields before we load dependencies *)
  let unknown_fields = Piqobj_of_piq.get_unknown_fields () in

  (* load imports and includes *)
  load_dependecies piqi;

  (* get all unique included modules recursively; piqi will become the last
   * element of the list *)
  let modules = get_includes piqi in
  (* expand included_in to contain the list of all included modules including
   * the current one *)
  piqi.P#included_piqi <- modules;

  (* get all imports from this module and included modules *)
  let resolved_imports = get_imports modules in
  piqi.P#resolved_import <- resolved_imports;

  (* check for duplicates & looped imports including self-import loop *)
  check_imports piqi;

  let imported_defs = get_imported_defs resolved_imports in
  piqi.P#imported_piqdef <- imported_defs;

  (* fill idtable with their imported modules' definitions *)
  let idtable = Idtable.empty in
  let idtable = add_imported_piqdefs idtable imported_defs in

  (* boot defintions *)
  let boot_defs, boot_custom_fields =
    match !boot_piqi with
      | Some x when !Config.noboot = false ->
          (* NOTE: boot defs should be already extended *)
          x.P#extended_piqdef, x.P#custom_field
      | _ ->
          (* boot module is being processed right now, or --noboot command-line
           * option was specified *)
          [], []
  in

  (* get all definitions from all included modules and the current module *)
  let defs = get_piqdefs modules in

  (* get all extensions *)
  let extensions = get_extensions modules in

  (* NOTE: for local definitions we're considering custom fields defined only
   * in this module *)
  let custom_fields = piqi.P#custom_field @ boot_custom_fields in
  check_unknown_fields unknown_fields custom_fields;

  (* expand all extensions over all definitions *)
  (* NOTE, DOC: boot defs can not be extended *)
  let extended_defs =
    match extensions with
      | [] -> defs
      | _ ->
          (* defs should be correct before extending them *)
          (* XXX: can they become correct after extension while being
           * incorrect before? *)
          check_defs idtable (boot_defs @ defs);

          List.iter check_extension piqi.P#extend;

          (* NOTE: for extensions we're considering all custom fields from all
           * included modules *)
          let custom_fields = custom_fields @ (get_custom_fields modules) in
          expand_extensions defs extensions custom_fields
  in

  (* implicitly add defintions from the boot module to the current module *)
  (* XXX: can we avoid adding them, insead referring to them during type resolve
   * stage? *)
  let resolved_defs = boot_defs @ extended_defs in

  (* preserve the original defintions *)
  let resolved_defs = copy_defs resolved_defs in

  (* check defs, resolve defintion names to types, assign codes, resolve default
   * fields *)
  ignore (resolve_defs idtable resolved_defs);

  (* set up parent namespace to local piqi defs *)
  List.iter (fun def -> set_parent def (`piqi piqi)) resolved_defs;

  piqi.P#extended_piqdef <- extended_defs;
  piqi.P#resolved_piqdef <- resolved_defs;

  (* run registered processing hooks *)
  List.iter (fun f -> f piqi) !processing_hooks;
  ()
 

(* XXX: disallow include and import of the same module or produce a warning? *)
(* NOTE: using absolute paths by this point *)
and load_piqi_file ?modname fname =
  trace "file: %s\n" fname;
  trace_enter ();
  (*
  Piqloc.trace := true;
  *)
  let ast = read_piqi_file fname in
  let piqi = load_piqi_ast ?modname fname ast in
  trace_leave ();
  piqi


and load_piqi_channel ?modname fname ch =
  let ast = read_piqi_channel fname ch in
  load_piqi_ast ?modname fname ast


and load_piqi_string modname content =
  let fname = "[embedded] " ^ modname  in
  let ast = read_piqi_string fname content in
  load_piqi_ast ~modname "" ast


and load_piqi_ast ?modname fname (ast :T.ast) =
  let piqi = parse_piqi ast in

  (* save the original piqi *)
  let piqi = P#{piqi with original_piqi = Some piqi} in

  check_assign_module_name ?modname fname piqi;

  if modname <> None
  then Piqi_db.add_piqi (some_of modname) piqi;

  process_piqi piqi;
  piqi


and load_piqi_module modname =
  check_modname modname;
  try Piqi_db.find_piqi modname
  with Not_found ->
    let fname = find_piqi modname in
    let piqi = load_piqi_file fname ~modname in
    piqi


and load_dependecies (piqi:T.piqi) =

  let included_piqi = load_includes piqi.P#includ in
  piqi.P#included_piqi <- included_piqi;

  (* preserve the original imports *)
  let resolved_imports = copy_imports piqi.P#import in
  load_imports resolved_imports;
  piqi.P#resolved_import <- resolved_imports;
  ()


and load_imports l = List.iter load_import l

and load_import x =
  let open Import in (
  trace "import: %s\n" x.Import.modname;
  (* load imported module *)
  let piqi = load_piqi_module x.modname in
  (* save imported piqi in import.piqi field *)
  x.piqi <- Some piqi;
  assign_import_name x piqi;
  ())


and load_includes l = List.map load_include l

and load_include x =
  let open Includ in (
  trace "include: %s\n" x.Includ.modname;
  (* load included piqi module if it isn't already *)
  let piqi = load_piqi_module x.modname in
  piqi)


let load_piqi_module modname =
  trace "loading module: %s\n" modname;
  let piqi = load_piqi_module modname in
  piqi


let load_piqi_boot_module (modname, content) =
  trace "loading embedded module: %s\n" modname;
  trace_enter ();
  let piqi = load_piqi_string modname content in
  Piqi_db.add_piqi modname piqi;
  trace_leave ();
  piqi


let load_piqi_boot_modules () =
  let rec aux = function
    | [x] ->
        (* the last entry in the list is the boot module; return it after
         * processing *)
        load_piqi_boot_module x
    | h::t -> 
        ignore (load_piqi_boot_module h);
        aux t
    | _ ->
        assert false
  in
  aux !T.embedded_piqi


(* load standard include which is included by default in each .piqi file *)
let load_boot_piqi () =
  (* TODO: optimize loading boot piqi at cc stage, that is provide two
   * boot_piqi versions: 1) text 2) compiled -- so that we won't spend time
   * parsing it *)
  let boot_file = !Piqi_config.boot_file in
  if boot_file = ""
  then (* load boot piqi from embedded piqi file *)
    (* XXX: error handling *)
    begin
      trace "boot using embedded modules\n";
      trace_enter ();
      let piqi = load_piqi_boot_modules () in
      trace_leave ();
      piqi
    end
  else (* explicitly specified boot .piqi file *)
    (* TODO: error handling *)
    begin
      trace "boot using boot file: %s\n" boot_file;
      trace_enter ();
      let piqi = load_piqi_file boot_file in
      trace_leave ();
      piqi
    end


let init () =
  match !boot_piqi with
    | None ->
        let piqi = load_boot_piqi () in
        (* init boot module and piqi loader for Piqi_db *)
        boot_piqi := Some piqi;
        Piqi_db.piqi_loader := Some load_piqi_module;
    | Some _ ->
        () (* already initialized *)


(* public interface: read piqi file *)
let read_piqi fname :T.ast =
  let ch = Piqi_main.open_input fname in
  read_piqi_channel fname ch


(* public interface: load piqi file *)
let load_piqi fname :T.piqi =
  (* TODO: move init() to the boot stage, see
   * load_boot_piqi() for details *)
  init ();
  trace "loading piqi file: %s\n" fname;
  trace_enter ();
  let ast = read_piqi fname in
  let piqi = load_piqi_ast fname ast in
  trace_leave ();
  piqi


let convert_obj new_piqtype obj =
  debug "convert_obj(0)\n";
  trace_enter ();
  (* XXX *)
  Piqobj_of_piq.resolve_defaults := false;
  (* serialize to ast and read back as a differnt piqtype *)
  let ast = Piqobj_to_piq.gen_obj obj in

  (* XXX: setting this option in order to delay, and then ignore all parsing
   * warnings *)
  Piqobj_of_piq.delay_unknown_warnings := true;

  let res = Piqobj_of_piq.parse_obj new_piqtype ast in

  (* reset all warnings *)
  ignore (Piqobj_of_piq.get_unknown_fields ());

  trace_leave ();
  res


(* XXX: this can be implemented more efficiently using table-base precalculated
 * mapping *)
let convert_binobj piqtype new_piqtype binobj =
  debug "convert_binobj(0)\n";
  trace_enter ();
  let obj = Piqobj_of_wire.parse_binobj ~piqtype binobj in
  debug_loc "convert_binobj(1)";
  let new_obj = convert_obj new_piqtype obj in
  debug_loc "convert_binobj(2)";
  let res = Piqobj_to_wire.gen_binobj new_obj ~named:false in
  Piqloc.icount := !Piqloc.ocount;
  debug_loc "convert_binobj(3)";
  trace_leave ();
  res

