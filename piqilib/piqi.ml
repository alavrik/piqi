(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011 Anton Lavrik

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
type idtable = T.piqdef Idtable.t


(* start in boot_mode by default, it will be switched off later (see below) *)
let is_boot_mode = ref true
(* similarly, for initialization *)
let is_initialized = ref false

(* resolved type definition for the Piqi language;
 * it will be appropriately initialized during boot stage (see below) *)
let piqi_lang_def :T.piqtype ref = ref `bool
(* resolved type definition for the Piqi specification *)
let piqi_spec_def :T.piqtype ref = ref `bool

(* resolved "piqdef" type definition
 * Will be appropriately initialized during boot stage (see below) *)
let piqdef_def :T.piqtype ref = ref `bool
let field_def :T.piqtype ref = ref `bool
let option_def :T.piqtype ref = ref `bool
let function_def :T.piqtype ref = ref `bool
let import_def :T.piqtype ref = ref `bool


(* loaded in boot () -- see below *)
let piqi_spec :T.piqi option ref = ref None
let piqi_lang :T.piqi option ref = ref None


(* processing hooks to be run at the end of Piqi module load & processing *)
let processing_hooks = ref []

let register_processing_hook (f :idtable -> T.piqi -> unit) =
  debug "register_processing_hook(0)\n";
  (* NOTE: create an empty idtable just to make invocations below work; none of
   * the plugins actually require a valid idtable to exist at this point, so we
   * don't care *)
  let idtable = Idtable.empty in

  (* run the hook on the embedded Piqi self-specification *)
  if !piqi_boot <> None
  then f idtable (some_of !piqi_boot);

  debug "register_processing_hook(1.5)\n";
  f idtable (some_of !piqi_lang);

  debug "register_processing_hook(1.6)\n";
  f idtable (some_of !piqi_spec);

  debug "register_processing_hook(1)\n";
  (* add the hook to the list of registered hooks *)
  processing_hooks := !processing_hooks @ [f]


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


let is_func_param def =
  match def with
    | `record x -> x.R#is_func_param
    | `enum x | `variant x -> x.V#is_func_param
    | `alias x -> x.A#is_func_param
    | `list x -> x.L#is_func_param


(* mark type definition as a function parameter *)
let set_is_func_param_flag def =
  match def with
    | `record x -> x.R#is_func_param <- true
    | `enum x | `variant x -> x.V#is_func_param <- true
    | `alias x -> x.A#is_func_param <- true
    | `list x -> x.L#is_func_param <- true


let resolve_typeref map t =
  match t with
    | `name name ->
        let def = find_def map name in
        if is_func_param def
        then error name ("type " ^ quote name ^ " is defined as a function parameter and can't be referenced");
        (def: T.piqdef :> T.typeref)
    | _ ->
        t (* already resolved *)


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


let check_dup_names what names =
  match find_dups names with
    | None -> ()
    | Some (name, prev) ->
        error name
          ("duplicate " ^ what ^ " name " ^ quote name ^ "\n" ^
            error_string prev "first defined here")


let check_typeref obj (t:T.typeref) =
  match t with 
    | `name x -> check_scoped_name x
    | #T.piqdef ->
        error obj "use of type definition as type is prohibited"
    | _ -> ()


let error_noboot obj s =
  if !is_boot_mode || !Config.noboot
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
  let fields = r.R#field in
  (* XXX: Protobuf doesn't print any warnings on records with no fields *)
  (*
  if fields = []
  then warning r ("record " ^ quote r.R#name ^ " doesn't specify any fields");
  *)
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
        check_dup_names "field" names
    | `variant x | `enum x ->
        let names = List.map (fun x -> name_of_option x) x.V#option in
        check_dup_names "option" names
    | `alias x ->
        check_resolved_alias x
    | _ -> ()


(* scoped extensions names should have exactly two sections separated by '.' *)
let is_valid_scoped_extension_name name =
  if not (Piqi_name.is_valid_name name ~allow:".")
  then false
  else
    match Piq_parser.tokenize name '.' with
      | [a; b] -> Piqi_name.is_valid_name a && Piqi_name.is_valid_name b
      | _ -> false


let check_extension_name = function
  | `name name | `typedef name | `import name | `func name ->
      if not (Piqi_name.is_valid_name name)
      then error name "invalid extension name"

  | `field name | `option name ->
      if not (is_valid_scoped_extension_name name)
      then error name "invalid scoped extension name"


let check_extension_spec spec =
  check_extension_name spec;
  match spec with
    | `name name ->
        C.warning name "use of .name for extending typedefs is deprecated; use .typedef instead"
    | _ -> ()


let check_extension x =
  let open Extend in
  begin
    if x.what = [] && x.piqi_with = []
    then error x ("extension doesn't specify any names");

    if x.quote = [] && x.piqi_with = []
    then error x ("extension doesn't specify any extensions");

    if x.quote <> []
    then C.warning (List.hd x.quote) "this style of extensions is deprecated; always use .with prefix";

    List.iter check_extension_spec x.what
  end


let debug_loc prefix =
  debug "%s out count = %d, in count = %d\n" prefix !Piqloc.ocount !Piqloc.icount


let assert_loc () =
  if (!Piqloc.ocount <> !Piqloc.icount)
  then
    failwith
     (Printf.sprintf "internal_error: out count = %d, in count = %d\n" !Piqloc.ocount !Piqloc.icount)


(* convert textobj, i.e. JSON or XML to piqobj -- this function will be set by
 * either Piqi_json or Piqi_xml, depending on which format we are dealing with
 * at the moment *)
let piqobj_of_ref_init (piqtype: T.piqtype) (text: int) :Piqobj.obj =
  assert false

let piqobj_of_ref = ref piqobj_of_ref_init


let resolve_default_value default piqtype piqobj =
    assert_loc ();
    debug_loc "resolve_default_value(0)";
    let binobj = Piqobj_to_wire.gen_binobj piqobj in

    (* NOTE: fixing (preserving) location counters which get skewed during
     * parsing defaults *)
    Piqloc.icount := !Piqloc.ocount;
    debug_loc "resolve_default_value(1)";

    default.T.Any.binobj <- Some binobj;
    default.T.Any.typename <- Some (C.full_piqi_typename piqtype);
    ()


let resolve_field_default x =
  (* debug "resolve_field_default: %s\n" (C.name_of_field x); *)
  let open F in
  match x.default, x.typeref with
    | None, _ -> () (* no default *)
    | Some {T.Any.binobj = Some _}, _ ->
        (* nothing to do -- object is already resolved *)
        ()
    | Some ({T.Any.ast = Some ast} as default), Some typeref ->
        let piqtype = C.piqtype typeref in
        let piqobj = Piqobj_of_piq.parse_obj piqtype ast in
        resolve_default_value default piqtype piqobj

    | Some ({T.Any.ref = Some ref} as default), Some typeref ->
        let piqtype = C.piqtype typeref in
        let piqobj = !piqobj_of_ref piqtype ref in
        resolve_default_value default piqtype piqobj

    | _, None -> () (* there is no default for a flag *)
    | _ ->
        assert false (* either binobj or ast or textobj must be defined *)


let resolve_defaults = function
  | `record x ->
      List.iter resolve_field_default x.R#field
  | _ -> ()


let copy_obj (x:'a) :'a =
  Obj.obj (Obj.dup (Obj.repr x))
let copy_obj x = reference copy_obj x


let copy_obj_list l = List.map copy_obj l
let copy_obj_list l = reference copy_obj_list l


let copy_variant ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x V#{ x with option = copy_obj_list x.option }
  else copy_obj x


let copy_record ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x R#{ x with field = copy_obj_list x.field }
  else copy_obj x


let copy_def ~copy_parts (x:T.piqdef) =
  let res =
    match x with
      | `record x -> `record (copy_record ~copy_parts x)
      | `variant x -> `variant (copy_variant ~copy_parts x)
      | `enum x -> `enum (copy_variant ~copy_parts x)
      | `alias x -> `alias (copy_obj x)
      | `list x -> `list (copy_obj x)
  in
  (* preserve location information *)
  Piqloc.addrefret x res


let copy_defs ?(copy_parts=true) defs = List.map (copy_def ~copy_parts) defs


let copy_imports l = List.map copy_obj l


let resolve_defs ?piqi idtable (defs:T.piqdef list) =
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

  (* assign wire codes, if they are unassigned; check otherwise; check
   * correctness of .wire-packed usage *)
  Piqi_wire.process_defs defs;

  (* set up parent namespace to local piqi defs *)
  (match piqi with
    | Some piqi ->
        List.iter (fun def -> set_parent def (`piqi piqi)) defs;
    | None -> ()
  );

  (* return updated idtable *)
  idtable


let check_defs idtable defs =
  ignore (resolve_defs idtable (copy_defs defs))


let read_piqi_common fname piq_parser :T.ast =
  (* don't expand abbreviations until we construct the containing object *)
  let res = Piq_parser.read_all piq_parser ~expand_abbr:false in

  if res = []
  then piqi_warning ("piqi file is empty: " ^ fname);

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
  try Pervasives.open_in_bin fname
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
        piqi.modname <- modname
    | None, None ->
        (* basename + chop .piqi and .proto.piqi extensions + underscores to
         * dashes *)
        let basename = Piqi_file.basename fname in
        let name = Piqi_name.make_local_name basename in
        if Piqi_name.is_valid_modname name
        then piqi.modname <- Some name
        else error piqi "piqi module name can not be derived from the file name"


let assign_import_name x =
  let open Import in
  match x.name with
    | Some x -> (* import name is already defined *)
        check_name x
    | None ->
        (* derive import name from the original module's name *)
        let name = Piqi_name.get_local_name x.modname in
        x.name <- Some name


let name_of_import x =
  let open Import in
  match x.name with
    | Some x -> x (* import name is already defined *)
    | None ->
        (* derive import name from the original module's name *)
        Piqi_name.get_local_name x.modname


let mlobj_to_piqobj piqtype wire_generator mlobj =
  debug_loc "mlobj_to_piqobj(0)";
  assert_loc ();
  let binobj = Piqirun.gen_binobj wire_generator mlobj in
  debug_loc "mlobj_to_piqobj(1.5)";

  (* dont' resolve defaults when reading wire *)
  let piqobj =
    C.with_resolve_defaults false (Piqobj_of_wire.parse_binobj piqtype) binobj
  in
  debug_loc "mlobj_to_piqobj(1)";
  assert_loc ();

  piqobj


let mlobj_to_ast piqtype wire_generator mlobj =
  debug_loc "mlobj_to_ast(0)";
  let piqobj = mlobj_to_piqobj piqtype wire_generator mlobj in
  debug_loc "mlobj_to_ast(1.5)";
  let ast = Piqobj_to_piq.gen_obj piqobj in
  debug_loc "mlobj_to_ast(1)";
  assert_loc ();
  ast


let mlobj_of_piqobj wire_parser piqobj =
  let binobj = Piqobj_to_wire.gen_binobj piqobj in
  let mlobj = Piqirun.parse_binobj wire_parser binobj in
  mlobj


let mlobj_of_ast piqtype wire_parser ast =
  debug_loc "mlobj_of_ast(0)";

  (*
  (* initialize starting location code *)
  let max_count = max !T.icount !T.ocount in
  T.icount := max_count;
  T.ocount := max_count;
  *)

  (* XXX: find a better way to set this option *)
  Piqobj_of_piq.delay_unknown_warnings := true;

  (* We have to resolve defaults while reading piqi in order to provide correct
   * location bindings. It is not possible to "fix" skewed location bindings
   * in piqtype.ml after default values get parsed. We rather decided to fix
   * location bindings here -- see resolve_defaults function for details *)
  let piqobj =
    C.with_resolve_defaults true (Piqobj_of_piq.parse_obj piqtype) ast
  in
  debug_loc "mlobj_of_ast(1.5)";

  Piqobj_of_piq.delay_unknown_warnings := false;

  let mlobj = mlobj_of_piqobj wire_parser piqobj in
  debug_loc "mlobj_of_ast(1)";
  assert_loc ();

  mlobj


let parse_piqi ast =
  (* XXX: handle errors *)
  debug "parse_piqi(0)\n";
  (* use prepared static "piqi" definition to parse the ast *)
  let res = mlobj_of_ast !piqi_lang_def T.parse_piqi ast in
  debug "parse_piqi(1)\n";
  res


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


(* From (key, value) list extract values for the specified key, and return the
 * list of extracted values and the remaining tuples; the order of both values
 * and remaining items is preserved *)
(* NOTE: not tail recursive *)
let takeout key l =
  let rec aux values rem = function
    | [] -> List.rev values, List.rev rem
    | (key', value)::t when key' = key ->
        aux (value::values) rem t
    | h::t ->
        aux values (h::rem) t
  in aux [] [] l
    

(* group unsorted (key, [value]) pairs by their first element (key) and return
 * the list of groups containing (key, [values]) for each group *)
let group_pairs l =
  let rec aux groups = function
    | [] -> List.rev groups
    | (key, value)::t ->
        let values, rem = takeout key t in
        let flatten_values = List.concat (value :: values) in
        aux ((key, flatten_values)::groups) rem
  in aux [] l


let parse_scoped_name name =
  match Piq_parser.tokenize name '.' with
    | [def_name; nested_name] -> def_name, nested_name
    | _ -> assert false (* this has been checked already *)


(* replace the first list element for which [f] returns true with [x]
 *
 * NOTE: non-tail recursive
 *)
let list_replace l f x =
  let rec aux = function
    | [] ->
        (* we were supposed to replace an item before we reached the end of the
         * list *)
        assert false
    | h::t ->
        if f h
        then x::t
        else h::(aux t)
  in
  aux l


let idtable_of_defs defs =
  let idtable = Idtable.empty in
  add_piqdefs idtable defs


let idtable_of_imports imports =
  List.fold_left
    (fun t x -> Idtable.add t (name_of_import x) x)
    Idtable.empty imports


let idtable_of_functions funcs =
  List.fold_left
    (fun t x -> Idtable.add t x.T.Func#name x)
    Idtable.empty funcs


let list_of_idtable idtable =
  Idtable.fold (fun k v l -> v::l) [] idtable


(* find record field by name *)
let find_field r field_name scoped_name =
  try
    List.find (fun x -> name_of_field x = field_name) r.R#field
  with Not_found ->
    error scoped_name ("record doesn't have field named " ^ quote field_name)


(* find variant option by name *)
let find_option v option_name scoped_name =
  try
    List.find (fun x -> name_of_option x = option_name) v.V#option
  with Not_found ->
    error scoped_name ("variant doesn't have option named " ^ quote option_name)


(* replace record field with the new one *)
let replace_field r f field_name =
  let fields = r.R#field in
  let new_fields = list_replace fields (fun x -> name_of_field x = field_name) f in
  Piqloc.addref fields new_fields;
  let new_record = R#{r with field = new_fields} in
  Piqloc.addref r new_record;
  new_record


(* replace variant option with the new one *)
let replace_option v o option_name =
  let options = v.V#option in
  let new_options = list_replace options (fun x -> name_of_option x = option_name) o in
  Piqloc.addref options new_options;
  let new_variant = V#{v with option = new_options} in
  Piqloc.addref v new_variant;
  new_variant


let apply_extensions obj obj_def obj_parse_f obj_gen_f extension_entries custom_fields =
  let trace' = !Piqloc.trace in
  (* Piqloc.trace := false; *)
  debug "apply_extensions(0)\n";
  let obj_ast = mlobj_to_ast obj_def obj_gen_f obj in
  let extension_asts = List.map (fun x -> some_of x.Any#ast) extension_entries in
  let extended_obj_ast =
    match obj_ast with
      | `named ({T.Named.value = ((`list l) as _ref)} as x) -> (* typedefs -- named containers *)
          let v = `list (l @ extension_asts) in

          let v = Piq_parser.piq_addrefret _ref v in

          let res = `named {x with T.Named.value = v} in

          ignore (Piq_parser.piq_addrefret x res);

          res

      | (`list l) as _ref -> (* fields and options -- plain containers *)
          let v = `list (l @ extension_asts) in
          Piq_parser.piq_addrefret _ref v

      | _ ->
          (* extensions can only be applied to named containers and all of
           * piqdefs are named containers *)
          assert false
  in
  let context_str = "while applying extensions to this definition ..." in
  debug "apply_extensions(1)\n";
  let extended_obj =
    try
      mlobj_of_ast obj_def obj_parse_f extended_obj_ast
    with (C.Error _) as e ->
      (* TODO, XXX: one error line is printed now, another (original) error
       * later -- it is inconsistent *)
      (
        prerr_endline (C.error_string obj context_str);
        (* re-raise the original exception after printing some context info *)
        raise e
      )
  in
  debug "apply_extensions(2)\n";
  Piqloc.trace := trace';

  (* get unparsed extension fields fields *)
  let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
  check_unknown_fields unknown_fields custom_fields
    ~prepend:(fun () -> C.warning obj context_str);

  extended_obj


let apply_option_extensions idtable scoped_name extension_entries custom_fields =
  let def_name, option_name = parse_scoped_name scoped_name in
  match find_def idtable def_name with
    | `variant v ->
        let option = find_option v option_name scoped_name in
        let extended_option =
          apply_extensions option !option_def T.parse_option T.gen__option
          extension_entries custom_fields
        in
        let extended_variant = replace_option v extended_option option_name in
        let extended_typedef = `variant extended_variant in
        Piqloc.addref extended_variant extended_typedef;
        (* replace the original variant with the extended one *)
        Idtable.add idtable def_name extended_typedef
    | _ ->
        error scoped_name
          ("can't apply option extensions no non-variant definition " ^ quote def_name)


let apply_field_extensions idtable scoped_name extension_entries custom_fields =
  let def_name, field_name = parse_scoped_name scoped_name in
  match find_def idtable def_name with
    | `record r ->
        let field = find_field r field_name scoped_name in
        let extended_field =
          apply_extensions field !field_def T.parse_field T.gen__field
          extension_entries custom_fields
        in
        let extended_record = replace_field r extended_field field_name in
        let extended_typedef = `record extended_record in
        Piqloc.addref extended_record extended_typedef;
        (* replace the original record with the extended one *)
        Idtable.add idtable def_name extended_typedef
    | _ ->
        error scoped_name
          ("can't apply field extensions no non-record definition " ^ quote def_name)


let extend_import idtable name extension_entries custom_fields =
  let import = 
    try Idtable.find idtable name
    with Not_found -> error name ("unknown import " ^ quote name)
  in
  let extended_import =
    apply_extensions import !import_def T.parse_import T.gen__import
    extension_entries custom_fields
  in
  (* replace the original import with the extended one *)
  Idtable.add idtable name extended_import


let extend_imports imports extensions custom_fields =
  (* group the list of extensions by definition obtaining the list of
   * (spec, [extension]) pairs *)
  let extensions_groups = group_pairs extensions in
  (* apply extensions to each import *)
  let idtable = idtable_of_imports imports in
  let idtable =
    List.fold_left
      (fun idtable (spec, extension_items) ->
        let name =
          match spec with
            | `import name -> name
            | _ -> assert false (* typedef and function extensions are already filtered out *)
        in
        extend_import idtable name extension_items custom_fields
      )
      idtable
      extensions_groups
  in
  (* convert the updated idtable to the list of resulting imports *)
  list_of_idtable idtable


let extend_function idtable name extension_entries custom_fields =
  let func =
    try Idtable.find idtable name
    with Not_found -> error name ("unknown function " ^ quote name)
  in
  let extended_func =
    apply_extensions func !function_def T.parse_func T.gen__func
    extension_entries custom_fields
  in
  (* replace the original function with the extended one *)
  Idtable.add idtable name extended_func


let extend_functions funs extensions custom_fields =
  (* group the list of extensions by definition obtaining the list of
   * (spec, [extension]) pairs *)
  let extensions_groups = group_pairs extensions in
  (* apply extensions to each function *)
  let idtable = idtable_of_functions funs in
  let idtable =
    List.fold_left
      (fun idtable (spec, extension_items) ->
        let name =
          match spec with
            | `func name -> name
            | _ -> assert false (* typedef and import extensions are already filtered out *)
        in
        extend_function idtable name extension_items custom_fields
      )
      idtable
      extensions_groups
  in
  (* convert the updated idtable to the list of resulting functions *)
  list_of_idtable idtable


(* apply field and option extensions *)
let apply_def_extensions idtable spec extension_entries custom_fields =
  match spec with
    | `name name | `typedef name ->
        let piqdef = find_def idtable name in
        let extended_piqdef =
          apply_extensions piqdef !piqdef_def T.parse_piqdef T.gen__piqdef
          extension_entries custom_fields
        in
        (* replace the original typedef with the extended one *)
        Idtable.add idtable name extended_piqdef
    | `field x ->
        apply_field_extensions idtable x extension_entries custom_fields
    | `option x ->
        apply_option_extensions idtable x extension_entries custom_fields
    | _ ->
        assert false (* import and function extensions are already filtered out *)


(* apply extensions to type defininitions *)
let apply_defs_extensions defs extensions custom_fields =
  (* group the list of extensions by definition obtaining the list of
   * (spec, [extension]) pairs *)
  let extensions_groups = group_pairs extensions in

  (* defs extensions must be applied before field and option extensions;
   * therefore partition partition extensions into typedef, and the other types
   * of extensions; and then append them back together *)
  let defs_extension_groups, other_extension_groups =
    List.partition (fun (spec, _) ->
      match spec with
        | `name _ | `typedef _ -> true (* these are the same, but `name is depreceated *)
        | _ -> false
    ) extensions_groups
  in
  let extensions_groups = defs_extension_groups @ other_extension_groups in

  (* create a new idtable from the list of definitions *)
  let idtable = idtable_of_defs defs in

  (* iterate through groups and apply extensions to correspondent definitions *)
  let idtable =
    List.fold_left
      (fun idtable (spec, extension_items) ->
        apply_def_extensions idtable spec extension_items custom_fields
      )
      idtable extensions_groups
  in
  (* convert the updated idtable to the list of resulting defs *)
  list_of_idtable idtable


(* partition extensions into typedef extesions, function extensions and import
 * extensions *)
let partition_extensions extensions =
  let open Extend in
  (* get a list of (what, [extension]) pairs from all extensions *)
  let l = flatmap
    (fun x -> List.map (fun what -> what, (x.piqi_with @ x.quote)) x.what)
    extensions
  in
  let d, f, i =
    List.fold_left
      (fun (d, f, i) ((spec, _) as x) ->
        match spec with
          | `func _ -> (d, x::f, i)
          | `import _ -> (d, f, x::i)
          | `typedef _ | `name _ | `field _ | `option _ -> (x::d, f, i))
      ([], [], []) l
  in
  (List.rev d, List.rev f, List.rev i)


let get_imported_defs imports =
  let aux x = 
    let piqi = some_of x.Import#piqi in
    (* in order to avoid conflict between local defs and also defs imported
     * several times, creating a shallow copy of imported defs just to be able
     * to safely mutate the "parent" field *)
    let imported_defs = copy_defs piqi.P#resolved_piqdef ~copy_parts:false in
    (* set parent namespace for imported definitions *)
    List.iter (fun def -> set_parent def (`import x)) imported_defs;
    imported_defs
  in
  flatmap aux imports


let make_param_name func param_name =
  (* construct the type name as a concatentation of the function name and
   * -input|output|error *)
  let func_name = func.T.Func#name in
  let type_name = func_name ^ "-" ^ param_name in
  (* create a location reference for the newly constructed type name *)
  Piqloc.addrefret func_name type_name


let make_param_alias name x =
  let res =
    A#{
      T.default_alias () with

      name = name;
      typeref = `name x;
    }
  in
  Piqloc.addrefret x res


let make_param_record name x =
  let res =
    R#{
      T.default_record () with

      name = name;
      field = x.T.Anonymous_record.field;
    }
  in
  let res = copy_record res in (* preserve the original fields *)
  Piqloc.addrefret x res


let make_param_variant name x =
  let res =
    V#{
      T.default_variant () with

      name = name;
      option = x.T.Anonymous_variant.option;
    }
  in
  let res = copy_variant res in (* preserve the original options *)
  Piqloc.addrefret x res


let make_param_list name x =
  let res =
    L#{
      T.default_piqlist () with

      name = name;
      typeref = x.T.Anonymous_list.typeref;
      (* XXX: what about wire-packed property? -- it is not defined for
       * anonymous list:
       * wire_packed = x.T.Anonymous_list.wire_packed;
       *)
    }
  in
  Piqloc.addrefret x res


(* convert function parameter to a type:
 *  - if the function parameter is a name, convert it to correspondent alias
 *  - if the function parameter is an anonymous record, convert it to
 *    correspondent record
 *  - do the same for anonymous variants, enums and lists
 *)
let resolve_param func param_name param =
  let type_name = make_param_name func param_name in
  let def =
    match param with
      | `name x ->
          (* make an alias from name reference *)
          `alias (make_param_alias type_name x)
      | `record x ->
          `record (make_param_record type_name x)
      | `variant x ->
          `variant (make_param_variant type_name x)
      | `enum x ->
          `enum (make_param_variant type_name x)
      | `list x ->
          `list (make_param_list type_name x)
  in
  Piqloc.addref param def;
  def


(* return func, [(def, (def_name, set_f))], where
 *
 * [def] is a definition derived from the function's input/output/erorr
 * parameter
 *
 * [def_name] is the definition's name
 *
 * [set_f] is a setter which takes a type definition and assigns it to the
 * correspondnt function's parameter
 *)
let process_func f =
  let open T.Func in
  let set_input def = set_is_func_param_flag def; f.resolved_input <- Some def
  and set_output def = set_is_func_param_flag def; f.resolved_output <- Some def
  and set_error def = set_is_func_param_flag def; f.resolved_error <- Some def
  in
  let process_param param_name param set_f =
    match param with
      | None -> []
      | Some param ->
          let def = resolve_param f param_name param in
          let res = (def, (piqdef_name def, set_f)) in
          [res]
  in
  let input = process_param "input" f.input set_input
  and output = process_param "output" f.output set_output
  and error = process_param "error" f.error set_error
  in
  (input @ output @ error)


let get_function_defs resolved_funs =
  (* get definitions derived from function parameters *)
  let defs_pairs = List.map process_func resolved_funs in

  let defs_pairs = List.concat defs_pairs in
  let defs, name_setter_assoc_l = List.split defs_pairs in

  (* prepare the setters map *)
  let setter_map = List.fold_left
    (fun t (name, setter) -> Idtable.add t name setter)
    Idtable.empty
    name_setter_assoc_l
  in
  (* returned definitions derived from function parameters and a map that will
   * be used to set definitions, once resolved, to their appropirate slots in
   * resolved functions *)
  defs, setter_map


(* return a list under top-level list element; remove modname and include
 * directives *)
let prepare_included_piqi_ast ast =
  match ast with
    | `list l ->
        List.filter
          (function
            | `named {T.Named.name = "module"} -> false
            | `named {T.Named.name = "include"} -> false
            | _ -> true
          )
          l
    | _ ->
        assert false


let expand_includes piqi included_piqi =
  (* get the list of included modules' ASTs *)
  let included_asts =
    flatmap (fun x -> prepare_included_piqi_ast (some_of x.P#ast)) included_piqi
  in
  (* transform the module's ast to include all elements from all included
   * modules *)
  let ast = some_of piqi.P#ast in
  let new_ast =
    match ast with
      | `list l ->
          let res = `list (included_asts @ l) in
          Piqloc.addrefret ast res
      | _ ->
          assert false
  in
  let res_piqi = parse_piqi new_ast in

  (* discard unknown fields -- they have been processed and reported separately
   * for each individual module *)
  ignore (Piqobj_of_piq.get_unknown_fields ());

  res_piqi.P#ast <- Some ast;
  res_piqi


let is_extension modname =
  String.contains (Piqi_name.get_local_name modname) '.'


(* find all applicable extensions for a given module *)
let find_extensions modname =
  let find_extension ext_name =
    let modname = modname ^ "." ^ ext_name in
    try
      ignore (Piqi_file.find_piqi modname);
      trace "found extension: %s\n" modname;
      [modname]
    with
      Not_found -> []
  in
  if is_extension modname
  then [] (* extensions are not appliable to extensions *)
  else flatmap find_extension !Config.extensions


(* do include & extension expansion for the loaded piqi using extensions from
 * all included piqi modules *)
let rec process_piqi ?modname ?(include_path=[]) ?(fname="") ?(ast: T.ast option) ~cache (orig_piqi: T.piqi) =

  (* report unparsed fields before we load dependencies (this is meaningless if
   * Piqi is not parsed from Piq)
   *)
  if ast <> None
  then (
    let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
    let custom_fields = orig_piqi.P#custom_field in
    check_unknown_fields unknown_fields custom_fields;
  );

  (* preserve the original piqi by creating a shallow copy of it *)
  let piqi = copy_obj orig_piqi in
  piqi.P#original_piqi <- Some orig_piqi;

  (* it is critical to cache loaded piqi module before we process any of its
   * dependencies; by doing this, we check for circular imports *)
  check_assign_module_name ?modname fname piqi;
  if cache then Piqi_db.add_piqi piqi;
  piqi.P#ast <- ast;

  (*
   * handle includes
   *)
  let extension_includes =
    if ast = None || not !is_initialized
    then
      (* extensions are not appliable for non-Piq representation and embeeded
       * modules loaded during initialization *)
      []
    else
      List.map (fun x -> Includ#{modname = x})
      (find_extensions (some_of piqi.P#modname))
  in
  let includes = piqi.P#includ @ extension_includes in
  let included_piqi = load_includes piqi includes  ~include_path in
  let piqi =
    if included_piqi = []
    then piqi
    else (
      let extended_piqi = expand_includes piqi included_piqi in

      extended_piqi.P#original_piqi <- Some orig_piqi;
      extended_piqi.P#modname <- piqi.P#modname;
      (* replace previously cached piqi module *)
      if cache then Piqi_db.replace_piqi extended_piqi;
      extended_piqi
    )
  in

  (* append the original (input) piqi module to the list of included piqi
   * modules *)
  let modules = included_piqi @ [piqi] in
  piqi.P#included_piqi <- modules;

  (*
   * get all extensions
   *)
  List.iter check_extension orig_piqi.P#extend;
  let defs_extensions, func_extensions, import_extensions =
    partition_extensions piqi.P#extend
  in

  (* NOTE: for extensions we're considering all custom fields from all
   * included modules *)
  let custom_fields = piqi.P#custom_field in

  (*
   * handle imports
   *)
  (* get all imports from included modules *)
  let imports = piqi.P#import in
  let extended_imports =
    if import_extensions = []
    then imports
    else extend_imports imports import_extensions custom_fields
  in
  (* preserve the original imports *)
  let resolved_imports = copy_imports extended_imports in
  load_imports piqi resolved_imports;
  piqi.P#resolved_import <- resolved_imports;
  piqi.P#extended_import <- extended_imports;

  (*
   * handle imported defs
   *)
  let imported_defs = get_imported_defs resolved_imports in
  piqi.P#imported_piqdef <- imported_defs;

  (* fill idtable with their imported modules' definitions *)
  let idtable = Idtable.empty in
  let idtable = add_imported_piqdefs idtable imported_defs in

  (* add defintions from the boot module to the idtable *)
  let boot_defs =
    match !piqi_boot with
      | Some x when !Config.noboot = false ->
          (* NOTE: boot defs should be already extended *)
          x.P#resolved_piqdef
      | _ ->
          (* boot module is being processed right now, or --noboot command-line
           * option was specified *)
          []
  in
  let idtable = add_piqdefs idtable boot_defs in

  (*
   * handle functions
   *)
  (* get all functions from this module and included modules *)
  let funs = piqi.P#func in

  (* check for duplicate function names *)
  let func_names = List.map (fun x -> x.T.Func#name) funs in
  List.iter check_name func_names;
  check_dup_names "function" func_names;

  let extended_funs =
    if func_extensions = []
    then funs
    else extend_functions funs func_extensions custom_fields
  in
  (* preserve the original functions *)
  let resolved_funs = List.map copy_obj extended_funs in
  piqi.P#resolved_func <- resolved_funs;
  piqi.P#extended_func <- extended_funs;

  (* get definitions derived from function parameters *)
  let func_defs, func_defs_map = get_function_defs resolved_funs in

  (* add function type definitions to Piqi defs *)
  let defs = piqi.P#piqdef @ func_defs in

  (* expand all extensions over all definitions *)
  (* NOTE: boot defs can not be extended *)
  let extended_defs =
    if defs_extensions = []
    then defs
    else (
        (* defs should be correct before extending them *)
        (* XXX: can they become correct after extension while being
         * incorrect before? *)
        check_defs idtable defs;
        apply_defs_extensions defs defs_extensions custom_fields
    )
  in
  (* preserve the original defintions by making a copy *)
  let resolved_defs = copy_defs extended_defs in

  (* set resolved_func.resolved_input/output/error fields to their correspondent
   * defs that were derived from function parameters *)
  List.iter
    (fun def ->
      try
        let setter = Idtable.find func_defs_map (piqdef_name def) in
        setter def
      with
        Not_found -> ()
    )
    resolved_defs;

  (* remove function defs from the list of extended piqdefs *)
  let extended_defs = List.filter
    (fun def -> not (Idtable.mem func_defs_map (piqdef_name def)))
    extended_defs
  in

  (* if the module includes (or is itself) piqi.org/piqi, use hash-based field
   * and option codes instead of auto-enumerated ones
   *
   * NOTE: code assignment is needed only for .piqi, Piqi specifications encoded
   * in other formats must already include explicitly speficied (and correct!)
   * wire codes.
   *
   * NOTE: this step must be performed before resolving defaults -- this is
   * critical for potential piqi extensions such as those used in various piqic
   *)
  if C.is_self_spec piqi then Piqi_wire.add_hashcodes resolved_defs;

  (* check defs, resolve defintion names to types, assign codes, resolve default
   * fields *)
  let idtable = resolve_defs ~piqi idtable resolved_defs in

  piqi.P#extended_piqdef <- extended_defs;
  piqi.P#resolved_piqdef <- resolved_defs;

  (* run registered processing hooks *)
  List.iter (fun f -> f idtable piqi) !processing_hooks;

  (* resolve defaults ANY to OBJ using field types and codes; we need to do it
   * after executing hooks, because otherwise json names will be unresolved and
   * default field resolution will fail *)
  List.iter resolve_defaults resolved_defs;

  piqi
 

(* XXX: disallow include and import of the same module or produce a warning? *)
(* NOTE: using absolute paths by this point *)
and load_piqi_file ?modname ?include_path fname =
  trace "file: %s\n" fname;
  trace_enter ();
  (*
  Piqloc.trace := true;
  *)
  let ast = read_piqi_file fname in
  (* cache only those modules that were included/imported/referenced by their
   * modname *)
  let cache = modname <> None in
  let piqi = load_piqi_ast ?modname ?include_path ~cache fname ast in
  trace_leave ();
  piqi


(* This function is not used at the time
and load_piqi_channel ?modname fname ch =
  let ast = read_piqi_channel fname ch in
  load_piqi_ast ?modname fname ast
*)


and load_piqi_string fname content =
  let ast = read_piqi_string fname content in
  load_piqi_ast fname ast ~cache:false


and load_piqi_ast ?modname ?(include_path=[]) ~cache fname (ast :T.ast) =
  let piqi = parse_piqi ast in
  process_piqi piqi ?modname ~include_path ~cache ~fname ~ast


and load_piqi_module ?(include_path=[]) modname =
  check_modname modname;
  (* check if the module is already loaded *)
  try Piqi_db.find_piqi modname
  with Not_found ->
    let fname =
      try
        Piqi_file.find_piqi modname
      with
        Not_found ->
          error modname ("piqi module is not found: " ^ quote modname)
    in
    let piqi = load_piqi_file fname ~modname ~include_path in
    piqi


and load_imports piqi l =
  List.iter load_import l;
  (* check for duplicates & looped imports including self-import loop *)
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
  check_dups l;

  (* simple check for import loops; provides very limited diagnostic *)
  (* NOTE: import loops disallowed in Protobuf as well *)
  if List.exists (fun x -> some_of x.Import#piqi == piqi) l
  then error piqi "imported piqi modules form a loop"


and load_import x =
  let open Import in (
    trace "import: %s\n" x.Import.modname;
    (* load imported module *)
    let piqi = load_piqi_module x.modname in
    (* save imported piqi in import.piqi field *)
    x.piqi <- Some piqi;
    assign_import_name x;
  )


and load_includes ~include_path piqi l =
  List.iter (fun x ->
    if x.Includ#modname = some_of piqi.P#modname
    then error x "piqi module includes itself"
  ) l;

  (* if there's any included extension that's also a parent, remove all included
   * extensions from the list of includes *)
  let remove_extensions l include_path =
    if List.exists (fun x ->
        let n = x.Includ#modname in
        is_extension n &&
        List.exists (fun x -> n = some_of x.P#modname) include_path
      ) l
    then
      List.filter (fun x ->
        let n = x.Includ#modname in
        let keep = not (is_extension x.Includ#modname) in
        if not keep
        then trace "removing extension include %s\n" (quote n);
        keep
      ) l
    else l
  in

  let new_include_path = piqi :: include_path in

  let l = remove_extensions l include_path in
  let included_piqi = List.map (load_include new_include_path) l in

  let process_recursive_piqi p =
    trace "included piqi module %s forms a loop\n" (quote (some_of p.P#modname));
    let includes = remove_extensions p.P#includ new_include_path in
    (* check for all Piqi includes that have been already processed in the DFS
     * include path *)
    List.iter
      (fun x ->
        let n = x.Includ#modname in
        if List.exists (fun p -> n = some_of p.P#modname) new_include_path
        then error x ("recursive include " ^ quote n)
      )
      includes;
    (* process the remaining includes as if they were included by the current
     * module *)
    trace_enter ();
    let res = load_includes ~include_path piqi includes in
    trace_leave ();
    res
  in

  (* append all Piqi modules from all included Piqi modules *)
  let l = flatmap
    (fun x ->
      let res = x.P#included_piqi in
      if res = [] (* the module is loaded, but hasn't been processed yet *)
      then
        (* process recursive module's non-recursive includes
         *
         * NOTE: we need the module's ast anyway, so appending it to the
         * results; if it happends to be this same module, it will be filtered
         * out below
         *)
        (process_recursive_piqi x) @ [ x ]
      else
        res
    )
    included_piqi
  in

  (* remove duplicates -- one module may be included from many modules *)
  let res = uniqq l in

  (* finally, remove itself from the list of included modules; it could happen
   * if there is a recursion *)
  List.filter (fun x -> x != piqi) res


and load_include include_path x =
  let open Includ in (
    trace "include: %s\n" x.Includ.modname;
    (* load included piqi module if it isn't already *)
    let piqi = load_piqi_module x.modname ~include_path in
    piqi
  )


let piqi_loader ?modname fname =
  load_piqi_file ?modname fname


let embedded_modname = "embedded/piqi.org/piqi-lang"


let find_embedded_piqtype name =
  Piqi_db.find_piqtype (embedded_modname ^ "/" ^ name)


(*
 * booting code; preparing the initial statically defined piqi defintions
 *)
let boot () =
  trace "boot(0)\n";
  (* process embedded Piqi self-specification *)
  (* don't cache them as we are adding the spec to the DB explicitly below *)

  let boot = process_piqi T.piqi_boot ~cache:false in
  piqi_boot := Some boot;

  let lang = process_piqi T.piqi_lang ~cache:false in
  piqi_lang := Some lang;

  let spec = process_piqi T.piqi_spec ~cache:false in
  piqi_spec := Some spec;

  (* add the boot spec to the DB under a special name *)
  boot.P#modname <- Some "embedded/piqi.org/piqi-boot";
  Piqi_db.add_piqi boot;

  (* add the self-spec to the DB under a special name *)
  spec.P#modname <- Some "embedded/piqi.org/piqi";
  Piqi_db.add_piqi spec;

  (* add the self-spec to the DB under a special name *)
  lang.P#modname <- Some embedded_modname;
  Piqi_db.add_piqi lang;

  (* resolved type definition for the Piqi language *)
  piqi_lang_def := find_embedded_piqtype "piqi";
  (* resolved type definition for the Piqi specification *)
  piqi_spec_def := Piqi_db.find_piqtype "embedded/piqi.org/piqi/piqi";
  (* resolved "piqdef" type definition *)
  piqdef_def := find_embedded_piqtype "piqdef";
  field_def := find_embedded_piqtype "field";
  option_def := find_embedded_piqtype "option";
  function_def := find_embedded_piqtype "function";
  import_def := find_embedded_piqtype "import";

  (* turn boot mode off *)
  is_boot_mode := false;

  (* resume object location tracking -- it is paused from the beginning *)
  Piqloc.resume ();

  (* initialize Piqi loader; doing it this way, because Piqi and Piqi_db are
   * mutually recursive modules *)
  Piqi_db.piqi_loader := Some piqi_loader;

  trace "boot(1)\n";
  ()


let _ =
  if !Sys.interactive
  then () (* don't do anything in interactive (toplevel) mode *)
  else (
    (*
    Piqi_config.debug_level := 1;
    *)
    boot ();
  )


let load_embedded_boot_module (modname, content) =
  trace "loading embedded module: %s\n" modname;
  trace_enter ();
  let fname = "embedded/" ^ modname in
  let piqi = load_piqi_string fname content in
  piqi.P#modname <- Some modname; (* fix the modname *)
  Piqi_db.add_piqi piqi;
  trace_leave ();
  piqi


let load_embedded_boot_modules () =
  let rec aux = function
    | [x] ->
        (* the last entry in the list is the boot module; return it after
         * processing *)
        load_embedded_boot_module x
    | h::t -> 
        ignore (load_embedded_boot_module h);
        aux t
    | _ ->
        assert false
  in
  let piqi_boot = aux !T.embedded_piqi in
  (* make loaded modules unsearcheable after loading all of them *)
  List.iter (fun (modname, _) -> Piqi_db.remove_piqi modname) !T.embedded_piqi;
  piqi_boot


(* Overriding already loaded boot_piqi with exactly the same boot module but now
 * it has correct location info because it is parsed from string representation
 *)
let load_embedded_boot_piqi () =
  if!T.embedded_piqi <> []
  then
    piqi_boot :=
      (
        (* reset previous boot module *)
        piqi_boot := None;
        trace "boot using embedded modules\n";
        trace_enter ();
        (* XXX: error handling *)
        let piqi = load_embedded_boot_modules () in
        trace_leave ();
        Some piqi
      )


(* used only by piqicc to load a custom Piqi boot module from a file *)
let load_boot_piqi boot_file  =
  piqi_boot :=
    (
      (* reset previous boot module *)
      piqi_boot := None;
      trace "boot using boot file: %s\n" boot_file;
      trace_enter ();
      (* TODO: error handling *)
      let piqi = load_piqi_file boot_file in
      trace_leave ();
      Some piqi
    )


(* this is a local function; it can be called more than once, but produce an
 * effect only on its first run *)
let init () =
  if not !is_initialized
  then (
    if !Config.debug_level > 0 || !Config.flag_trace
    then load_embedded_boot_piqi ();

    is_initialized := true
  )


(* public interface: read piqi file *)
let read_piqi fname :T.ast =
  let ch = Piqi_main.open_input fname in
  read_piqi_channel fname ch


(* public interface: load piqi file *)
let load_piqi fname :T.piqi =
  init ();
  trace "loading piqi file: %s\n" fname;
  trace_enter ();
  let ast = read_piqi fname in
  let piqi = load_piqi_ast fname ast ~cache:true in
  trace_leave ();
  piqi


(* expand all includes and, optionally, extensions and produce an expanded
 * version of the Piqi module *)
let expand_piqi ?(includes_only=false) piqi =
  let open P in
  let orig_piqi = some_of piqi.original_piqi in

  (* create a new piqi module from the original piqi module *)
  let res_piqi =
    {
      orig_piqi with

      includ = [];

      extend =
        if includes_only
        then piqi.extend (* all extensions *)
        else []; (* extensions are already applied *)

      piqdef =
        if includes_only
        then piqi.piqdef (* all typedefs *)
        else piqi.extended_piqdef; (* all typedefs with extensions applied *)

      import =
        if includes_only
        then piqi.import
        else piqi.extended_import; (* all imports with extensions applied *)

      func =
        if includes_only
        then piqi.func
        else piqi.extended_func; (* all functions with extensions applied *)
    }
  in
  res_piqi


(* narrow down Piqi language to Piqi specficiation
 *
 * Piqi language (piqi-lang.piqi) adds the following extensions to Piqi
 * specification (piqi.piqi):
 *
 *   - includes
 *   - extensions
 *   - custom fields
 *   - embedded definition of function parameters
 *
 * In addition, in certain cases, Piqi specificaion should have fully resolved
 * defaults. For example, all required fields should be explicitly marked as
 * such instead of leaving this value out and assuming the are required by
 * default.
 *)
let lang_to_spec piqi =
  let open P in
  (* expand includes and apply all extensions *)
  let piqi = expand_piqi piqi in
  (* remove custom fields handle embedded typedefs in functions *)
  let res_piqi =
    {
      piqi with
      custom_field = [];
      (* TODO: transform functions to remove embedded type definitions and add
       * them at top-level *)
    }
  in
  res_piqi

