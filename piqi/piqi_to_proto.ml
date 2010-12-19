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


(*
 * Generate Protocol Buffers .proto file from .piqi definitions
 *)


module C = Piqi_common
open C
open Iolist
 

(* XXX: move to some common module *)
let gen_code code =
  ios (Int32.to_string (some_of code)) 
  (*
  ios (string_of_int (some_of code))
  *)


(* generate fully-qualified proto names using parents *)
let gen_name parent name =
  let name = some_of name in
  match some_of parent with
    | `piqi _ -> name (* local module *)
    | `import x ->
        let piqi = some_of x.Import#piqi in
        match piqi.P#proto_package with
          | None -> name (* no .proto package => use flat .proto namespace *)
          | Some package ->
              (* build fully-qualified name *)
              "." ^ package ^ "." ^ name


(* indication whether 'any' type was used somewhere in .piqi specification *)
let is_any_used = ref false


let rec typename (t:T.piqtype) =
  match t with
    | `int -> "sint32"
    | `float -> "double"
    | `bool -> "bool"
    | `string -> "string"
    | `word -> "string"
    | `binary -> "bytes"
    | `text -> "string"
    | `record t -> gen_name t.R#parent t.R#proto_name
    | `variant t -> gen_name t.V#parent t.V#proto_name
    | `enum t -> gen_name t.E#parent t.E#proto_name
    | `list t -> gen_name t.L#parent t.L#proto_name
    | `alias t ->
        (* unwind aliases to their original type *)
        gen_alias t
    | `any ->
        is_any_used := true;
        (* NOTE: protoc has a bug: when compiling C++ stubs it doesn't
         * convert proto names into fully qualified C++ names,
         * e.g. wire_type is converted to piqi::piqi::wire_type -- and not
         * ::piqi::piqi::wire_type. Non-fully qualified name in case of
         * repeated namespace parts confuses g++
         *
         * Thus, adding "_org" suffix to the top namespace for now.
         *)
        ".piqi_org.piqtype.any"


and gen_alias x =
  let open Alias in
  match x.proto_type with
    | Some x -> x
    | None ->
        match piqtype x.typeref with
          | `alias x -> gen_alias x
          | x -> typename x


let gen_typeref t =
  ios (typename (piqtype t))


let gen_typeref' = function
  | None -> ios "bool"
  | Some t -> gen_typeref t


let piqdef_proto_name = function
  | `record t -> some_of t.R#proto_name
  | `variant t -> some_of t.V#proto_name
  | `enum t -> some_of t.E#proto_name
  | `alias t -> some_of t.A#proto_name
  | `list t -> some_of t.L#proto_name
  | _ ->
      (* this function will be called only for named types (i.e. piqdefs) *)
      assert false


let protoname_of name typeref =
  match name, typeref with
    | Some n, _ -> ios n
    | None, Some t ->
        ios (piqdef_proto_name t)
    | _ -> assert false


let protoname_of_field f =
  let open F in protoname_of f.proto_name f.typeref


let protoname_of_option o =
  let open O in protoname_of o.proto_name o.typeref


let string_of_mode = function
    | `required -> "required"
    | `optional -> "optional"
    | `repeated -> "repeated"


let make_default_io x =
  iol [ ios " [default = "; x; ios "]" ]


let make_default s =
  make_default_io (ios s)


(* NOTE: not generating defaults for structured objects *)
let rec gen_default_obj (x:Piqobj.obj) =
  match x with
    (* built-in types *)
    | `int x ->
        make_default (Int64.to_string x)
    | `uint x ->
        make_default (Printf.sprintf "%Lu" x)
    | `float x ->
        (* NOTE: This is correct, since Ocaml follows the same notation for
         * floats as in Protobuf's ".proto" files: nan, * inf, -inf *)
        make_default (Piq_gen.string_of_float x)
    | `bool true -> make_default "true"
    | `bool false -> make_default "false"
    | `string x | `word x | `text x ->
        (* NOTE: Piqi escapes is a subset of C escapes used by Protobuf *)
        make_default_io (ioq (Piq_lexer.escape_string x))
    | `binary x ->
        make_default_io (ioq (Piq_lexer.escape_binary x))
    | `enum x ->
        let o = x.Piqobj.Variant#option in
        let o = o.Piqobj.Option#piqtype in
        make_default (some_of o.O#proto_name)
    | `alias x ->
        gen_default_obj x.Piqobj.Alias#obj (* recurse *)
    | `any _ | `record _ | `variant _ | `list _ ->
        (* NOTE: Protobuf doesn't support defaults for complex structures *)
        warning x "dropping default value as .proto doesn't support structured defaults";
        iol []


(* using the same technique as in Piqi.resolve_field_default *)
let gen_default x =
  let open F in
  match x.default, x.typeref with
    | Some {T.Any.ast = Some ast }, Some typeref ->
        let piqtype = C.piqtype typeref in
        let piqobj = Piqobj_of_piq.parse_obj piqtype ast in
        gen_default_obj piqobj
    | _, _ -> iol [] (* there is no default *)


let gen_field f = 
  let open F in
  let fdef = iod " " (* field definition *)
    [
      ios (string_of_mode f.mode);
      gen_typeref' f.typeref;
      protoname_of_field f; ios "=";
        gen_code f.code ^^ gen_default f ^^ ios ";";
    ]
  in
  fdef


let gen_record r =
  let open R in
  (* field definition list *) 
  let fdefs = List.map gen_field r.field in
  let rdef = iol
    [
      ios "message "; ios (some_of r.proto_name);
      ios " {"; indent;
      iod "\n" fdefs; unindent; eol;
      ios "}"; eol;
    ]
  in rdef


let gen_const c =
  let open O in
  iod " " [
    ios (some_of c.proto_name); ios "="; gen_code c.code ^^ ios ";";
  ]


let gen_enum e =
  let open E in
  let const_defs = List.map gen_const e.option in
  iol
    [
      ios "enum "; ios (some_of e.proto_name); ios " {"; indent;
        iod "\n" const_defs; unindent; eol;
      ios "}"; eol;
    ]


let gen_option o =
  let open Option in
  iod " " [
    ios "optional"; gen_typeref' o.typeref;
      protoname_of_option o; ios "="; gen_code o.code ^^ ios ";";
  ]


let gen_variant v =
  let open Variant in
  (* field definition list *) 
  let vdefs = List.map gen_option v.option in
  let vdef = iol
    [
      ios "message "; ios (some_of v.proto_name);
      ios " {"; indent;
      iod "\n" vdefs; unindent; eol;
      ios "}"; eol;
    ]
  in vdef


let gen_list l =
  let open L in
  let ldef = iol
    [
      ios "message "; ios (some_of l.proto_name);
      ios " {"; indent;
      ios "repeated "; gen_typeref l.typeref; ios " elem = 1;";
      unindent; eol;
      ios "}"; eol;
    ]
  in ldef


let gen_def = function
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t
  | _ -> assert false


let gen_defs (defs:T.piqdef list) =
  (* don't generate alias definitions as protobuf doesn't have support for it *)
  let _, defs = List.partition (function `alias _ -> true | _ -> false) defs in
  let defs = List.map gen_def defs in
  iod "\n" defs


(* gen import path based on piqi modname *)
let gen_import_path modname =
  let _dir, fname = Piqi_file.find_piqi_file modname in
  (*
  (* XXX: revert slashes on Windows *)
  let fname = Piqi_file.make_os_path fname in
  *)
  let fname =
    if Filename.check_suffix fname ".piqi"
    then fname ^ ".proto"
    else
      if Filename.check_suffix fname ".proto.piqi"
      then Filename.chop_suffix fname ".piqi"
      else assert false (* we don't support other extensions *)
  in
  ioq fname


let gen_import x =
  let open Import in
  (* XXX: save filename in import record rather than resolving x.modname to
   * filename each time? *)
  iod " " [
    ios "import"; gen_import_path x.modname; ios ";";
    eol;
  ]


let gen_imports l =
  let l = List.map gen_import l in
  iol l


let gen_piqi (piqi:T.piqi) =
  let open P in
  let package =
    match piqi.P.proto_package with
      | None -> iol [] (* no package name *)
      | Some n -> iol [ios "package "; ios n; ios ";"; eol; eol]
  in
  (* add import "piqi.org/piqtype.piqi.proto" if 'any' typeref is used *)
  is_any_used := false;
  let defs = gen_defs piqi.P#resolved_piqdef in
  let piqi_import = 
    if !is_any_used && piqi.modname <> Some "piqi.org/piqtype"
    then
      iol [
        ios "import \"piqi.org/piqtype.piqi.proto\";";
        eol;
      ]
    else iol []
  in
  iod "\n" [
    package;
    piqi_import;
    gen_imports piqi.P#resolved_import;
    defs;
  ]


(*
 * set proto names if not specified by user
 *)

(* proto name of piqi name *)
let proto_name n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  dashes_to_underscores n


let protoname n =
  Some (proto_name n)


(* variant of protoname for optional names *)
let protoname' n =
  match n with
    | None -> None
    | Some n -> protoname n


let protoname_field x =
  let open Field in
  if x.proto_name = None then x.proto_name <- protoname' x.name


let protoname_record x =
  let open Record in
  (if x.proto_name = None then x.proto_name <- protoname x.name;
   List.iter protoname_field x.field)


let protoname_option x =
  let open Option in
  if x.proto_name = None then x.proto_name <- protoname' x.name


let protoname_variant x =
  let open Variant in
  (if x.proto_name = None then x.proto_name <- protoname x.name;
   List.iter protoname_option x.option)


let protoname_enum x =
  let open Enum in
  (if x.proto_name = None then x.proto_name <- protoname x.name;
   List.iter protoname_option x.option)


let protoname_alias x =
  let open Alias in
  if x.proto_name = None then x.proto_name <- protoname x.name


let protoname_list x =
  let open L in
  if x.proto_name = None then x.proto_name <- protoname x.name


let protoname_piqdef = function
  | `record x -> protoname_record x
  | `variant x -> protoname_variant x
  | `enum x -> protoname_enum x
  | `alias x -> protoname_alias x
  | `list x -> protoname_list x


let protoname_defs (defs:T.piqdef list) =
  List.iter protoname_piqdef defs


(*
let proto_modname n =
  let namespace, name = Piqi_name.split_modname (some_of n) in
  match namespace with
    | "local" -> 
        for i = 0 to String.length name - 1
        do
          if name.[i] = '/' then name.[i] <- '.'
        done;
        Some name
    | x ->
        error x ("unknown/unsupported namespace " ^ quote x)
*)


let rec protoname_piqi (piqi:T.piqi) =
  let open P in
  begin
    (*
    if piqi.proto_package = None then piqi.proto_package <- proto_modname piqi.modname;
    *)
    protoname_defs piqi.P#resolved_piqdef;
    protoname_defs piqi.P#imported_piqdef;
    protoname_imports piqi.P#resolved_import;
  end


and protoname_imports imports = List.iter protoname_import imports


and protoname_import import =
  let open Import in
  begin
    protoname_piqi (some_of import.piqi)
  end


let piqi_to_proto (piqi: T.piqi) ch =
  (* implicitly add defintions (aliases) from the boot module to the current
   * module *)
  let boot_defs =
    match !boot_piqi with
      | None -> []
      | Some x -> x. P#resolved_piqdef
  in
  piqi.P#resolved_piqdef <- boot_defs @ piqi.P#resolved_piqdef;

  (* set proto names which are not specified by user *)
  protoname_piqi piqi;
  let code = gen_piqi piqi in
  Iolist.to_channel ch code


module Main = Piqi_main
open Main


let piqi_to_proto_file ifile =
  let piqi = Piqi.load_piqi ifile in

  if not (Filename.check_suffix ifile ".piqi")
  then piqi_error "input file name must have '.piqi' extension";

  let ofile =
    if !ofile <> ""
    then !ofile
    else ifile ^ ".proto"
  in
  let ch = Main.open_output ofile in

  piqi_to_proto piqi ch


let usage = "Usage: piqi to-proto [options] <.piqi file>\nOptions:"

let speclist = Main.common_speclist @
  [
    arg_o;
  ]


let run () =
  Main.parse_args () ~speclist ~usage;
  piqi_to_proto_file !ifile

 
let _ =
  Main.register_command run "to-proto" "convert %.piqi to %.piqi.proto"

