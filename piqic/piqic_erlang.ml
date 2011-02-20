(*pp camlp4o -I $PIQI_ROOT/camlp4 pa_labelscope.cmo pa_openin.cmo *)
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


(* 
 * piq interface compiler for Erlang
 *)

open Piqi_common


(*
 * set Erlang names if not specified by user
 *)

let _ =
  (* normalize Piqi identifiers unless overrided by the command-line option *)
  Piqic_common.flag_normalize := true


(* Erlang name of piqi name *)
let erlang_name n =
  let n =
    if !Piqic_common.flag_normalize
    then Piqi_name.normalize_name n
    else String.uncapitalize n
  in
  dashes_to_underscores n


let erlname n =
  Some (erlang_name n)


(* variant of erlname for optional names *)
let erlname' n =
  match n with
    | None -> n
    | Some n -> erlname n


let erlname_field x =
  let open Field in
  if x.erlang_name = None then x.erlang_name <- erlname' x.name


let erlname_record x =
  let open Record in
  (if x.erlang_name = None then x.erlang_name <- erlname x.name;
   List.iter erlname_field x.field)


let erlname_option x =
  let open Option in
  if x.erlang_name = None then x.erlang_name <- erlname' x.name


let erlname_variant x =
  let open Variant in
  (if x.erlang_name = None then x.erlang_name <- erlname x.name;
   List.iter erlname_option x.option)


let erlname_enum x =
  let open Enum in
  (if x.erlang_name = None then x.erlang_name <- erlname x.name;
   List.iter erlname_option x.option)


let erlname_alias x =
  let open Alias in
  if x.erlang_name = None then x.erlang_name <- erlname x.name


let erlname_list x =
  let open L in
  if x.erlang_name = None then x.erlang_name <- erlname x.name


let erlname_piqdef = function
  | `record x -> erlname_record x
  | `variant x -> erlname_variant x
  | `enum x -> erlname_enum x
  | `alias x -> erlname_alias x
  | `list x -> erlname_list x


let erlname_defs (defs:T.piqdef list) =
  List.iter erlname_piqdef defs


let erlname_func_param func_name param_name param =
  let make_name () =
    Some (func_name ^ "_" ^ param_name)
  in
  match param with
   | None -> ()
   | Some (`record x) ->
       x.R#erlang_name <- make_name ()
   | Some (`alias x) ->
       x.A#erlang_name <- make_name ()


let erlname_func x =
  let open T.Func in (
    if x.ocaml_name = None then x.erlang_name <- erlname x.name;
    let func_name = some_of x.erlang_name in
    erlname_func_param func_name "input" x.resolved_input;
    erlname_func_param func_name "output" x.resolved_output;
    erlname_func_param func_name "error" x.resolved_error;
  )


let erlname_functions l =
  List.iter erlname_func l


let erl_modname n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  Some (erlang_name n)


let rec erlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    if piqi.erlang_module = None
    then piqi.erlang_module <- erl_modname (some_of piqi.modname);

    (* if type prefix is not defined by user, set it to
     * <erlang-module-name> "_" *)
    if piqi.erlang_type_prefix = None
    then piqi.erlang_type_prefix <- Some (some_of piqi.erlang_module ^ "_");

    (* naming function parameters first, because otherwise they will be
     * overriden in erlname_defs *)
    erlname_functions piqi.P#resolved_func;

    erlname_defs piqi.P#resolved_piqdef;
    erlname_defs piqi.P#imported_piqdef;
    erlname_imports piqi.P#resolved_import;
  end

and erlname_imports imports = List.iter erlname_import imports

and erlname_import import =
  let open Import in
  begin
    match import.piqi with
      | None -> () (* unresolved meaning that is called from piqic_expand.ml *)
      | Some piqi -> (* normal "piqic erlang" mode -- naming the dependencies *)
          erlname_piqi piqi
  end


open Iolist

module Main = Piqi_main
open Main


(* command-line flags *)
let flag_gen_impl_header = ref false
let flag_gen_default_impl = ref false


let gen_hrl modname piqi =
  (* open output .hrl file *)
  let ofile = modname ^ ".hrl" in
  let ch = Main.open_output ofile in

  (* call piq interface compiler for Erlang *)
  let types = Piqic_erlang_types.gen_piqi piqi in
  let def = "__" ^ String.uppercase modname ^ "_HRL__" in
  let code = iol [
    ios "-ifndef("; ios def; ios ")."; eol;
    ios "-define("; ios def; ios ", 1)."; eol;
    eol;
    types;
    eol;
    ios "-endif."; eol;
  ]
  in
  Iolist.to_channel ch code;
  Main.close_output ()


let gen_embedded_piqi piqi =
  let l = Piqic_common.build_piqi_deps piqi in
  let l = List.map Piqic_erlang_in.gen_erlang_binary l in
  iol [
    ios "piqi() ->"; indent;
      ios "["; indent;
        iod ",\n        " l;
        unindent; eol;
      ios "].";
    unindent; eol;
  ]


let gen_erl modname piqi =
  (* open output .erl file *)
  let ofile = modname ^ ".erl" in
  let ch = Main.open_output ofile in

  let code_gen = Piqic_erlang_out.gen_piqi piqi in
  let code_parse = Piqic_erlang_in.gen_piqi piqi in
  let code_gen_defauls =
    if !Piqic_common.flag_gen_defaults || !flag_gen_default_impl
    then Piqic_erlang_defaults.gen_piqi piqi
    else iol []
  in
  let code_embedded_piqi =
    if !Piqic_common.flag_embed_piqi || !flag_gen_default_impl
    then gen_embedded_piqi piqi
    else iol []
  in
  let code = iol [
    ios "-module("; ios modname; ios ")."; eol;
    ios "-compile(export_all)."; eol;
    eol;
    ios "-include_lib(\"piqirun/include/piqirun.hrl\")."; eol;
    ios "-include("; ioq (modname ^ ".hrl"); ios ")."; eol;
    eol;
    code_gen; eol;
    code_parse; eol;
    code_gen_defauls; eol;
    code_embedded_piqi; eol;
  ]
  in
  Iolist.to_channel ch code;
  Main.close_output ()


let gen_impl_hrl modname piqi =
  (* open output _impl.hrl file *)
  let ofile = modname ^ "_impl.hrl" in
  let ch = Main.open_output ofile in

  let specs_code = Piqic_erlang_impl.gen_function_specs piqi in
  let def = "__" ^ String.uppercase modname ^ "_IMPL_HRL__" in
  let code = iol [
    ios "-ifndef("; ios def; ios ")."; eol;
    ios "-define("; ios def; ios ", 1)."; eol;
    eol;
    ios "-include("; ioq (modname ^ ".hrl"); ios ")."; eol;
    eol;
    specs_code; eol;
    eol;
    ios "-endif."; eol;
  ]
  in
  Iolist.to_channel ch code;
  Main.close_output ()


let gen_default_impl_erl modname piqi =
  (* open output _default_impl.erl file *)
  let ofile = modname ^ "_default_impl.erl" in
  let ch = Main.open_output ofile in
  let impl = Piqic_erlang_impl.gen_function_default_impls piqi in
  let code = iol [
    ios "-module("; ios modname; ios "_default_impl)."; eol;
    ios "-compile(export_all)."; eol;
    eol;
    ios "-include("; ioq (modname ^ "_impl.hrl"); ios ").";
    eol; eol;
    impl;
    eol; eol;
  ]
  in
  Iolist.to_channel ch code;
  Main.close_output ()


let piqic (piqi: T.piqi) =
  Piqic_common.piqic_common piqi;

  (* set Erlang names which are not specified by user *)
  erlname_piqi piqi;

  (* set current module's name and type prefix *)
  let modname = some_of piqi.P#erlang_module in
  Piqic_erlang_types.top_modname := some_of piqi.P#erlang_module;
  Piqic_erlang_types.type_prefix := some_of piqi.P#erlang_type_prefix;

  (* set Erlang name for the type "any" *)
  if !Piqic_common.is_self_spec
  then (
    let def = Piqi_db.find_local_piqdef piqi "any" in
    let erl_name = Piqic_erlang_types.piqdef_erlname def in
    Piqic_erlang_types.any_erlname := erl_name
  );

  gen_hrl modname piqi;
  gen_erl modname piqi;

  if !flag_gen_impl_header || !flag_gen_default_impl
  then gen_impl_hrl modname piqi;

  if !flag_gen_default_impl
  then gen_default_impl_erl modname piqi;
  ()


let piqic_file ifile =
  Piqic_common.init ();

  (* load input .piqi file *)
  let piqi = Piqi.load_piqi ifile in

  (* chdir to the output directory *)
  Main.chdir_output !odir;

  piqic piqi


let usage = "Usage: piqic erlang [options] <.piqi file>\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_C;
    Piqic_common.arg__normalize;
    Piqic_common.arg__gen_defaults;
    Piqic_common.arg__embed_piqi;

    "--gen-impl-header", Arg.Set flag_gen_impl_header,
      "Genereate Erlang function specifications (_impl.hrl)";

    "--gen-default-impl", Arg.Set flag_gen_default_impl,
      "Genereate Erlang default function implementations (_default_impl.erl) (implies --embed-piqi, --gen-defaults, --gen-impl-header)"
  ]


let run () =
  Main.parse_args () ~usage ~speclist;
  piqic_file !ifile

 
let _ =
  Main.register_command run "erlang"
    "generate Erlang headers and codecs from %.piqi"

