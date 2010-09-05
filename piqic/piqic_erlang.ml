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
 * piq interface compiler for Erlang
 *)

open Piqi_common
open Piqic_common


(*
 * set Erlang names if not specified by user
 *)


(* Erlang name of piqi name *)
let erlang_name n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  dashes_to_underscores n


let erlang_lcname n = (* uncapitalize *)
  (* XXX, TODO: normalize instead of just uncapitalizing? *)
  String.uncapitalize (erlang_name n)


let erlname n =
  Some (erlang_lcname n)


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

let erl_modname n =
  (* TODO *)
  erlname (some_of n)


let rec erlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    if piqi.erlang_module = None
    then piqi.erlang_module <- erl_modname piqi.modname;

    (* if type prefix is not defined by user, set it to
     * <erlang-module-name> "_" *)
    if piqi.erlang_type_prefix = None
    then piqi.erlang_type_prefix <- Some (some_of piqi.erlang_module ^ "_");

    erlname_defs piqi.P#resolved_piqdef;
    erlname_defs piqi.P#imported_piqdef; (* XXX: why do we need that? *)
    erlname_imports piqi.P#resolved_import;
  end

and erlname_imports imports = List.iter erlname_import imports

and erlname_import import =
  let open Import in
  begin
    erlname_piqi (some_of import.piqi);
    import.erlang_name <- erlname (some_of import.name)
  end


open Iolist

module Main = Piqi_main
open Main


let piqic (piqi: T.piqi) =
  piqic_common piqi;

  (* set Erlang names which are not specified by user *)
  erlname_piqi piqi;

  (* set current module's name and type prefix *)
  let modname = some_of piqi.P#erlang_module in
  Piqic_erlang_types.top_modname := some_of piqi.P#erlang_module;
  Piqic_erlang_types.type_prefix := some_of piqi.P#erlang_type_prefix;

  (* open output .hrl file *)
  let ofile = modname ^ ".hrl" in
  let ch = Main.open_output ofile in

  (* call piq interface compiler for Erlang *)
  let types = Piqic_erlang_types.gen_piqi piqi in
  let def = "__" ^ String.uppercase modname ^ "_HRL__" in
  let def = ios def in
  let code = iol [
    ios "-ifndef("; def; ios ")."; eol;
    ios "-define("; def; ios ", 1)."; eol;
    eol;
    types;
    eol;
    ios "-endif."; eol;
  ]
  in
  Iolist.to_channel ch code;
  Main.close_output ();

  (* open output .erl file *)
  let ofile = modname ^ ".erl" in
  let ch = Main.open_output ofile in

  let code_gen = Piqic_erlang_out.gen_piqi piqi in
  let code_parse = Piqic_erlang_in.gen_piqi piqi in
  let code = iol [
    ios "-module("; ios modname; ios ")."; eol;
    ios "-compile(export_all)."; eol;
    eol;
    ios "-include_lib(\"piqirun/include/piqirun.hrl\")."; eol;
    ios "-include("; ioq (modname ^ ".hrl"); ios ")."; eol;
    eol;
    code_gen; eol;
    code_parse; eol;
  ]
  in
  Iolist.to_channel ch code;
  ()


(* command-line flags *)


let piqic_file ifile =

  (* load input .piqi file *)
  let piqi = Piqi.load_piqi ifile in

  (* chdir to the output directory *)
  Main.chdir_output !odir;

  piqic piqi


let usage = "Usage: piqic erlang [options] <.piqi file>\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_C;
  ]


let run () =
  Main.parse_args () ~usage ~speclist;
  piqic_file !ifile

 
let _ =
  Main.register_command run "erlang"
    "generate Erlang headers and codecs from %.piqi"

