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


let init_res_piqi orig_piqi =
  let open P in
  {
    modname = orig_piqi.modname;
    proto_package = orig_piqi.proto_package;
    piqdef = [];
    extend = [];
    includ = [];
    import = [];
    func = [];

    custom_field = [];

    (* piqi-impl (implementation related) extensions *)
    extended_piqdef = [];
    resolved_piqdef = [];
    imported_piqdef = [];
    resolved_import = [];
    included_piqi = [];
    original_piqi = None;
  }


let expand_piqi ?(includes_only=false) piqi =
  let open P in
  let all_piqi = piqi.included_piqi in
  let orig_piqi = some_of piqi.original_piqi in

  (* create a new piqi module from the original piqi module *)
  let res_piqi = init_res_piqi orig_piqi in

  (* copy all imports to the resulting module *)
  let imports = Piqi.get_imports all_piqi in
  res_piqi.import <- imports;

  (* copy all definitions to the resulting module *)
  res_piqi.piqdef <-
    if includes_only
    then Piqi.get_piqdefs all_piqi
    else piqi.extended_piqdef;

  (* copy all extensions to the resulting module *)
  res_piqi.extend <-
    if includes_only
    then Piqi.get_extensions all_piqi
    else [];

  (* TODO, XXX: copy all functions to the resulting module *)

  res_piqi


module Main = Piqi_main
open Main


(* command-line arguments *)
let flag_includes_only = ref false

let usage = "Usage: piqi expand [options] <.piqi file> [output file]\nOptions:"

let speclist = Main.common_speclist @
  [
    arg_o;

   "--includes-only", Arg.Set flag_includes_only,
     "expand only includes (don't expand extensions)";
  ]


let expand_file filename =
  let ch = Main.open_output !ofile in
  let piqi = Piqi.load_piqi filename in
  let res_piqi = expand_piqi piqi ~includes_only:!flag_includes_only in
  Piqi_pp.prettyprint_piqi ch res_piqi


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:2;
  expand_file !ifile

 
let _ =
  Main.register_command run "expand"
    "expand %.piqi includes and extensions"

