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


module Main = Piqi_main
open Main


(* command-line arguments *)
let flag_expand_includes = ref false


let piqi_to_ast piqi =
  Piqi.mlobj_to_ast Piqi.piqi_def T.gen_piqi piqi


let unresolve_piqi piqi =
  P#{
    (* piqi with *)

    modname = None;
    proto_package = None;

    piqdef =
      if !flag_expand_includes
      then piqi.piqdef 
      else
        (* extended piqdef will be printed only once for the first included
         * module (see below) *)
        [];

    extend =
      if !flag_expand_includes
      then piqi.P#extend
      else [];

    includ = [];
    import = [];
    custom_field = [];

    (* strip all piqi-impl (implementation related) extensions *)
    extended_piqdef = [];
    resolved_piqdef = [];
    imported_piqdef = [];
    resolved_import = [];
    included_piqi = [];
    original_piqi = None;
  }


let print_piqi ch piqi =
  let ast = piqi_to_ast piqi in
  Piqi_pp.prettyprint_piqi_ast ch ast


let print_modname ch modname =
  match modname with
    | Some x ->
        (* original name *)
        output_string ch (".module " ^ x);
        output_string ch "\n\n"
    | None -> ()


let print_proto_package ch = function
  | Some x ->
      output_string ch (".proto-package " ^ quote x);
      output_string ch "\n\n"
  | None -> ()


let expand_piqi ch filename =
  (* TODO: don't resolve defaults: i.e. for field.mode *)

  (* TODO: tranform output AST to produce nicer results: strip optional filed
   * names, etc *)
  let piqi = Piqi.load_piqi filename in

  let all_piqi = piqi.P#included_piqi in
  let orig_piqi = some_of piqi.P#original_piqi in

  let imports = Piqi.get_imports all_piqi in
  (* XXX: they are already ignored during the expansion
  let ignored_fields = Piqi.get_ignored_fields all_piqi in
  *)

  (* print the top-level module *)
  let top = unresolve_piqi (List.hd all_piqi) in
  print_modname ch orig_piqi.P#modname;
  print_proto_package ch piqi.P#proto_package;
  (* moving other stuff at the top *)
  top.P#import <- imports;
  if not !flag_expand_includes
  then top.P#piqdef <- piqi.P#extended_piqdef;
  (* XXX: they are already ignored during the expansion
  top.P#custom_field <- custom_fields;
  *)
  print_piqi ch top;

  (* print the rest of includes *)
  List.iter (fun x ->
    let x = unresolve_piqi x in
    print_piqi ch x) (List.tl all_piqi)


let usage = "Usage: piqi expand [options] <.piqi file> [output file]\nOptions:"

let speclist = Main.common_speclist @
  [
    arg_o;

   "--includes-only", Arg.Set flag_expand_includes,
     "expand only includes (don't expand extensions)";
  ]


let expand_file filename =
  let ch = Main.open_output !ofile in
  expand_piqi ch filename


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:2;
  expand_file !ifile

 
let _ =
  Main.register_command run "expand"
    "expand %.piqi includes and extensions"

