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

    Piqi_main.arg__include_extension;
  ]


let expand_file filename =
  let ch = Main.open_output !ofile in
  let piqi = Piqi.load_piqi filename in
  let res_piqi = Piqi.expand_piqi piqi ~includes_only:!flag_includes_only in
  Piqi_pp.prettyprint_piqi ch res_piqi


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:2;
  expand_file !ifile

 
let _ =
  Main.register_command run "expand"
    "expand %.piqi includes and extensions"

