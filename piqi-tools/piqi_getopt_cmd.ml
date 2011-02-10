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
 * Command-line element parsing, pretty-printing and converting to various
 * output formats.
 *)


module C = Piqi_common
open C


(* command-line arguments *)
let output_encoding = ref ""
let typename = ref ""


(* index of the "--" element in argv array *)
let argv_start_index = ref 0


module Main = Piqi_main
open Main


let validate_options () =
  if !typename = "" (* pretty-print mode *)
  then (
    if !output_encoding <> ""
    then piqi_error "option -t can not be used without --piqtype";
  )


let getopt_command () =
  validate_options ();
  (* open output file *)
  let och = Main.open_output !ofile in
  (* interpret command-line arguments after "--" as Piq data *)
  let piq_ast = Piqi_getopt.getopt_piq !argv_start_index in
  match piq_ast with
    | None -> () (* no data *)
    | Some ast when !typename = "" ->
        (* with no --piqtype parameter given, just pretty-print the Piq AST *)
        Piqi_pp.prettyprint_ast och ast;
        output_char och '\n'
    | Some ast ->
        let writer = Piqi_convert.make_writer !output_encoding in
        let piqtype = Piqi_convert.find_piqtype !typename in
        (* parse the Piq AST according to "--piqtype" and convert to the output
         * format according to "-t" *)
        C.resolve_defaults := !Piqi_convert.flag_add_defaults;
        let piqobj = Piqobj_of_piq.parse_obj piqtype ast in
        (* write the object *)
        writer och (Piq.Typed_piqobj piqobj)


(* find the position of the first argument after "--" *)
let rest_fun arg =
  if !argv_start_index = 0 (* first argument after first occurence of "--" *)
  then argv_start_index := !Arg.current + 1
  else ()


let usage = "Usage: piqi getopt [options] -- [<data arguments>] \nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;

    "-t", Arg.Set_string output_encoding,
    "piq|wire|pb|json|piq-json output encoding (piq is used by default)";

    "--piqtype", Arg.Set_string typename,
    "<typename> type of the object represented by data arguments";

    "--add-defaults", Arg.Set Piqi_convert.flag_add_defaults,
    "add field default values while converting records";

    "--", Arg.Rest rest_fun,
    "separator between piqi command-line arguments and data arguments";
  ]


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:0;
  if !argv_start_index = 0 (* "--" is not present in the list of arguments *)
  then argv_start_index := Array.length Sys.argv;
  getopt_command ()

 
let _ =
  Main.register_command run "getopt"
    "interpret command-line arguments as Piq data, pretty-print and convert to various encodings"

