(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

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
let output_format = Convert.output_format
let typename = Convert.typename


let validate_options () =
  if !typename = "" (* pretty-print mode *)
  then (
    if !output_format <> ""
    then piqi_error "option -t can not be used without --type";
  )


let getopt_command () =
  Piqi_getopt.init ();
  Convert.init ();

  validate_options ();
  (* open output file *)
  let och = Main.open_output !Main.ofile in
  (* interpret command-line arguments after "--" as Piq data *)
  let piq_ast_list = Piqi_getopt.getopt_piq () in
  match piq_ast_list with
    | [] when !typename = "" -> () (* no data *)
    | _ when !typename = "" ->
        (* with no --type parameter given, just pretty-print the Piq AST *)
        let ast =
          (* if there's more that one element, wrap them into a list *)
          match piq_ast_list with
            | [x] -> x
            | l -> `list l
        in
        Piqi_pp.prettyprint_ast och ast
    | _ ->
        let writer = Convert.make_writer !output_format in
        let piqtype = Piqi_convert.find_type !typename in
        let piqobj = Piqi_getopt.parse_args piqtype piq_ast_list in

        (* write the object *)
        writer och (Piqi_convert.Typed_piqobj piqobj)


let usage = "Usage: piqi getopt [options] -- [<data arguments>] \nOptions:"


let speclist = Main.common_speclist @
  [
    Main.arg__strict;
    Main.arg_o;

    Convert.arg__type;

    Convert.arg_t;
    Convert.arg__add_defaults;
    Convert.arg__json_omit_missing_fields;
    Convert.arg__json_omit_null_fields;
    Convert.arg__piq_frameless_output;

    Main.arg__include_extension;
    Piqi_getopt.arg__rest;
  ]


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:0;
  getopt_command ()

 
let _ =
  Main.register_command run "getopt"
    "interpret command-line arguments as Piq data, pretty-print and convert to various formats"

