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


module C = Piqi_common
open C


(* command-line parameters *)
let flag_normalize = ref false
let flag_expand_abbr = ref false
let flag_expand_names = ref false
let flag_expand_splices = ref false
let flag_parse_literals = ref false
let input_format = ref ""
let output_format = ref ""


let normalize_ast ast =
  Piq_ast.map_words ast Piqi_name.normalize_name


let transform_ast ast =
  let ast =
    if !flag_expand_names
    then Piq_parser.expand_names ast
    else ast
  in
  let ast =
    if !flag_expand_splices
    then Piq_parser.expand_splices ast
    else ast
  in
  let ast =
    if !flag_normalize
    then normalize_ast ast
    else ast
  in
  ast


let make_reader piq_input_format =
  let fname = !Main.ifile in
  let ch = Main.open_input fname in

  match piq_input_format with
    | "" ->
        (* piq reder/parser *)
        let piq_parser = Piq_parser.init_from_channel fname ch in
        let piq_ast_parser () =
          match Piq_parser.read_next piq_parser ~skip_trailing_comma:true with
            | Some ast -> ast
            | None -> raise Piqi_convert.EOF
        in
        piq_ast_parser
    | _ ->
        (* serialized piq ast reader *)
        Convert.make_piq_ast_parser piq_input_format fname ch


let write_piq piq_output_format ch piq_ast =
  match piq_output_format with
    | "" ->
        Piqi_pp.prettyprint_ast ch piq_ast;
        output_char ch '\n'
    | _ ->
        Convert.write_piq_ast piq_output_format ch piq_ast


let make_writer piq_output_format =
  write_piq piq_output_format


let check_input_or_output_format name format =
  match format with
    | "piq" -> ()
    | "piqi" -> ()
    | x ->
        piqi_error ("unknown " ^ name ^ " format: " ^ U.quote x)


let validate_options input_format output_format =
  check_input_or_output_format "input" input_format;
  check_input_or_output_format "output" output_format;
  ()


let prettyprint_file () =
  Convert.init ();

  let input_format, piq_input_format = Convert.get_input_format !input_format in
  let output_format, piq_output_format = Convert.get_output_format !output_format in

  validate_options input_format output_format;

  let reader = make_reader piq_input_format in
  let writer = make_writer piq_output_format in

  (* open output file *)
  let och = Main.open_output !Main.ofile in

  try
    while true do
      let ast = reader () in

      let ast = transform_ast ast in

      writer och ast;

      (* reset location db to allow GC to collect previously read objects *)
      Piqloc.reset ();
    done
  with Piqi_convert.EOF -> ()


let usage = "Usage: piqi pp [options] [<.piqi|.piq file>] [output file]\nOptions:"


(* command-line parameters *)
let arg_f =
    "-f", Arg.Set_string input_format,
    "piq|piq.pb|piq.json|piq.xml|piq.piq input format (piq is used by default)"

let arg_t =
    "-t", Arg.Set_string output_format,
    "piq|piq.pb|piq.json|piq.xml|piq.piq output format (piq is used by default)"


let speclist = Main.common_speclist @
  [
    Main.arg_o;

    (* input *)
    arg_f;

    "--normalize-words", Arg.Set flag_normalize,
    "normalize all words while pretty-printing (convert CamelCase to camel-case)";

    "--expand-abbr", Arg.Set flag_expand_abbr,
    "expand built-in syntax abbreviations, equivalent to --expand-names --expand-splices";

    "--expand-names", Arg.Set flag_expand_names,
    "expand chained names, e.g. .foo.bar ...";

    "--expand-splices", Arg.Set flag_expand_splices,
    "expand splices, e.g. foo* [ ... ]";

    "--parse-literals", Arg.Set flag_parse_literals,
    "deprecated: this option has no effect";

    Convert.arg__piq_relaxed_parsing;

    (* output *)
    arg_t;

    Main.arg__;
  ]


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;

  if !flag_expand_abbr
  then (
    flag_expand_names := true;
    flag_expand_splices := true;
  );

  prettyprint_file ()

 
let _ =
  Main.register_command run "pp" "pretty-print %.piqi or %.piq"

