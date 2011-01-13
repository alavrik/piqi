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


(* global constants *)
let getopt_filename = "argv" (* fake filename for error reporting *)


let error s =
  (* using fake location here, the actual location (i.e. the index of the
   * argument) will be correctly provided by the exception handler below *)
  let loc = (0,0) in
  raise (Piq_lexer.Error (s, loc))


let parse_string_arg s =
  let lexbuf = Piq_lexer.init_from_string s in
  let res =
    try
      Piq_lexer.token lexbuf
    with
      Piq_lexer.Error (err, _loc) -> error (err ^ ": " ^ s)
  in
  match res with
    | Piq_lexer.String (t, s') when String.length s' + 2 = String.length s ->
        res
    | Piq_lexer.String _ ->
        error ("trailing characters after string: " ^ s) (* s is alread quoted *)
    | _ ->
        assert false (* something that starts with '"' have to be a string *)


(* specifies the same as Piq_lexer's "let regexp word =" *)
let is_valid_word s =
  let is_valid_char = function
    | '(' | ')' | '[' | ']' | '{' | '}'
    | '"' | '%' | '#' | '\000'..'\032' | '\127' -> false
    | _ -> true
  in
  let len = String.length s in
  let rec check_char i =
    if i >= len
    then true
    else
      if is_valid_char s.[i] 
      then check_char (i + 1)
      else false
  in
  if len = 0
  then false
  else check_char 0

 
let parse_word_arg s =
  if is_valid_word s
  then Piq_lexer.Word s
  else
    (* return unquoted arg that is not a word as a Unicode string *)
    Piq_lexer.String (Piq_lexer.String_u, s)


let parse_name_arg s =
  if is_valid_word s
  then (
    s.[0] <- '.'; (* replace '-' with '.' to get a Piq name *)
    Piq_lexer.Word s
  )
  else error ("invalid name: " ^ quote s)


let parse_arg s =
  let len = String.length s in

  if len = 0
  then error "empty argument";
  if s = "-" || s = "--"
  then error ("invalid argument: " ^ s);

  match s with
    (* NOTE: we don't support '(' and ')' and '[]' is handeled separately below *)
    | "[" -> Piq_lexer.Lbr
    | "]" -> Piq_lexer.Rbr
    | s when s.[0] = '"' -> parse_string_arg s
    | s when s.[0] = '-' && (s.[1] < '0' || s.[1] > '9') -> parse_name_arg s
    | s -> parse_word_arg s


let parse_argv start =
  let make_token i tok =
    (* 1-based token position in the argv starting from the position after "--" *)
    let loc = (0, i - start + 1) in
    (tok, loc)
  in
  let parse_make_arg i x =
    let tok =
      try parse_arg x
      with Piq_lexer.Error (err, _loc) ->
        C.error_at (getopt_filename, 0, i) err
    in
    make_token i tok
  in
  let len = Array.length Sys.argv in
  let rec aux i =
    if i >= len
    then [make_token i Piq_lexer.EOF]
    else
      match Sys.argv.(i) with
        | "[]" -> (* split it into two tokens '[' and ']' *)
            Sys.argv.(i) <- "]";
            (parse_make_arg i "[") :: (aux i)
        | x -> 
            (parse_make_arg i x) :: (aux (i+1))
  in
  aux start


(* index of the "--" element in argv array *)
let argv_start_index = ref 0

(* command-line arguments *)
let output_encoding = ref ""
let typename = ref ""


module Main = Piqi_main
open Main


let getopt_piq () =
  let tokens = parse_argv !argv_start_index in
  let piq_parser = Piq_parser.init_from_token_list getopt_filename tokens in
  let piq_objects = Piq_parser.read_all piq_parser in
  let piq_ast =
    match piq_objects with
      | [] -> None
      | [x] -> Some x
      | l ->
          (* if there's more that one Piq objects, wrap them into a list *)
          let res = `list l in
          Piqloc.addref (List.hd l) res; (* preserve the location info *)
          Some res
  in
  piq_ast


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
  let piq_ast = getopt_piq () in
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
        Piqobj_of_piq.resolve_defaults := !Piqi_convert.flag_add_defaults;
        Piqobj_of_piq.parse_words_as_strings := true;
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
    "separator between piqi command-line arguments adn data arguments";
  ]


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:0;
  if !argv_start_index = 0 (* "--" is not present in the list of arguments *)
  then argv_start_index := Array.length Sys.argv;
  getopt_command ()

 
let _ =
  Main.register_command run "getopt"
    "interpret command-line arguments as Piq data, pretty-print and convert to various encodings"

