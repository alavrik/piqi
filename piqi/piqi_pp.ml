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


open Piqi_common 


let normalize_list l =
  let isupper c = (c >= 'A' && c <= 'Z') in
  let tolower c =  Char.chr (Char.code c + 32) in
  let rec aux hump accu = function
    | [] -> List.rev accu
    | h::t when h = '_' || h = '-' ->
        aux true ('-'::accu) t
    | h::t when isupper h && not hump -> (* first hump character *)
        aux true ((tolower h)::'-'::accu) t
    | h::t when isupper h && hump -> (* another hump character *)
        aux hump ((tolower h)::accu) t
    | h::t when h = '.' || h = ':' || h = '/' ->
        aux true (h::accu) t
    | h::t -> (* end of hump *)
        aux false (h::accu) t
  in
  match l with
    | [] -> []
    | h::_ -> aux (isupper h) [] l


let normalize_name s =
  string_of_list (normalize_list (list_of_string s))


let map_words (x:T.ast) f =
  let rec aux = function
    | `word s -> `word (f s)
    | `name s -> `name (f s)
    | `named ({T.Named.value = v} as x) ->
        `named {x with T.Named.value = aux v}
    | `typed ({T.Typed.value = v} as x) ->
        let ast = some_of v.T.Any.ast in
        let v = {v with T.Any.ast = Some (aux ast)} in
       `typed {x with T.Typed.value = v}
    | `list l -> `list (List.map aux l)
    | `control l -> `control (List.map aux l)
    | x -> x
  in aux x


let normalize ast =
  map_words ast normalize_name


module Main = Piqi_main
open Main


let flag_normalize = ref false
let flag_expand_abbr = ref false


let usage = "Usage: piqi pp [options] [<.piqi|.piq file>] [output file]\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;

    "--normalize-words", Arg.Set flag_normalize,
    "normalize all words while pretty-printing (convert CamelCase to camel-case)";

    "--expand-abbr", Arg.Set flag_expand_abbr,
    "expand built-in syntax abbreviations";

    arg__;
  ]


(* old method for pretty-printing:
let prettyprint_ast ast =
  let code = Piq_gen.print_ast ast in
  Iolist.to_channel ch code
*)


let prettyprint_ast ch ast =
  Piq_gen.to_channel ch ast


let rec prettyprint_list ch ast_list =
  let rec aux = function
    | [] -> ()
    | [x] -> prettyprint_ast ch x
    | h::t ->
          prettyprint_ast ch h;
          output_char ch '\n';
          aux t
  in aux ast_list


let prettyprint_piqi_ast ch ast =
  match ast with
    | `list l -> prettyprint_list ch l
    | _ -> assert false


let transform_ast ast =
  let ast =
    if !flag_normalize
    then normalize ast
    else ast
  in
  let ast =
    if !flag_expand_abbr
    then Piq_parser.expand ast
    else ast
  in ast


let open_piq fname =
  let ch = Piqi_main.open_input fname in
  let piq_parser = Piq_parser.init_from_channel fname ch in
  piq_parser


let read_piq_obj piq_parser =
  Piq_parser.read_next piq_parser


let prettyprint_piq ch piq_parser =
  let rec aux () =
    match read_piq_obj piq_parser with
      | None -> ()
      | Some ast ->
          let ast = transform_ast ast in
          prettyprint_ast ch ast;
          output_char ch '\n';
          aux ()
  in aux ()


let prettyprint_file filename =
  let ch = Main.open_output !ofile in
  (* switch piq parser/generator to pretty-print mode *)
  Config.pp_mode := true;
  let piq_parser = open_piq filename in
  prettyprint_piq ch piq_parser


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  prettyprint_file !ifile

 
let _ =
  Main.register_command run "pp" "pretty-print %.piqi or %.piq"

