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
 * Interpreting command-line arguments as Piq data
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
  let token () =
    try
      Piq_lexer.token lexbuf
    with
      Piq_lexer.Error (err, _loc) -> error (err ^ ": " ^ s)
  in
  let res = token () in
  match res with
    | Piq_lexer.String _ ->
        (* there must be no other literal after the string *)
        if token() = Piq_lexer.EOF
        then res
        else
          (* s is alread quoted *)
          error ("trailing characters after string: " ^ s)
    | _ ->
        assert false (* something that starts with '"' have to be a string *)


let parse_word_arg s =
  if Piq_lexer.is_valid_word s
  then
    (* Raw word -- a valid utf8 Piq word: may be parsed as either of these: word,
     * bool, number, string, binary *)
    Piq_lexer.Raw_word s
  else
    (* Raw binary -- just a sequence of bytes: may be parsed as either binary or
     * utf8 string *)
    Piq_lexer.Raw_binary s


let parse_name_arg s =
  (* cut the leading '-' and check if what we got is a valid Piq name *)
  let n = String.sub s 1 (String.length s - 1) in
  if Piqi_name.is_valid_name n ~allow:"."
  then (
    s.[0] <- '.'; (* replace '-' with '.' to turn it into a Piq name *)
    Piq_lexer.Word s
  )
  else error ("invalid name: " ^ quote s)


let read_file filename =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let buf = Buffer.create len in
  Buffer.add_channel buf ch len;
  close_in ch;
  Buffer.contents buf


let read_file filename =
  try read_file filename
  with Sys_error s ->
    error ("error reading file argument: " ^ s)


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
    | s when s.[0] = '@' ->
        let filename = String.sub s 1 (len - 1) in
        let content = read_file filename in
        (* Raw binary -- just a sequence of bytes: may be parsed as either
         * binary or utf8 string *)
        Piq_lexer.Raw_binary content
    (* NOTE: it is safe to check s.[1] because a single '-' case is eliminated
     * above *)
    | s when s.[0] = '-' && (s.[1] < '0' || s.[1] > '9') -> parse_name_arg s
    | s when s.[0] = '.' -> parse_name_arg s (* Piq -style names *)
    (* XXX: support typenames and, possibly, other literals? *)
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


(* find the position of the first argument after "--" *)
let rest_fun arg =
  if !argv_start_index = 0 (* first argument after first occurence of "--" *)
  then argv_start_index := !Arg.current + 1
  else ()


let arg__rest =
    "--", Arg.Rest rest_fun,
    "separator between piqi command-line arguments and data arguments"


let getopt_piq () =
  let start =
    if !argv_start_index = 0 (* "--" is not present in the list of arguments *)
    then Array.length Sys.argv
    else !argv_start_index
  in
  let tokens = parse_argv start in
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

