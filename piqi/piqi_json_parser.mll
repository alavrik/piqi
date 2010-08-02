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

The JSON parser implementation is based on Martin Jambon's "yojson" library.
The original code was taken from here: 
  svn://scm.ocamlcore.org/svnroot/yojson/trunk/yojson

Below is the original copyright notice and the license:

Copyright (c) 2010 Martin Jambon
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

{
  open Piqi_json_common

  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
	buf.lex_start_p <- buf.lex_curr_p;
	buf.lex_curr_p <- {buf.lex_curr_p
			   with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing


  type lexer_state = {
    buf : Buffer.t;
      (* Buffer used to accumulate substrings *)

    mutable lnum : int;
      (* Current line number (starting from 1) *)

    mutable bol : int;
      (* Absolute position of the first character of the current line 
	 (starting from 0) *)

    mutable fname : string option;
      (* Name describing the input file *)
  }


  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol in
    let _pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol - 1) in
    let filename =
      match v.fname with
	  None -> "" (* XXX, TODO: make it uniform across Piqi *)
	| Some s -> s
    in
    let bytes =
      if pos1 = _pos2
      then (pos1+1)
      else (pos1+1) (* (_pos2+1) *)
    in
    let loc = (filename, v.lnum, bytes) in
    Piqi_common.error_at loc descr


  let lexer_error descr v lexbuf =
    custom_error 
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf


  let retry_parse_uint s =
    Piqi_c.piqi_strtoull s

  let parse_uint s =
    (* see Piq_parser for details *)
    try Int64.of_string s
    with Failure _ ->
      retry_parse_uint s

  let parse_int64 s =
    match s.[0] with
      | '-' -> `Int64 (Int64.of_string s) (* negative integer *)
      | _ -> `Uint64 (parse_uint s)


  let make_int s v lexbuf =
    try `Int (int_of_string s)
    with Failure _ ->
      try parse_int64 s
      with Failure _ ->
        lexer_error "Int overflow" v lexbuf


  let set_file_name v fname =
    v.fname <- fname

  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_substring buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f lexbuf.lex_buffer lexbuf.lex_start_pos len

  (* these exceptions are used internally to terminate top-down parsing
   * branches *)
  exception End_of_array
  exception End_of_object
}

let space = [' ' '\t' '\r']+

let digit = ['0'-'9']
let nonzero = ['1'-'9']
let digits = digit+
let frac = '.' digits
let e = ['e' 'E']['+' '-']?
let exp = e digits

let positive_int = (digit | nonzero digits)
let float = '-'? positive_int (frac | exp | frac exp)
let number = '-'? positive_int (frac | exp | frac exp)?

let hex = [ '0'-'9' 'a'-'f' 'A'-'F' ]

let ident = ['a'-'z' 'A'-'Z' (* XXX: *) '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*


(* taken from http://www.w3.org/2005/03/23-lex-U *)
(* NOTE: may occur only in strings, thus exluding control characters (1, 0x1f),
 * '"', and '\\' *)
let utf8_char =
(* (* 1 *)   ['\x00'-'\x7F'] -- original class *)
(* 1 *)   [^ '\x00'-'\x1F' '\x80'-'\xff' '"' '\\']
(* 2 *) | (['\xC2'-'\xDF'] ['\x80'-'\xBF'])
(* 3 *) | ( '\xE0'         ['\xA0'-'\xBF'] ['\x80'-'\xBF'])
(* 4 *) | (['\xE1'-'\xEC'] ['\x80'-'\xBF'] ['\x80'-'\xBF'])
(* 5 *) | ( '\xED'         ['\x80'-'\x9F'] ['\x80'-'\xBF'])
(* 6 *) | (['\xEE'-'\xEF'] ['\x80'-'\xBF'] ['\x80'-'\xBF'])
(* 7 *) | ( '\xF0'         ['\x90'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF'])
(* 8 *) | (['\xF1'-'\xF3'] ['\x80'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF'])
(* 9 *) | ( '\xF4'         ['\x80'-'\x8F'] ['\x80'-'\xBF'] ['\x80'-'\xBF'])

(* '"' *)


rule read_json v = parse
  | "true"      { `Bool true }
  | "false"     { `Bool false }
  | "null"      { `Null }
  | '"'         {
                  if not !Piqi_config.pp_mode
                  then
	            (Buffer.clear v.buf;
		    `String (finish_string v lexbuf)
                    )
                  else
                    `Stringlit (finish_stringlit v lexbuf)
                }
  | '-'? positive_int
                {
                  let s = lexeme lexbuf in
                  if not !Piqi_config.pp_mode
                  then make_int s v lexbuf
                  else `Intlit s
                }
  | float       {
                  if not !Piqi_config.pp_mode
                  then
                    `Float (float_of_string (lexeme lexbuf))
                  else
                    `Floatlit (lexeme lexbuf)
                }

  | '{'          { let acc = ref [] in
		   try
		     read_space v lexbuf;
		     read_object_end lexbuf;
		     let field_name = read_ident v lexbuf in
		     read_space v lexbuf;
		     read_colon v lexbuf;
		     read_space v lexbuf;
		     acc := (field_name, read_json v lexbuf) :: !acc;
		     while true do
		       read_space v lexbuf;
		       read_object_sep v lexbuf;
		       read_space v lexbuf;
		       let field_name = read_ident v lexbuf in
		       read_space v lexbuf;
		       read_colon v lexbuf;
		       read_space v lexbuf;
		       acc := (field_name, read_json v lexbuf) :: !acc;
		     done;
		     assert false
		   with End_of_object ->
		     `Assoc (List.rev !acc)
		 }

  | '['          { let acc = ref [] in
		   try
		     read_space v lexbuf;
		     read_array_end lexbuf;
		     acc := read_json v lexbuf :: !acc;
		     while true do
		       read_space v lexbuf;
		       read_array_sep v lexbuf;
		       read_space v lexbuf;
		       acc := read_json v lexbuf :: !acc;
		     done;
		     assert false
		   with End_of_array ->
		     `List (List.rev !acc)
		 }

  | "\n"         { newline v lexbuf; read_json v lexbuf }
  | space        { read_json v lexbuf }
  | eof          { custom_error "Unexpected end of input" v lexbuf }
  | _            { lexer_error "Invalid token" v lexbuf }


and finish_string v = parse
    '"'           { Buffer.contents v.buf }
  | '\\'          { finish_escaped_char v lexbuf;
		    finish_string v lexbuf }
  (*
  | [^ '"' '\\']+ -- original 8-bit sequence
  *)
  | utf8_char+    { add_lexeme v.buf lexbuf;
		    finish_string v lexbuf }
  | ['\x00'- '\x1f']
                  { custom_error "Invalid string literal" v lexbuf }
  | _             { custom_error "Invalid utf-8 sequence" v lexbuf }
  | eof           { custom_error "Unexpected end of input" v lexbuf }


and finish_escaped_char v = parse 
    '"'
  | '\\'
  | '/' as c { Buffer.add_char v.buf c }
  | 'b'  { Buffer.add_char v.buf '\b' }
  | 'f'  { Buffer.add_char v.buf '\012' }
  | 'n'  { Buffer.add_char v.buf '\n' }
  | 'r'  { Buffer.add_char v.buf '\r' }
  | 't'  { Buffer.add_char v.buf '\t' }
  | 'u' (( hex hex hex hex ) as s)
         { let i = Piq_lexer.int_of_xstring s in
           Utf8.store v.buf i }
  | _    { lexer_error "Invalid escape sequence" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }


and finish_stringlit v = parse
    ( '\\' (['"' '\\' '/' 'b' 'f' 'n' 'r' 't'] | 'u' hex hex hex hex)
    (* | [^'"' '\\'] )* '"' -- original 8-bit sequence *)
    | utf8_char )* '"'
         {
           let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
	   let s = String.create (len+1) in
	   s.[0] <- '"';
	   String.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
	   s
	 }
  | ['\x00'- '\x1f']
         { lexer_error "Invalid string literal" v lexbuf }
  | _    { custom_error "Invalid utf-8 sequence" v lexbuf }
  | eof  { custom_error "Unexpected end of input" v lexbuf }


(* Readers expecting a particular JSON construct *)

and read_eof = parse
    eof       { true }
  | ""        { false }

and read_space v = parse
  | '\n'                     { newline v lexbuf; read_space v lexbuf }
  | [' ' '\t' '\r']+         { read_space v lexbuf }
  | ""                       { () }

and read_ident v = parse
  | '"' (ident as s) '"'
             { s }
  | _        { lexer_error "Expected string identifier but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }


and read_array_end = parse
    ']'      { raise End_of_array }
  | ""       { () }

and read_array_sep v = parse
    ','      { () }
  | ']'      { raise End_of_array }
  | _        { lexer_error "Expected ',' or ']' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_object_end = parse
    '}'      { raise End_of_object }
  | ""       { () }

and read_object_sep v = parse
    ','      { () }
  | '}'      { raise End_of_object }
  | _        { lexer_error "Expected ',' or '}' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and read_colon v = parse
    ':'      { () }
  | _        { lexer_error "Expected ':' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }


{
  (* TODO, XXX: detect JSON encoding and make sure that it is utf-8 *)

  let _ = (read_json : lexer_state -> Lexing.lexbuf -> json)

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      custom_error "Junk after end of JSON value" v lexbuf

  let init_lexer ?buf ?fname ?(lnum = 1) () =
    let buf =
      match buf with
	  None -> Buffer.create 256
	| Some buf -> buf
    in
    {
      buf = buf;
      lnum = lnum;
      bol = 0;
      fname = fname
    }


  let read_next (v, lexbuf) =
    read_space v lexbuf;
    if read_eof lexbuf
    then None
    else Some (read_json v lexbuf)


  let read_all json_parser =
    let rec aux accu =
      match read_next json_parser with
        | None -> List.rev accu
        | Some x -> aux (x::accu)
    in aux []


  let init_from_string ?buf ?fname ?lnum s =
    let lexbuf = Lexing.from_string s in
    let v = init_lexer ?buf ?fname ?lnum () in
    (v, lexbuf)


  let init_from_channel ?buf ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    (v, lexbuf)
}
