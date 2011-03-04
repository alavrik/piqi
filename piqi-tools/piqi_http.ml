(*pp camlp4o -I `ocamlfind query ulex` pa_ulex.cma *)
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


(* Piqi functionality related to HTTP
 *
 * This module contains HTTP response parser and a HTTP client based on the
 * "curl" command-line tool.
 *)


(*
 * definitions from HTTP/1.1 standard
 *)

let regexp cr = '\r'
let regexp lf = '\n'
let regexp sp = ' '  (* space *)
let regexp ht = '\t' (* horisontal tab *)
let regexp crlf = cr lf

let regexp digit = ['0'-'9']


(*
  CHAR           = <any US-ASCII character (octets 0 - 127)>

  CTL            = <any US-ASCII control character
                   (octets 0 - 31) and DEL (127)>

  LWS            = [CRLF] 1*( SP | HT ) ; liner white space

  TEXT           = <any OCTET except CTLs,
                   but including LWS>
*)
let regexp lws = crlf? (sp | ht)+
let regexp text = [^ 0-31 127] | lws


(*
  token          = 1*<any CHAR except CTLs or separators>
  separators     = "(" | ")" | "<" | ">" | "@"
                 | "," | ";" | ":" | "\" | <">
                 | "/" | "[" | "]" | "?" | "="
                 | "{" | "}" | SP | HT
*)
let regexp token = [^ "()<>@,;:\\\"/[]?={} \t" 0-31 127-255 ]+


(* HTTP message:
  generic-message = start-line
                    *(message-header CRLF)
                    CRLF
                    [ message-body ]
  start-line      = Request-Line | Status-Line

  Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF

  HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
  Status-Code    = 3DIGIT
  Reason-Phrase  = *<TEXT, excluding CR, LF>
*)

let regexp http_version = "HTTP" "/" digit+ "." digit+

let regexp status_code = digit digit digit

(* XXX: what does excluding CR, LF mean? *)
let regexp reason_phrase = [^ 0-31 127 '\r' '\n']*

let regexp status_line = http_version sp status_code sp reason_phrase crlf

let regexp status_line_head = http_version sp
let regexp status_line_tail = sp reason_phrase crlf


(* HTTP header:
  message-header = field-name ":" [ field-value ]
  field-name     = token
  field-value    = *( field-content | LWS )
  field-content  = <the OCTETs making up the field-value
                   and consisting of either *TEXT or combinations
                   of token, separators, and quoted-string>
*)

(* TODO: this regexp doesn't include quoted-sting case, only "*TEXT" *)
let regexp field_value = text*

let regexp message_header = token ":" field_value?

let regexp message_header_tail = ":" field_value?


(*
 * Parsing HTTP response
 *)

exception Error of string

let error s = raise (Error s)

let unexpected_eof () = error "unexpected EOF"
let invalid_character () = error "invalid character"


let parse_status_line_head = lexer
  | status_line_head -> ()
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()


let parse_status_code = lexer
  | status_code ->
      let s = Ulexing.latin1_lexeme lexbuf in
      int_of_string s
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()


let parse_status_line_tail = lexer
  | status_line_tail -> ()
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()


let parse_status_line lexbuf =
  try
    parse_status_line_head lexbuf;
    let code = parse_status_code lexbuf in
    parse_status_line_tail lexbuf;
    code
  with _ ->
    error "error parsing HTTP status line"


let skip_lexeme_ws lexbuf pos len =
  let rec aux pos =
    if pos = len (* end of lexeme *)
    then pos
    else
      let i = Ulexing.lexeme_char lexbuf pos in
      let c = Char.chr i in
      match c with
        | ' ' | '\t' -> aux (pos + 1) (* skip ws char *)
        | _ -> pos
  in
  aux pos


let parse_message_header_tail = lexer
  | message_header_tail crlf ->
      (* chop crlf at the end of the string (-2) *)
      let len = Ulexing.lexeme_length lexbuf - 2 in
      (* skip ':' character in front of the value *)
      let pos = 1 in
      (* skip whitespace in front of the value *)
      let pos = skip_lexeme_ws lexbuf pos len in
      let value = Ulexing.latin1_sub_lexeme lexbuf pos (len - pos) in
      value
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()


let rec parse_headers accu = lexer
  | token ->
      let field_name = Ulexing.latin1_lexeme lexbuf in
      let field_value = parse_message_header_tail lexbuf in
      let lowercase_name = String.lowercase field_name in
      parse_headers ((lowercase_name, field_value) :: accu) lexbuf

  | crlf ->
      List.rev accu

  | eof -> unexpected_eof ()
  | _ -> invalid_character ()


let parse_headers lexbuf =
  try
    let headers = parse_headers [] lexbuf in
    let body_offset = Ulexing.lexeme_end lexbuf in
    (headers, body_offset)
  with _ ->
    error "error parsing HTTP headers"


let parse_response_header buf len =
  let str =
    if String.length buf = len
    then buf
    else String.sub buf 0 len
  in
  let lexbuf = Ulexing.from_latin1_string str in
  let status_code = parse_status_line lexbuf in
  let headers, body_offset = parse_headers lexbuf in
  (status_code, headers, body_offset)


let read_buf_size = 8096
let read_buf = String.create read_buf_size


(* reading input in [read_buf_size] chunks *)
let read_next ch =
  Pervasives.input ch read_buf 0 read_buf_size


(* read the body unil eof *)
let rec read_body ch body_buf =
  let len = read_next ch in
  (* add a portion of input we've just read to the buffer *)
  Buffer.add_substring body_buf read_buf 0 len;
  if len < read_buf_size
  then () (* eof *)
  else read_body ch body_buf


let read_response ch =
  let len = read_next ch in
  if len = 0 then error "empty input";

  let status_code, headers, body_offset = parse_response_header read_buf len in

  (* create a buffer for reading HTTP message body *)
  let body_buf = Buffer.create read_buf_size in
  (* add a piece of previously read body to the body buffer *)
  Buffer.add_substring body_buf read_buf body_offset (len - body_offset);

  (* read the remainder of the body *)
  (* TODO, XXX: use "Content-Length" header? *)
  if len = read_buf_size
  then read_body ch body_buf;

  (* return the body as as a string *)
  let body = Buffer.contents body_buf in

  (status_code, headers, body)


let make_http_request ?body command =
  (* TODO: add tracing
  prerr_endline command;
  *)
  let send_body ch body =
    match body with
      | None -> ()
      | Some x ->
          output_string ch x;
          close_out ch
  in
  let ((in_channel, out_channel) as handle) = Unix.open_process command in
  send_body out_channel body;
  let res =
    try
      `ok (read_response in_channel)
    with
      Error x -> `error x
  in
  let status = Unix.close_process handle in
  match status, res with
    | Unix.WEXITED 0, `ok x -> x
    | Unix.WEXITED 0, `error x -> error x
    | Unix.WEXITED n, _ ->
        error ("curl exited with error code " ^ (string_of_int n))
    | Unix.WSIGNALED n, _ ->
        error ("curl was killed by signal " ^ (string_of_int n))
    | Unix.WSTOPPED n, _ ->
        error ("curl was stopped by signal " ^ (string_of_int n))


(*
  -i stands for output reponse http header in addition to payload data
  -s stands for silent
*)
(* XXX: add "User-Agent" header? *)
(* XXX: add the ability to specify custom curl options *)
let curl_command = "curl -i -s"


let get ?accept url =
  let accept_header =
    match accept with
      | None -> ""
      | Some x -> "-H 'Accept: " ^ x ^ "'"
  in
  let command =
    String.concat " " [
      curl_command; "-X GET";
      accept_header;
      "'" ^ url ^ "'"
    ]
  in
  make_http_request command


let post ?body ?content_type ?accept url =
  let content_type_header =
    match content_type with
      | None -> ""
      | Some _ when body = None -> ""
      | Some x -> "-H 'Content-Type: " ^ x ^ "'"
  in
  let accept_header =
    match accept with
      | None -> ""
      | Some x -> "-H 'Accept: " ^ x ^ "'"
  in
  let data_binary =
    match body with
      | None -> ""
      | Some x -> "--data-binary @-"
  in
  let command =
    String.concat " " [
      curl_command; "-X POST";
      content_type_header;
      accept_header;
      data_binary;
      "'" ^ url ^ "'"
    ]
  in
  make_http_request command ?body


let get_header name headers =
  try
    Some (List.assoc name headers)
  with Not_found -> None


(* some testing code:
let print_response response =
  let (status_code, headers, body) = response in
  Printf.printf "Status code: %d\n" status_code;
  Printf.printf "Headers:\n";
  List.iter (fun (n, v) -> Printf.printf "      %s: %s\n" n v) headers;
  Printf.printf "Length: %d\n" (String.length body);
  Printf.printf "Body:\n\n";
  print_endline body


let _ =
  let ch = open_in "t" in
  let res = read_response ch in
  print_response res;
  exit 0


let _ =
  let res = get Sys.argv.(1) in
  print_response res;
  exit 0


let _ =
  let res = post Sys.argv.(1) in
  print_response res;
  exit 0
*)

