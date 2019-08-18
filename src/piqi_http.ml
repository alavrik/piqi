(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2017, 2018 Anton Lavrik

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

let cr = [%sedlex.regexp? '\r']
let lf = [%sedlex.regexp? '\n']
let sp = [%sedlex.regexp? ' ' ]  (* space *)
let ht = [%sedlex.regexp? '\t']  (* horisontal tab *)
let crlf = [%sedlex.regexp? cr, lf]

let digit = [%sedlex.regexp? '0'..'9']


(*
  CHAR           = <any US-ASCII character (octets 0 - 127)>

  CTL            = <any US-ASCII control character
                   (octets 0 - 31) and DEL (127)>

  LWS            = [CRLF] 1*( SP | HT ) ; liner white space

  TEXT           = <any OCTET except CTLs,
                   but including LWS>
*)
let lws = [%sedlex.regexp? Opt crlf, Plus (sp | ht)]
let text = [%sedlex.regexp? Compl (0 .. 31 | 127) | lws]


(*
  token          = 1*<any CHAR except CTLs or separators>
  separators     = "(" | ")" | "<" | ">" | "@"
                 | "," | ";" | ":" | "\" | <">
                 | "/" | "[" | "]" | "?" | "="
                 | "{" | "}" | SP | HT
*)
let token = [%sedlex.regexp? Plus (Compl ((Chars "()<>@,;:\\\"/[]?={} \t") | 0 .. 31 | 127 .. 255))]


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

let http_version = [%sedlex.regexp? "HTTP", "/", Plus digit, ".", Plus digit]

let status_code = [%sedlex.regexp? digit, digit, digit]

(* XXX: what does excluding CR, LF mean? *)
let reason_phrase = [%sedlex.regexp? Star (Compl (0 .. 31 | 127 | '\r' | '\n'))]

let status_line = [%sedlex.regexp? http_version, sp, status_code, sp, reason_phrase, crlf]

let status_line_head = [%sedlex.regexp? http_version, sp]
let status_line_tail = [%sedlex.regexp? sp, reason_phrase, crlf]


(* HTTP header:
  message-header = field-name ":" [ field-value ]
  field-name     = token
  field-value    = *( field-content | LWS )
  field-content  = <the OCTETs making up the field-value
                   and consisting of either *TEXT or combinations
                   of token, separators, and quoted-string>
*)

(* NOTE, XXX: this regexp doesn't include quoted-string case, only "*TEXT" *)
let field_value = [%sedlex.regexp? Star text]

let message_header = [%sedlex.regexp? token, ":", Opt field_value]

let message_header_tail = [%sedlex.regexp? ":", Opt field_value]


(*
 * Parsing HTTP response
 *)

exception Error of string

let error s = raise (Error s)

let unexpected_eof () = error "unexpected EOF"
let invalid_character () = error "invalid character"


let handle_exn context exn =
  let s =
    match exn with
      | Error x -> x
      | _ -> Piqi_common.string_of_exn exn
  in error (context ^ ": " ^ s)


let parse_status_line_head lexbuf = [%sedlex match lexbuf with
  | status_line_head -> ()
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()
]


let parse_status_code lexbuf = [%sedlex match lexbuf with
  | status_code ->
      let s = Sedlexing.Latin1.lexeme lexbuf in
      int_of_string s
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()
]


let parse_status_line_tail lexbuf = [%sedlex match lexbuf with
  | status_line_tail -> ()
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()
]


let parse_status_line lexbuf =
  try
    parse_status_line_head lexbuf;
    let code = parse_status_code lexbuf in
    parse_status_line_tail lexbuf;
    code
  with exn ->
    handle_exn "error parsing HTTP status line" exn


let skip_lexeme_ws lexbuf pos len =
  let rec aux pos =
    if pos = len (* end of lexeme *)
    then pos
    else
      let c = Sedlexing.lexeme_char lexbuf pos in
      let is_ws_char =
        let code = Uchar.to_int c in
        code = Char.code ' ' || code = Char.code '\t'
      in
      if is_ws_char
      then aux (pos + 1)  (* skip ws char *)
      else pos
  in
  aux pos


let parse_message_header_tail lexbuf = [%sedlex match lexbuf with
  | message_header_tail, crlf ->
      (* chop crlf at the end of the string (-2) *)
      let len = Sedlexing.lexeme_length lexbuf - 2 in
      (* skip ':' character in front of the value *)
      let pos = 1 in
      (* skip whitespace in front of the value *)
      let pos = skip_lexeme_ws lexbuf pos len in
      let value = Sedlexing.Latin1.sub_lexeme lexbuf pos (len - pos) in
      value
  | eof -> unexpected_eof ()
  | _ -> invalid_character ()
]


let rec parse_headers accu lexbuf = [%sedlex match lexbuf with
  | token ->
      let field_name = Sedlexing.Latin1.lexeme lexbuf in
      let field_value = parse_message_header_tail lexbuf in
      (* NOTE: we'll have to live with the compilation warning about
       * String.lowercase being deprecated: can't use String.lowercase_ascii
       * because it is not supported in OCaml 4.02, it was added in 4.03 *)
      let lowercase_name = String.lowercase field_name in
      parse_headers ((lowercase_name, field_value) :: accu) lexbuf

  | crlf ->
      List.rev accu

  | eof -> unexpected_eof ()
  | _ -> invalid_character ()
]


let parse_headers lexbuf =
  try
    let headers = parse_headers [] lexbuf in
    let body_offset = Sedlexing.lexeme_end lexbuf in
    (headers, body_offset)
  with exn ->
    handle_exn "error parsing HTTP headers" exn


let parse_response_header buf len =
  let str =
    if String.length buf = len
    then buf
    else String.sub buf 0 len
  in
  let lexbuf = Sedlexing.Latin1.from_string str in
  let status_code = parse_status_line lexbuf in
  let headers, body_offset = parse_headers lexbuf in
  (status_code, headers, body_offset)


let read_buf_size = 8096
let read_buf = Bytes.create read_buf_size


(* reading input in [read_buf_size] chunks *)
let read_next ch =
  Pervasives.input ch read_buf 0 read_buf_size


(* read the body unil eof *)
let read_body ch body_buf =
  let rec aux () =
    let len = read_next ch in
    if len = 0
    then () (* eof *)
    else (
      (* add a portion of input we've just read to the buffer *)
      Buffer.add_substring body_buf (Bytes.unsafe_to_string read_buf) 0 len;
      aux ()
    )
  in aux ()


let read_response ch =
  let len = read_next ch in
  if len = 0 then error "response is empty";

  let status_code, headers, body_offset = parse_response_header (Bytes.unsafe_to_string read_buf) len in

  (* create a buffer for reading HTTP message body *)
  let body_buf = Buffer.create read_buf_size in
  (* add a piece of previously read body to the body buffer *)
  Buffer.add_substring body_buf (Bytes.unsafe_to_string read_buf) body_offset (len - body_offset);

  (* read the remainder of the body *)
  (* XXX: use "Content-Length" header? *)
  read_body ch body_buf;

  (* return the body as as a string *)
  let body = Buffer.contents body_buf in

  (status_code, headers, body)


let make_http_request ?body command =
  Piqi_common.trace "piqi_http: running curl: %s\n" command;
  let ((in_channel, out_channel) as handle) = Unix.open_process command in

  (match body with
    | None -> ()
    | Some x ->
        output_string out_channel x;
        close_out out_channel
  );

  let res =
    try
      `ok (read_response in_channel)
    with exn ->
      `error exn
  in
  let status = Unix.close_process handle in
  match status, res with
    | Unix.WEXITED 0, `ok x -> x
    | Unix.WEXITED 0, `error exn -> raise exn
    | Unix.WEXITED 127, _ ->
        error ("shell command couldn't be executed: " ^ command)
    | Unix.WEXITED n, _ ->
        error ("curl exited with error code " ^ (string_of_int n))
    | Unix.WSIGNALED n, _ ->
        error ("curl was killed by signal " ^ (string_of_int n))
    | Unix.WSTOPPED n, _ ->
        error ("curl was stopped by signal " ^ (string_of_int n))


(* XXX: add the ability to specify custom curl options *)
let make_curl_command
    ?accept
    ?content_type
    ?user_agent
    ?body
    http_method url =

  let accept_header =
    match accept with
      | None -> ""
      | Some x -> "-H 'Accept: " ^ x ^ "'"
  in
  let content_type_header =
    match content_type with
      | None -> ""
      | Some _ when body = None -> ""
      | Some x -> "-H 'Content-Type: " ^ x ^ "'"
  in
  let user_agent_header =
    match user_agent with
      | None -> ""
      | Some x -> "-A '" ^ x ^ "'"
  in
  let data_binary =
    match body with
      | None -> ""
      | Some x -> "--data-binary @-"
  in
  (*
    -i stands for output response http header in addition to payload data
    -s stands for silent
  *)
  String.concat " " [
    "curl -i -s -X"; http_method;
    accept_header;
    content_type_header;
    user_agent_header;
    data_binary;
    "'" ^ url ^ "'"
  ]


let get ?accept ?user_agent url =
  let command =
    make_curl_command ?accept ?user_agent "GET" url in
  make_http_request command


let post ?body ?content_type ?accept ?user_agent url =
  let command =
    make_curl_command ?accept ?content_type ?user_agent ?body "POST" url
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

