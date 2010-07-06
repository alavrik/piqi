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

(* Runtime support for piqi/Protocol Buffers wire format encoding
 *
 * Encoding rules follow this specification:
 *
 *   http://code.google.com/apis/protocolbuffers/docs/encoding.html
 *)


module Buf =
  struct
    (* auxiliary iolist type and related primitives *)
    type t =
        Ios of string
      | Iol of t list
      | Iob of char


    let ios x = Ios x
    let iol l = Iol l
    let iob b = Iob b


    (* iolist buf output *)
    let to_buffer0 buf l =
      let rec aux = function
        | Ios s -> Buffer.add_string buf s
        | Iol l -> List.iter aux l
        | Iob b -> Buffer.add_char buf b
      in aux l


    (* iolist output size *)
    let size l =
      let rec aux = function
        | Ios s -> String.length s
        | Iol l -> List.fold_left (fun accu x -> accu + (aux x)) 0 l
        | Iob _ -> 1
      in aux l


    let to_string l =
      let buf = Buffer.create (size l) in
      to_buffer0 buf l;
      Buffer.contents buf


    let to_buffer l =
      let buf = Buffer.create 80 in
      to_buffer0 buf l;
      buf


    let to_channel ch code =
      let buf = to_buffer code in
      Buffer.output_buffer ch buf
  end


open Buf


let to_string = Buf.to_string
let to_buffer = Buf.to_buffer
let to_channel = Buf.to_channel


let iob i = (* IO char represented as Ios '_' *)
  iob (Char.chr i)


let gen_varint_value64 x =
  let rec aux x =
    let b = Int64.to_int (Int64.logand x 0x7FL) in (* base 128 *)
    let rem = Int64.shift_right_logical x 7 in
    (* Printf.printf "x: %LX, byte: %X, rem: %LX\n" x b rem; *)
    if rem = 0L
    then [iob b]
    else
      begin
        (* set msb indicating that more bytes will follow *)
        let b = b lor 0x80 in
        (iob b) :: (aux rem)
      end
  in iol (aux x)


let gen_unsigned_varint_value x =
  let rec aux x =
    let b = x land 0x7F in (* base 128 *)
    let rem = x lsr 7 in
    if rem = 0
    then [iob b]
    else
      begin
        (* set msb indicating that more bytes will follow *)
        let b = b lor 0x80 in
        (iob b) :: (aux rem)
      end
  in iol (aux x)


let gen_varint_value x =
  (* negative varints are encoded as bit-complement 64-bit varints, always
   * producing 10-bytes long value *)
  if x < 0
  then gen_varint_value64 (Int64.of_int x)
  else gen_unsigned_varint_value x


let gen_unsigned_varint_value32 x =
  let rec aux x =
    let b = Int32.to_int (Int32.logand x 0x7Fl) in (* base 128 *)
    let rem = Int32.shift_right_logical x 7 in
    if rem = 0l
    then [iob b]
    else
      begin
        (* set msb indicating that more bytes will follow *)
        let b = b lor 0x80 in
        (iob b) :: (aux rem)
      end
  in iol (aux x)


let gen_varint_value32 x =
  (* negative varints are encoded as bit-complement 64-bit varints, always
   * producing 10-bytes long value *)
  if Int32.logand x 0x8000_0000l <> 0l (* x < 0? *)
  then gen_varint_value64 (Int64.of_int32 x)
  else gen_unsigned_varint_value32 x


let gen_key ktype code =
  if code = -1 (* special code meaning that key sould not be generated *)
  then iol []
  else gen_unsigned_varint_value (ktype lor (code lsl 3))


let gen_varint code x =
  iol [
    gen_key 0 code;
    gen_varint_value x;
  ]

let gen_unsigned_varint code x =
  iol [
    gen_key 0 code;
    gen_unsigned_varint_value x;
  ]

let gen_varint32 code x =
  iol [
    gen_key 0 code;
    gen_varint_value32 x;
  ]

let gen_unsigned_varint32 code x =
  iol [
    gen_key 0 code;
    gen_unsigned_varint_value32 x;
  ]

let gen_varint64 code x =
  iol [
    gen_key 0 code;
    gen_varint_value64 x;
  ]


let gen_fixed32 code x = (* little-endian *)
  let s = String.create 4 in
  let x = ref x in
  for i = 0 to 3
  do
    let b = Char.chr (Int32.to_int (Int32.logand !x 0xFFl)) in
    s.[i] <- b;
    x := Int32.shift_right_logical !x 8
  done;
  iol [
    gen_key 5 code;
    ios s;
  ]


let gen_fixed64 code x = (* little-endian *)
  let s = String.create 8 in
  let x = ref x in
  for i = 0 to 7
  do
    let b = Char.chr (Int64.to_int (Int64.logand !x 0xFFL)) in
    s.[i] <- b;
    x := Int64.shift_right_logical !x 8
  done;
  iol [
    gen_key 1 code;
    ios s;
  ]


let int_to_varint code x =
  gen_varint code x

let int_to_zigzag_varint code x =
  (* encode signed integer using ZigZag encoding;
   * NOTE: using arithmetic right shift *)
  let x = (x lsl 1) lxor (x asr 62) in (* NOTE: can use lesser value than 62 on 32 bit? *)
  gen_unsigned_varint code x


let int64_to_varint code x =
  gen_varint64 code x

let int64_to_zigzag_varint code x =
  (* encode signed integer using ZigZag encoding;
   * NOTE: using arithmetic right shift *)
  let x = Int64.logxor (Int64.shift_left x 1) (Int64.shift_right x 63) in
  int64_to_varint code x

let int64_to_fixed64 code x =
  gen_fixed64 code x

let int64_to_fixed32 code x =
  gen_fixed32 code (Int64.to_int32 x)


let int32_to_varint code x =
  gen_varint32 code x

let int32_to_zigzag_varint code x =
  (* encode signed integer using ZigZag encoding;
   * NOTE: using arithmetic right shift *)
  let x = Int32.logxor (Int32.shift_left x 1) (Int32.shift_right x 31) in
  gen_unsigned_varint32 code x


let int32_to_fixed32 code x =
  gen_fixed32 code x

let int32_to_fixed64 code x =
  gen_fixed64 code (Int64.of_int32 x)


let int_to_fixed32 code x =
  gen_fixed32 code (Int32.of_int x)

let int_to_fixed64 code x =
  gen_fixed64 code (Int64.of_int x)


let int32_to_signed_fixed32 = int32_to_fixed32
let int64_to_signed_fixed64 = int64_to_fixed64
let int32_to_signed_fixed64 = int32_to_fixed64
let int64_to_signed_fixed32 = int64_to_fixed32

let int_to_signed_varint = int_to_varint
let int32_to_signed_varint = int32_to_varint
let int64_to_signed_varint = int64_to_varint


let float_to_fixed32 code x =
  (* XXX *)
  gen_fixed32 code (Int32.bits_of_float x)

let float_to_fixed64 code x =
  (* XXX *)
  gen_fixed64 code (Int64.bits_of_float x)

(* let gen_float = float_to_fixed64 *)


let bool_to_varint code = function
  | true -> gen_unsigned_varint code 1
  | false -> gen_unsigned_varint code 0

let gen_bool = bool_to_varint 


let gen_string code s = 
  (* special code meaning that key and length sould not be generated *)
  let contents = ios s in
  if code = -1
  then contents
  else
    iol [
      gen_key 2 code;
      gen_unsigned_varint_value (String.length s);
      contents;
    ]


let string_to_block = gen_string
let binary_to_block = gen_string (* binaries use the same encoding as strings *)
let word_to_block = gen_string (* word is encoded as string *)
let text_to_block = gen_string (* text is encoded as string *)


let gen_req_field code f x = f code x


let gen_opt_field code f = function
  | Some x -> f code x
  | None -> Iol []


let gen_rep_field code f l =
  iol (List.map (fun x -> f code x) l)


let gen_record code contents =
  let contents = iol contents in
  (* special code meaning that key and length sould not be generated *)
  if code = -1
  then contents
  else
    iol [
      gen_key 2 code;
      (* the length of consequent data *)
      gen_unsigned_varint_value (Buf.size contents);
      contents;
    ]


(* generate binary representation of <type>_list .proto structure *)
let gen_list f code l =
  (* NOTE: using "1" as list element code *)
  let contents = List.map (f 1) l in
  gen_record code contents


let gen_binobj gen_obj ?name x =
  let obj = gen_obj 2 x in
  let l =
    match name with
      | Some name -> iol [ gen_string 1 name; obj ]
      | None -> obj
  in
  (* return the rusult encoded as a binary string *)
  Buf.to_string l
