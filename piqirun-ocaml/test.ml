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

open Piqirun


(* superposition operator *)
let ( ** ) f g x = f (g x)


let test_parse x =
  (IBuf.of_string ** OBuf.to_string) x


let test_parse_varint x =
  (parse_varint ** test_parse) x


let assert_eq a b = assert (a = b)


let test_int x =
  assert (x = (int_of_varint ** test_parse_varint ** gen_varint_value) x)

let test_int32 x =
  assert (x = (int32_of_varint ** test_parse_varint ** gen_varint32_value) x)

let test_int64 x =
  (*
  assert_eq x ((int64_of_varint ** test_parse_varint ** gen_varint_value64) x)
  *)
  assert (x = (int64_of_varint ** test_parse_varint ** gen_varint64_value) x)

let test_key x =
  assert (x = ((fun x -> let _, code = parse_field_header x in code) ** test_parse ** gen_key 0) x)


let test_gen_parse x =
  (init_from_string ** to_string) x


let test_zigzag_int x =
  assert (x = (int_of_zigzag_varint ** test_gen_parse ** int_to_zigzag_varint (-1)) x)

let test_zigzag_int32 x =
  assert (x = (int32_of_zigzag_varint ** test_gen_parse ** int32_to_zigzag_varint (-1)) x)

let test_zigzag_int64 x =
  assert (x = (int64_of_zigzag_varint ** test_gen_parse ** int64_to_zigzag_varint (-1)) x)


let test_fixed_int32 x =
  assert (x = (int32_of_fixed32 ** test_gen_parse ** int32_to_fixed32 (-1)) x)

let test_fixed_int64 x =
  assert (x = (int64_of_fixed64 ** test_gen_parse ** int64_to_fixed64 (-1)) x)




let int_input =
  [
    0; 1; 2; 3; -1; -2; -3;
    min_int; min_int + 1; min_int + 2; min_int + 3;
    max_int; max_int - 1; max_int - 2; max_int - 3;
  ]


open Int32

let int32_input =
  let int_intput = List.map (fun x -> of_int x) int_input in
  int_intput @
  [
    min_int; succ min_int; succ (succ min_int); succ (succ (succ min_int));
    max_int; pred max_int; pred (pred max_int); pred (pred (pred max_int));
  ]


open Int64

let int64_input =
  let int32_intput = List.map (fun x -> of_int32 x) int32_input in
  int32_intput @
  [
    min_int; succ min_int; succ (succ min_int); succ (succ (succ min_int));
    max_int; pred max_int; pred (pred max_int); pred (pred (pred max_int));
  ]


let max_key = (1 lsl 29) - 1

let key_input = [ 1; 2; 3; max_key - 1; max_key ]


(* TODO:
 * tests for malformed/broken/unexpectedly terminated input
 * tests for OCaml's type overflows
 * tests for cross-type reading, e.g. int32 -> int64, varint -> int64, etc.
 *)

let test _ =
  List.iter test_int int_input;
  List.iter test_int32 int32_input;
  List.iter test_int64 int64_input;

  List.iter test_zigzag_int int_input;
  List.iter test_zigzag_int32 int32_input;
  List.iter test_zigzag_int64 int64_input;

  List.iter test_fixed_int32 int32_input;
  List.iter test_fixed_int64 int64_input;

  List.iter test_key key_input;
  ()


let _ =
  if !Sys.interactive
  then ()
  else test ()

