(*pp camlp4o -I $PIQI_ROOT/camlp4 pa_labelscope.cmo pa_openin.cmo *)
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


(*
 * check and assign wire codes for fields and options
 *)

(*

From Protobuf Language Guide
(http://code.google.com/apis/protocolbuffers/docs/proto.html)

The smallest tag number you can specify is 1, and the largest is 2^29 - 1, or
536,870,911. You also cannot use the numbers 19000 though 19999
(FieldDescriptor::kFirstReservedNumber through
FieldDescriptor::kLastReservedNumber), as they are reserved for the Protocol
Buffers implementation - the protocol buffer compiler will complain if you use
one of these reserved numbers in your .proto. 

Enumerator constants must be in the range of a 32-bit integer. Since enum
values use varint encoding on the wire, negative values are inefficient and
thus not recommended.

*)

open Piqi_common


let default_wire_type (t:T.piqtype) =
  match t with
    | `int -> `zigzag_varint
    | `float -> `fixed64
    | `bool -> `varint
    | _ -> `block


let wire_type_name (wt:T.wire_type) = 
  match wt with
    | `varint -> "varint"
    | `zigzag_varint -> "zigzag_varint"
    | `fixed32 -> "fixed32"
    | `fixed64 -> "fixed64"
    | `signed_varint -> "signed_varint"
    | `signed_fixed32 -> "signed_fixed32"
    | `signed_fixed64 -> "signed_fixed64"
    | `block -> "block"


let get_wire_type (t:T.piqtype) (wt:T.wire_type option) =
  match wt with
    | None -> default_wire_type t
    | Some wt -> wt


let get_wire_type_name t wt =
  let wt = get_wire_type t wt in
  wire_type_name wt


(*
 * add wire codes if not specified by user
 *) 

let do_resolve = ref true


let invalid_max_code = Int32.shift_left 1l 29 (* 2 ^ 29 *)


let incr i =
  i := Int32.succ !i;
  if !i = 19000l
  then i := 20000l
  else
    if !i = invalid_max_code
    then
      piqi_error "auto-generatated wire code exceeds allowed range (1..2^29-1)"
    else ()


let addcodes_field code f =
  let open T.Field in
  match f.code with
    | None -> (* assign previously unassigned code *)
        f.code <- Some !code; incr code
    | Some _ -> assert false


let addcodes_option code o =
  let open T.Option in
  match o.code with
    | None -> (* assign previously unassigned code *)
        o.code <- Some !code; incr code
    | Some _ -> assert false


let addcodes_enum_option code o =
  let open T.Option in
  match o.code with
    | None -> (* assign previously unassigned code *)
        o.code <- Some !code; code := Int32.succ !code
    | Some _ -> assert false


let check_code i =
  let (<) a b = Int32.compare a b < 0 in
  let (>=) a b = Int32.compare a b >= 0 in
  if i < 1l || i >= invalid_max_code || (i >= 19000l && i < 20000l)
  then error i "wire code is out of allowed range"


let check_codes codes =
  List.iter check_code codes;
  match find_dups codes with
    | None -> ()
    | Some code ->
        error code ("duplicate wire code: " ^ Int32.to_string code)


let check_enum_codes codes =
  match find_dups codes with
    | None -> ()
    | Some code ->
        warning code ("duplicate enum wire code: " ^ Int32.to_string code)


let addcodes_record r =
  let open T.Record in
  let fields = r.field in
  if List.exists (fun x -> x.T.Field#code <> None) fields
  then
    begin
      if List.exists (fun x -> x.T.Field#code = None) fields
      then error r "codes must be defined for either all or none fields"
      else
        (* all field codes are assigned *)
        let codes = List.map (fun x -> some_of x.T.Field#code) fields in
        check_codes codes
    end
  else 
    if !do_resolve
    then
      let code = ref 1l in (* assign codes *)
      List.iter (addcodes_field code) fields
    else ()


let addcodes_variant v =
  let open T.Variant in
  let options = v.option in
  if List.exists (fun x -> x.T.Option#code <> None) options
  then
    begin
      if List.exists (fun x -> x.T.Option#code = None) options
      then error v "codes must be defined for either all or none variant options"
      else
        (* all option codes are assigned *)
        let codes = List.map (fun x -> some_of x.T.Option#code) options in
        check_codes codes
    end
  else 
    if !do_resolve
    then
      let code = ref 1l in (* assign codes *)
      List.iter (addcodes_option code) options
    else ()


let addcodes_enum v =
  let open T.Variant in
  let options = v.option in
  if List.exists (fun x -> x.T.Option#code <> None) options
  then
    begin
      if List.exists (fun x -> x.T.Option#code = None) options
      then error v "codes must be defined for either all or none enum options"
      else
        (* all option codes are assigned *)
        let codes = List.map (fun x -> some_of x.T.Option#code) options in
        check_enum_codes codes
    end
  else 
    if !do_resolve
    then
      (* XXX: assign enum constant values starting from 0? *)
      let code = ref 1l in (* assign codes *)
      List.iter (addcodes_enum_option code) options
    else ()


let addcodes_def = function
  | `record x -> addcodes_record x
  | `variant x -> addcodes_variant x
  | `enum x -> addcodes_enum x
  | _ -> ()


let add_codes (defs: T.piqdef list) =
  List.iter addcodes_def defs


let check_codes defs =
  do_resolve := false;
  add_codes defs;
  do_resolve := true;
  ()

