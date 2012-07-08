(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011, 2012 Anton Lavrik

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

open Piqobj_common


(* whether to generate piqi-any as :typed or leave it as piqi-any (this depends
 * on whether we are going to pretty-print the ast or not)
 *)
let is_external_mode = ref false


(* NOTE: loosing precision here, in future we will support encoding floats as
 * string literals containing binary representation of 64-bit IEEE float *)
let gen_float x = `float x


let is_ascii_string s =
  let len = String.length s in
  let rec aux i =
    if i >= len
    then true
    else
      if Char.code s.[i] <= 127
      then aux (i + 1)
      else false
  in
  aux 0


let gen_string ?piq_format s =
  match piq_format with
   | Some `text ->
       `text s
   | Some `word when Piq_lexer.is_valid_word s ->
       `word s
   | _ ->
      if is_ascii_string s
      then
        `ascii_string s
      else
        `utf8_string s


let gen_binary s =
  if is_ascii_string s
  then
    `ascii_string s
  else
    `binary s


let make_named name value =
  `named Piq_ast.Named#{name = name; value = value}


let make_name name =
  `name name


let make_typed typename ast :piq_ast =
  let res = Piq_ast.Typed#{typename = typename; value = ast} in
  Piqloc.addref ast res;
  `typed res


(* (re-)order fields according to their positions in the original piqi spec *)
let order_record_fields t piqobj_fields =
  let find_fields ft l =
    List.partition (fun x -> x.F.t == ft) l
  in
  let res, _rem =
    List.fold_left
      (fun (accu, rem) x -> (* folder *)
        let res, rem' = find_fields x rem in
        (List.rev_append res accu, rem'))

      ([], piqobj_fields) (* accu *)

      t.T.Record#field (* list to fold *)
  in
  List.rev res


let rec gen_obj0 ?(piq_format: T.piq_format option) (x:Piqobj.obj) :piq_ast =
  match x with
    (* built-in types *)
    | `int x -> `int x
    | `uint x -> `uint x
    | `float x -> gen_float x
    | `bool x -> `bool x
    | `string x -> gen_string x ?piq_format
    | `binary x -> gen_binary x
    | `any x -> gen_any x
    (* custom types *)
    | `record x -> gen_record x
    | `variant x -> gen_variant x
    | `enum x -> gen_enum x
    | `list x -> gen_list x
    | `alias x -> gen_alias x ?piq_format


(* TODO: provide more precise locations for fields, options, etc *)
and gen_obj ?piq_format x =
  let res = gen_obj0 x ?piq_format in
  match res with
    | `piqi_any any ->
        Piqloc.addrefret x res
    | _ ->
        Piq_parser.piq_addrefret x res


and gen_typed_obj x =
  let name = Piqobj_common.full_typename x in
  `typed Piq_ast.Typed#{typename = name; value = gen_obj x}


(* TODO: optimize by storing piqobj in T.Any *)
and ast_of_any any :piq_ast =
  match any with
    | {T.Any.cached_piq_ast = Some ast} -> ast
    | {T.Any.piq_ast_ref = Some piq_ast_ref} ->
        let ast = Piqi_objstore.get piq_ast_ref in
        any.T.Any#cached_piq_ast <- Some ast; (* cache the result *)
        ast
    | {T.Any.typename = Some typename; binobj = Some binobj} ->
        (* generate ast representation from the binary object if the type is known
         *)
        (match Piqi_db.try_find_piqtype typename with
          | Some t ->
              Piqloc.pause ();
              let obj = Piqobj_of_wire.parse_binobj t binobj in
              let ast = gen_obj obj in
              Piqloc.resume ();
              any.T.Any#cached_piq_ast <- Some ast; (* cache the result *)
              ast
          | None ->
              assert false
        )
    | _ ->
        assert false


and gen_any x =
  let open Any in
  if not !is_external_mode
  then
    (* FIXME: memory leak by allocating elements in objstore and never freeing
     * them *)
    (* for internal use, just passing it around as is *)
    `piqi_any (Piqi_objstore.put x.any)
  else
    let ast = ast_of_any x.any in
    match x.any.T.Any#typename with
      | Some typename ->
          make_typed typename ast
      | None ->
          ast


and gen_record x =
  let open R in
  (* TODO, XXX: doing ordering at every generation step is inefficient *)
  let fields = order_record_fields x.t x.field in
  `list (List.map gen_field fields)


and gen_field x =
  let open F in
  let name = name_of_field x.t in
  let res =
    match x.obj with
      | None ->
          make_name name
      | Some obj ->
          make_named name (gen_obj obj ?piq_format:x.t.T.Field#piq_format)
  in
  Piq_parser.piq_addrefret x res


and gen_variant x =
  let open V in
  gen_option x.option


and gen_option x =
  let open O in
  let name = name_of_option x.t in
  let res =
    match x.obj with
      | None -> make_name name
      | Some obj -> make_named name (gen_obj obj ?piq_format:x.t.T.Option#piq_format)
  in Piq_parser.piq_addrefret x res


and gen_enum x =
  let open E in
  gen_option x.option


and gen_list x = 
  let open L in
  `list (List.map (fun obj -> gen_obj obj ?piq_format:x.t.T.Piqi_list#piq_format) x.obj)


and gen_alias ?(piq_format: T.piq_format option) x =
  let open A in
  (* upper-level setting overrides lower-level setting *)
  let this_piq_format = x.t.T.Alias#piq_format in
  let piq_format =
    if this_piq_format <> None
    then this_piq_format
    else piq_format
  in
  match x.obj with
    | `alias x ->
        gen_alias x ?piq_format
    | x ->
        gen_obj x ?piq_format

