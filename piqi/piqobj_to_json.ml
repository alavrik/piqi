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

open Piqi_json_common


module C = Piqi_common
open C


module R = Piqobj.Record
module F = Piqobj.Field
module V = Piqobj.Variant
module E = Piqobj.Variant
module O = Piqobj.Option
module A = Piqobj.Alias
module Any = Piqobj.Any
module L = Piqobj.List


let is_ast_def = function
  | `variant {C.V.name = "ast"} -> true
  | _ -> false

let ast_def = Piqi._find_def is_ast_def


let make_named name value =
  name, value


let make_name name =
  name, `Null ()


let rec gen_obj (x:Piqobj.obj) :json =
  match x with
    (* built-in types *)
    | `int x -> `Int x
    | `uint x -> `Uint x
    | `float x -> `Float x
    | `bool x -> `Bool x
    | `string x -> `String x
    | `binary x -> `String (Base64.encode x)
    | `word x -> `String x
    | `text x -> `String x
    | `any x -> gen_any x
    (* custom types *)
    | `record x -> gen_record x
    | `variant x -> gen_variant x
    | `enum x -> gen_enum x
    | `list x -> gen_list x
    | `alias x -> gen_alias x


and gen_typed_obj x =
  let name = Piqobj_common.full_typename x in
  let json = gen_obj x in
  `Assoc [
    "_piqtype", `String name;
    "_piqobj", json;
  ]


and gen_any x =
  open Any in
  (* XXX: is ast always defined? *)

  (* TODO: handle typed *)
  let ast = some_of x.any.T.Any.ast in
  (* convert ast to binary *)
  let binobj = Piqirun_gen.gen_binobj T.gen_ast ast in
  (* convert binary ast to piqobj of type ast *)
  let piqtype = ast_def in
  let piqobj = Piqobj_of_wire.parse_binobj ~piqtype binobj in
  (* generate json from the piqobj *)
  gen_obj piqobj


and gen_record x =
  open R in
  let field_types = x.piqtype.T.Record.field in
  `Assoc (List.map (gen_field x.field) field_types)


and gen_field fields t =
  open T.Field in
  let name = some_of t.json_name in
  open F in
  let pred f = f.piqtype == t in
  match t.mode with
    | `required | `optional ->
        (try
          let f = List.find pred fields in
          (match f.obj with
             | None -> make_name name (* flag *)
             | Some obj -> make_named name (gen_obj obj)
          )
        with
          Not_found -> make_name name (* optional *) )
    | `repeated ->
        let fields = List.find_all pred fields in
        let json_fields = List.map (fun f -> gen_obj (some_of f.obj)) fields in
        make_named name (`List json_fields)


and gen_variant x =
  open V in
  let o = gen_option x.option in
  `Assoc [o]


and gen_option x =
  open O in
  let name = some_of x.piqtype.T.Option.json_name in
  match x.obj with
    | None -> make_name name
    | Some obj -> make_named name (gen_obj obj)


and gen_enum x =
  open V in
  gen_enum_option x.option


and gen_enum_option x =
  open O in
  let name = some_of x.piqtype.T.Option.json_name in
  `String name


and gen_list x = 
  open L in
  `List (List.map gen_obj x.obj)


and gen_alias x =
  open A in
  match x.obj with
    | `alias x -> gen_alias x
    | x -> gen_obj x

