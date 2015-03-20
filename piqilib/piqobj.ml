(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

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

module rec Piqobj:
  sig
    type ('a, 'b) field = ('a, 'b) Field.t
    type ('a, 'b) option = ('a, 'b) Option.t
    type ('a, 'b) record = ('a, 'b) Record.t
    type ('a, 'b) variant = ('a, 'b) Variant.t
    type ('a, 'b) enum = ('a, 'b) Enum.t
    type ('a, 'b) alias =  ('a, 'b) Alias.t
    type 'a list = 'a Core.Std.List.t

    type ('a, 'b) any = ('a, 'b) Any.t

    type ('a, 'b) typedef =
      [ `record of ('a, 'b) record
      | `variant of ('a, 'b) variant
      | `enum of ('a, 'b) enum
      | `alias of ('a, 'b) alias
      | `list of ('a, 'b) List.t ]

    type ('a, 'b) obj =
      [ ('a, 'b) typedef
        (* built-in types *)
      | `int of int64 (* XXX: use big_int for internal representation? *)
      | `uint of int64
      | `float of float
      | `bool of bool
      | `string of string
      | `binary of string

      | `any of ('a, 'b) any ]
  end = Piqobj

and Record:
  sig
    type ('a, 'b) t =
      {
        mutable t : Piqi_impl_piqi.record;
        mutable field : ('a, 'b) Piqobj.field list;
        mutable unparsed_piq_fields_ref : int option;
      }
  end = Record

and Field:
  sig
    type ('a, 'b) t =
      {
        mutable t : Piqi_impl_piqi.field;
        mutable obj: ('a, 'b) Piqobj.obj option;
      }
  end = Field

and Variant:
  sig
    type ('a, 'b) t =
      {
        mutable t : Piqi_impl_piqi.variant;
        mutable option : ('a, 'b) Piqobj.option;
      }
  end = Variant

and Enum:
  sig
    type ('a, 'b) t =
      {
        mutable t : Piqi_impl_piqi.enum;
        mutable option : ('a, 'b) Piqobj.option;
      }
  end = Enum

and Option:
  sig
    type ('a, 'b) t =
      {
        mutable t : Piqi_impl_piqi.option;
        mutable obj: ('a, 'b) Piqobj.obj option; (* None for named options, i.e. constants *)
      }
  end = Option

and List:
  sig
    type ('a, 'b) t =
      {
        mutable t : Piqi_impl_piqi.piqi_list;
        mutable obj: ('a, 'b) Piqobj.obj list;
      }
  end = List

and Alias:
  sig
    type ('a, 'b ) t =
      {
        mutable t : Piqi_impl_piqi.alias;
        mutable obj: ('a, 'b) Piqobj.obj;
      }
  end = Alias

and Any:
  sig
    type ('a, 'b) t =
      {
        (* type of the object, when the type is known (NOTE: object can be
         * untyped *)
        mutable typename : string option;

        (* internal representation of a typed data object *)
        mutable obj: (('a, 'b) Piqobj.obj) option;

        (* external representation in various formats *)
        mutable pb : string option; (* protocol buffers binary *)
        mutable piq_ast : Piq_ast.ast option;
        (* the original embedded json reprsented as a string *)
        mutable json_string : string option;
        mutable json_ast : Piqi_json_type.json option;
        mutable xml_ast : Piqi_xml_type.xml_elem option;

        (* unique reference to self in Piqi_objstore *)
        mutable ref: int option;
      }
  end = Any


module C = Piqi_common


let default_any () =
  Any.({
    obj = None;
    typename = None;
    pb = None;
    piq_ast = None;
    json_string = None;
    json_ast = None;
    xml_ast = None;
    ref = None;
  })


(* store Piqobj.any and return reference of the stored object in Piqi_objstore
 *)
let put_any (any: ('a, 'b) Piqobj.any) :int =
  let open Any in
  match any.ref with
    | Some ref -> ref
    | None ->
        (* FIXME: memory leak by allocating elements in objstore and never
         * freeing them *)
        let ref = Piqi_objstore.put any in
        any.ref <- Some ref;
        C.debug "Piqobj.put_any: with ref %d\n" ref;
        ref


(* find Piqobj.any by reference in Piqi_objstore *)
let get_any (ref: int) : ('a, 'b) Piqobj.any =
  Piqi_objstore.get ref


let any_of_piqi_any (piqi_any: Piqi_impl_piqi.any) : ('a, 'b) Piqobj.any =
  match piqi_any.Piqi_impl_piqi.Any.ref with
    | Some ref ->
        (* recover internally passed Piqobj.any from an integer reference *)
        C.debug "Piqobj.any_of_piqi_any: recovering any from existing ref %d\n" ref;
        get_any ref
    | None ->
        (* NOTE: this branch is used when the function is called from
         * Piqi.resolve_field_default when Piqi is read from Protobuf during
         * Piqi.boot *)
        let any = Any.({
          (default_any ()) with
          typename = piqi_any.Piqi_impl_piqi.Any.typename;
          pb = piqi_any.Piqi_impl_piqi.Any.protobuf;
        }) in
        (* cache the value in objstore and in the piqi_any itself *)
        let ref = put_any any in
        C.debug "Piqobj.any_of_piqi_any: creating new any with ref %d\n" ref;
        piqi_any.Piqi_impl_piqi.Any.ref <- Some ref;
        any


(* these functions will be properly set by piqobj_to* modules *)
let to_pb   obj  = assert false
let to_piq  obj  = assert false
let to_json obj  = assert false
let to_xml  obj  = assert false

let of_pb   piqtype (x :string) = assert false
let of_piq  piqtype (x :Piq_ast.ast) = assert false
let of_json piqtype (x :Piqi_json_type.json) = assert false
let of_xml  piqtype (x :Piqi_xml_type.xml_elem) = assert false

let to_pb    = ref Piqobj_to_protobuf.gen_binobj
let to_piq   = ref to_piq
let to_json  = ref to_json
let to_xml   = ref to_xml
             
let of_pb    = ref of_pb
let of_piq   = ref of_piq
let of_json  = ref of_json
let of_xml   = ref of_xml


(* these function will be set by correspondent piqi_json* and piqi_xml* modules;
 * they are used for unptyped json
 * TODO: find a better module for these functions *)
let json_of_string (x:string) :Piqi_json_type.json = assert false
let xml_of_string  (x:string) :Piqi_xml_type.xml list = assert false

let string_of_json (x :Piqi_json_type.json) :string = assert false
let string_of_xml  (x :Piqi_xml_type.xml) :string = assert false

let json_of_string = ref json_of_string
let xml_of_string  = ref xml_of_string
let string_of_json = ref string_of_json
let string_of_xml  = ref string_of_xml


let of_any (piqtype: Piqi_impl_piqi.piqtype) (any :('a, 'b) Piqobj.any) : ('a, 'b) Piqobj.obj option =
  let open Any in
  if any.pb <> None (* try parsing from Protobuf *)
  then
    let obj = !of_pb piqtype (C.some_of any.pb) in
    Some obj
  else if any.piq_ast <> None
  then
    let obj = !of_piq piqtype (C.some_of any.piq_ast) in
    Some obj
  else if any.json_ast <> None
  then
    let obj = !of_json piqtype (C.some_of any.json_ast) in
    Some obj
  else if any.xml_ast <> None
  then
    let obj = !of_xml piqtype (C.some_of any.xml_ast) in
    Some obj
  else
    None


(* resolve obj from any given, possibly given its type *)
let resolve_obj ?(piqtype: Piqi_impl_piqi.piqtype option) (any :('a, 'b) Piqobj.any) :unit =
  let open Any in
  if any.obj <> None
  then () (* already resolved *)
  else (
    let do_resolve_obj piqtype =
      (* cache typename
       * XXX: do not use fully qualified names for locally defined types? *)
      if any.typename = None
      then any.typename <- Some (Piqi_common.full_piqi_typename piqtype);

      let obj = of_any piqtype any in
      any.obj <- obj
    in
    match piqtype, any.typename with
      | Some t, _ ->
          (* XXX: when both are present, check their correspondence? *)
          C.debug "Piqobj.resolve_obj using known type\n";
          do_resolve_obj t
      | None, Some typename ->
          C.debug "Piqobj.resolve_obj using type %s\n" typename;
          do_resolve_obj (Piqi_db.find_piqtype typename)
      | _ ->
          () (* can't resolve, because type is unknown *)
  )


let piq_of_any (any: ('a, 'b) Piqobj.any) :Piq_ast.ast option =
  let open Any in
  match any.piq_ast with
    | (Some _) as res -> res
    | _ -> (
        (* resolve obj if it wasn't resolved before *)
        resolve_obj any;
        match any.obj with
          | None -> None (* obj could't be resolved *)
          | Some obj ->
              let ast = !to_piq obj in
              any.piq_ast <- Some ast; (* XXX: cache the result *)
              Some ast
    )


let pb_of_any any =
  let open Any in
  match any.pb with
    | (Some _) as res -> res
    | _ -> (
        (* resolve obj if it wasn't resolved before *)
        resolve_obj any;
        match any.obj with
          | None -> None (* obj could't be resolved *)
          | Some obj ->
              Piqloc.pause ();
              let pb = !to_pb (C.some_of any.obj) in
              Piqloc.resume ();
              any.pb <- Some pb; (* XXX: cache the result *)
              Some pb
    )


let json_of_any (any:  ('a, 'b) Piqobj.any) :Piqi_json_type.json option =
  let open Any in
  match any.json_ast with
    | Some _ when any.json_string <> None ->
        (* TODO: this is rather inefficient and redundant -- a better solution
         * would be to change that JSON AST representation and parse literals
         * into intermediate representation (piqobj) format in piqobj_of_json.ml
         *)
        let s = C.some_of any.json_string in
        (* already validated, we just need to parse it in pretty-printed mode so
         * that we can print it nicely while preserving the original int, float
         * and string literals *)
        Piqloc.pause (); (* no need to preserve location information here *)
        let json_ast = Piqi_util.with_bool Piqi_config.pp_mode true (fun () -> !json_of_string s) in
        Piqloc.resume ();
        Some json_ast
    | (Some _) as res -> res
    | _ -> (
        (* resolve obj if it wasn't resolved before *)
        resolve_obj any;
        match any.obj with
          | None -> None (* obj could't be resolved *)
          | Some obj ->
              Piqloc.pause ();
              let json = !to_json obj in
              Piqloc.resume ();
              any.json_ast <- Some json; (* XXX: cache the result *)
              Some json
    )


let xml_of_any (any:  ('a, 'b) Piqobj.any) :Piqi_xml_type.xml list option =
  let open Any in
  match any.xml_ast with
    | Some (_name, xml_list) -> Some xml_list
    | _ -> (
        (* resolve obj if it wasn't resolved before *)
        resolve_obj any;
        match any.obj with
          | None -> None (* obj could't be resolved *)
          | Some obj ->
              Piqloc.pause ();
              let xml_list = !to_xml obj in
              Piqloc.resume ();
              any.xml_ast <- Some ("undefined", xml_list); (* XXX: cache the result *)
              Some xml_list
    )


(* this is used internally mostly for piq extensions and default values that are
 * guaranteed to have piq representation *)
let piq_of_piqi_any piqi_any :Piq_ast.ast =
  let any = any_of_piqi_any piqi_any in
  C.some_of (piq_of_any any)


(* same as the above, plus this is used only by piqic *)
let pb_of_piqi_any piqi_any :string =
  let any = any_of_piqi_any piqi_any in
  C.some_of (pb_of_any any)


include Piqobj

