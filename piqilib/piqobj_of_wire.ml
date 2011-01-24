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


module W = Piqi_wire


let next_count = Piqloc.next_icount


let reference0 f x =
  let count = next_count () in
  let obj = f x in
  Piqloc.addrefret count obj


let reference f t x =
  let count = next_count () in
  let obj = f t x in
  Piqloc.addrefret count obj


(* XXX: resolve_defaults should be disabled by default *)
let resolve_defaults = ref false


(* XXX: move to Piqi_wire? *)
let parse_int ?wire_type x =
  let r0 = reference0 in
  let wire_type = W.get_wire_type `int wire_type in
  match wire_type with
    | `varint -> `uint (r0 Piqirun.int64_of_varint x)
    | `zigzag_varint -> `int (r0 Piqirun.int64_of_zigzag_varint x)
    | `fixed32 -> `uint (r0 Piqirun.int64_of_fixed32 x)
    | `fixed64 -> `uint (r0 Piqirun.int64_of_fixed64 x)
    | `signed_varint -> `int (r0 Piqirun.int64_of_signed_varint x)
    | `signed_fixed32 -> `int (r0 Piqirun.int64_of_signed_fixed32 x)
    | `signed_fixed64 -> `int (r0 Piqirun.int64_of_signed_fixed64 x)
    | `block -> assert false (* XXX *)


let parse_float ?wire_type x =
  let r0 = reference0 in
  let wire_type = W.get_wire_type `float wire_type in
  match wire_type with
    | `fixed32 -> r0 Piqirun.float_of_fixed32 x
    | `fixed64 -> r0 Piqirun.float_of_fixed64 x
    | _ -> assert false (* XXX *)


let rec parse_obj0 (t:T.piqtype) x :Piqobj.obj =
  let r0 = reference0 in
  let r = reference in
  match t with
    (* built-in types *)
    | `int -> parse_int x
    | `float -> `float (parse_float x)
    | `bool -> `bool (r0 Piqirun.parse_bool x)
    | `string -> `string (r0 Piqirun.parse_string x)
    | `binary -> `binary (r0 Piqirun.parse_binary x)
    | `word -> `word (r0 Piqirun.parse_string x)
    | `text -> `text (r0 Piqirun.parse_string x)
    | `any -> `any (parse_any x)
    (* custom types *)
    | `record t -> `record (r parse_record t x)
    | `variant t -> `variant (r parse_variant t x)
    | `enum t -> `enum (r parse_enum t x)
    | `list t -> `list (r parse_list t x)
    | `alias t -> `alias (parse_alias0 t x)


and parse_obj t x =
  (* using the same count here as we will use for parsing the objects themselves
   *)
  let count = !Piqloc.icount in
  let res = parse_obj0 t x in
  Piqloc.addrefret count res


and try_parse_binobj ?piqtype binobj =
  let name, x = Piqirun.parse_binobj binobj in
  if name = None && piqtype = None
  then None (* type of binary object is unknown *)
  else
    let t =
      match name, piqtype with
        | None, Some t -> t
        | Some n, Some t ->
            let n'= C.full_piqi_typename t in
            if n = n'
            then t
            else
              (* XXX: warning? *)
              piqi_error
                ("mismatch between supplied type and binobj type: " ^ n' ^ " v/s " ^ n)
        | Some n, None ->
            begin
              try 
                Piqi_db.find_piqtype n
              with Not_found -> 
                (* XXX: location? *)
                piqi_error ("unknown binobj type: " ^ n)
            end
        | _ -> assert false
    in
    let res = parse_obj t x in
    Some res


and parse_binobj ?piqtype binobj = 
  match try_parse_binobj ?piqtype binobj with
    | Some x -> x
    | None -> 
        piqi_error "error parsing binobj: type is unspecified/unknown"
        

and parse_any x =
  let piq_any = T.parse_any x in
  let obj =
    match piq_any.T.Any#binobj with
      | Some x ->
          (* parse binobj if it is named (i.e. typed) *)
          try_parse_binobj x
      | None -> None
  in
  Any#{ any = piq_any; obj = obj }


and parse_record t x =
  let l = Piqirun.parse_record x in
  (* NOTE: fields are pre-order by wire code *)
  let fields_spec = t.T.Record#wire_field in
  let fields, rem = List.fold_left parse_field ([], l) fields_spec in
  Piqirun.check_unparsed_fields rem;
  R#{ piqtype = t; field = List.rev fields}


and parse_field (accu, rem) t =
  let fields, rem =
    match t.T.Field#typeref with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let code = Int32.to_int (some_of t.code) in
  let res, rem = Piqirun.parse_flag code l in
  match res with
    | false -> [], rem
    | true ->
        begin
          let res = F#{ piqtype = t; obj = None } in

          (* skip boolean used to encode empty flag value *)
          let count = next_count () in
          Piqloc.addref count res;

          [res], rem
        end


and do_parse_field t l =
  let open T.Field in
  let code = Int32.to_int (some_of t.code) in
  let field_type = piqtype (some_of t.typeref) in
  let parse_f = parse_obj field_type in
  let values, rem =
    match t.mode with
      | `required -> 
          let x, rem = parse_required_field code parse_f l in
          [x], rem
      | `optional ->
          let default =
            if !resolve_defaults
            then t.default
            else None
          in
          (* XXX: location reference for default? *)
          let x, rem = parse_optional_field code parse_f default l in
          let res = (match x with Some x -> [x] | None -> []) in
          res, rem
      | `repeated ->
          parse_repeated_field code parse_f l
  in
  let fields =
    List.map (fun x ->
      let res = F#{ piqtype = t; obj = Some x } in
      Piqloc.addrefret x res) values
  in
  fields, rem


and parse_required_field code parse_f l =
  Piqirun.parse_req_field code parse_f l


and parse_optional_field code parse_f default l =
  match default with
    | None -> Piqirun.parse_opt_field code parse_f l
    | Some x ->
        let default = some_of x.T.Any.binobj in
        let res, rem = Piqirun.parse_req_field code parse_f l ~default in
        Some res, rem


and parse_repeated_field code parse_f l =
  Piqirun.parse_rep_field code parse_f l


and parse_variant t x =
  let code, obj = Piqirun.parse_variant x in
  let code32 = Int32.of_int code in
  let options = t.T.Variant#option in
  let option =
    try
      let o = List.find (fun o -> some_of o.T.Option#code = code32) options in
      parse_option o obj
    with Not_found ->
      Piqirun.error_variant x code
  in
  V#{ piqtype = t; option = option }


and parse_option t x =
  let open T.Option in
  match t.typeref with
    | None ->
        if Piqirun.parse_bool x = true
        then
          let res = O#{ piqtype = t; obj = None } in
          (* skip boolean used to encode empty option value *)
          let count = next_count () in
          Piqloc.addrefret count res
        else
          piqi_error "invalid representation of untyped option"
    | Some typeref ->
        let option_type = piqtype typeref in
        let obj = parse_obj option_type x in
        let res = O#{ piqtype = t; obj = Some obj } in
        Piqloc.addrefret obj res


and parse_enum t x =
  match x with
    | Piqirun.Varint code ->
        let code = Int32.of_int code in
        do_parse_enum t x code
    | Piqirun.Varint64 code ->
        let code = Int64.to_int32 code in
        do_parse_enum t x code
    | _ ->
        Piqirun.error_enum_obj x


and do_parse_enum t x code32 =
  let options = t.T.Variant#option in
  let option =
    try
      let o = List.find (fun o -> some_of o.T.Option#code = code32) options in
      let res = O#{ piqtype = o; obj = None } in
      (* add location reference which is equal to the enum location *)
      Piqloc.addrefret !Piqloc.icount res
    with Not_found ->
      Piqirun.error_enum_const x
  in
  V#{ piqtype = t; option = option }


and parse_list t x = 
  let obj_type = piqtype t.T.Piqlist#typeref in
  let contents = Piqirun.parse_list (parse_obj obj_type) x in
  L#{ piqtype = t; obj = contents }


and parse_alias0 t x =
  parse_alias t x


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias t ?wire_type x =
  let open T.Alias in
  let this_wire_type = t.wire_type in
  (* wire-type defined in this alias trumps wire-type passed by the upper
   * definition *)
  (* XXX: report a wire-type conflict rather than silently use the default? *)
  let wire_type =
    match wire_type, this_wire_type with
      | _, Some _ -> this_wire_type
      | _ -> wire_type
  in
  let obj_type = piqtype t.typeref in
  let obj = parse_alias_obj obj_type ?wire_type x in
  A#{ piqtype = t; obj = obj }


and parse_alias_obj t ?wire_type x =
  match t with
    | `int -> parse_int x ?wire_type
    | `float -> `float (parse_float x ?wire_type)
    | `alias t -> `alias (parse_alias t x ?wire_type)
    | _ -> parse_obj t x
