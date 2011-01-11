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
 * Typefull generator generator for decoding piq data from wire (Protocol
 * Buffers wire) format.
 *)

open Piqi_common
open Iolist


(* reuse several functions *)
open Piqic_ocaml_types
open Piqic_ocaml_out


let rec gen_parse_type ocaml_type wire_type x =
  match x with
    | `any ->
        if !top_modname = "Piqtype"
        then ios "parse_any"
        else ios "Piqtype.parse_any"
    | (#T.piqdef as x) ->
        let modname = gen_parent x in
        modname ^^ ios "parse_" ^^ ios (piqdef_mlname x)
    | _ -> (* gen parsers for built-in types *)
        iol [
          gen_cc "(fun x -> let count = next_count() in refer count (";
            ios "Piqirun.";
            ios (gen_ocaml_type_name x ocaml_type);
            ios "_of_";
            ios (W.get_wire_type_name x wire_type);
          gen_cc " x))";
        ]

and gen_parse_typeref ?ocaml_type ?wire_type (t:T.typeref) =
  gen_parse_type ocaml_type wire_type (piqtype t)


(* TODO: parse defaults once at boot time rather than each time when we need to
 * parse a field *)
let gen_default = function
  | None -> iol []
  | Some {T.Any.binobj = Some x} ->
      iol [ios "~default:"; ioq (String.escaped x) ]
  | _ ->
      assert false (* binobj should be defined by that time *)


let esc x = ios "_" ^^ ios x


(* let gen_field_cons = Gen_i_piq.gen_field_cons *)
let gen_field_cons rname f =
  let open Field in
  let fname = mlname_of_field f in
  let ffname = (* fully-qualified field name *)
    iol [ios rname; ios "."; ios fname]
  in 
  (* field construction code *)
  iod " " [ ffname; ios "="; esc fname; ios ";" ]


let gen_field_parser f =
  let open Field in
  let fname = mlname_of_field f in
  let mode = gen_mode f in
  let fcons =
  match f.typeref with
    | Some typeref ->
        (* field constructor *)
        iod " "
          [
            (* "parse_(req|opt|rep)_field" function invocation *)
            ios "Piqirun.parse_" ^^ ios mode ^^ ios "_field";
              gen_code f.code;
              gen_parse_typeref typeref; ios " x";
              gen_default f.default;
          ]
    | None ->
        (* flag constructor *)
        iod " " [ 
          (* NOTE: providing special handling for boxed values, see "refer" *)
          gen_cc "(let count = next_count() in refer count";
          ios "Piqirun.parse_flag"; gen_code f.code; ios " x";
          gen_cc ")";
        ]
  in
  (* field parsing code *)
  iod " " [ esc fname; ios ", x ="; fcons; ]


let gen_record r =
  (* fully-qualified capitalized record name *)
  let rname = capitalize (some_of r.R#ocaml_name) in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fconsl = (* field constructor list *)
    List.map (gen_field_cons rname) fields
  in
  let fparserl = (* field parsers list *)
    List.map gen_field_parser fields
  in
  let rcons = (* record constructor *)
    iol [
      ios "let "; iod " in let " fparserl;
      ios " in Piqirun.check_unparsed_fields x; {"; iol fconsl; ios "}";
    ]
  in (* parse_<record-name> function delcaration *)
  iod " "
    [
      ios "parse_" ^^ ios (some_of r.R#ocaml_name); ios "x =";
      ios "let x = Piqirun.parse_record x in";
      gen_cc "let count = next_count() in refer count (";
      rcons;
      gen_cc ")";
    ]


let gen_const c =
  let open Option in
  (* NOTE: using int32 codes which can't be represented on 32-bit and 64-bit
   * uniformely *)
  let code_str = gen_code c.code in
  let code = some_of c.code in
  let varint_pattern =
    ios "Piqirun.Varint " ^^ code_str
  in
  let varint64_pattern =
    ios "Piqirun.Varint64 " ^^ code_str ^^ ios "L"
  in
  let varint_safe_pattern =
    ios "Piqirun.Varint x " ^^
      ios "when Sys.word_size = 64 && Int64.of_int x = " ^^ code_str ^^ ios "L"
  in
  let name = gen_pvar_name (some_of c.ocaml_name) in
  let make_expr pattern =
    iod " " [ios "| "; pattern; ios "->"; name]
  in
  let (>=) a b = Int32.compare a b >= 0 in
  let (<=) a b = Int32.compare a b <= 0 in
  let expr =
    if code <= 0x3fff_ffffl && code >= -0x4000_0000l (* int31? *)
    then
      (* code literal is safe on both 64-bit and 32-bit platforms *)
      make_expr varint_pattern
    else
      iol [
        make_expr varint64_pattern;
        make_expr varint_safe_pattern;
      ]
  in expr


let gen_enum e =
  let open Enum in
  let consts = List.map gen_const e.option in
  iod " "
    [
      ios "parse_" ^^ ios (some_of e.ocaml_name); ios "x =";
      gen_cc "let count = next_count() in refer count (";
        ios "match x with";
        iol consts;
        ios "| Piqirun.Varint x -> Piqirun.error_enum_const x";
        ios "| obj -> Piqirun.error_enum_obj obj";
      gen_cc ")";
    ]


let rec gen_option varname o =
  let open Option in
  match o.ocaml_name, o.typeref with
    | Some mln, None -> (* boolean true *)
        iod " " [
          ios "|"; gen_code o.code; ios "when x = Piqirun.Varint 1"; ios "->";
            (* NOTE: providing special handling for boxed values, see "refer" *)
            gen_cc "let count = next_count() in refer count";
            gen_pvar_name mln;
        ]
    | None, Some ((`variant _) as t) | None, Some ((`enum _) as t) ->
        iod " " [
          ios "|"; gen_code o.code; ios "->";
            ios "("; gen_parse_typeref t; ios "x :>"; ios varname; ios ")"
        ]
    | _, Some t ->
        let n = mlname_of_option o in
        iod " " [
          ios "|"; gen_code o.code; ios "->";
            ios "let res = ";
              gen_cc "let count = curr_count() in refer count (";
              gen_parse_typeref t; ios "x";
              gen_cc ")";
              ios "in";
              gen_pvar_name n; ios "res";
        ]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = List.map (gen_option (some_of v.ocaml_name)) v.option in
  iod " "
    [
      ios "parse_" ^^ ios (some_of v.ocaml_name); ios "x =";
      ios "let code, x = Piqirun.parse_variant x in";
        gen_cc "let count = next_count() in refer count (";
        ios "match code with";
          iod " " options;
          ios "| _ -> Piqirun.error_variant x code";
          gen_cc ")";
    ]


let gen_alias a =
  let open Alias in
  iod " "
    [
      ios "parse_" ^^ ios (some_of a.ocaml_name); ios "x =";
      gen_parse_typeref
        a.typeref ?ocaml_type:a.ocaml_type ?wire_type:a.wire_type; ios " x";
    ]


let gen_parse_list t =
  iol [
    ios "(";
      gen_cc "let count = next_count() in refer count (";
        ios "Piqirun.parse_list (" ^^ gen_parse_typeref t ^^ ios ")";
      gen_cc ")";
    ios ")";
  ]


let gen_list l =
  let open L in
  iod " "
    [
      ios "parse_" ^^ ios (some_of l.ocaml_name); ios "x =";
      gen_parse_list l.typeref; ios " x";
    ]


let gen_def = function
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t
  | `alias t -> gen_alias t


let gen_alias a = 
  let open Alias in
  if a.typeref = `any && not !Piqic_common.depends_on_piq_any
  then []
  else [gen_alias a]


let gen_def = function
  | `alias x -> gen_alias x
  | x -> [gen_def x]


let gen_defs (defs:T.piqdef list) =
  let defs = flatmap gen_def defs in
  if defs = []
  then iol []
  else iod " "
    [
      gen_cc "let next_count = Piqloc.next_icount";
      gen_cc "let curr_count () = !Piqloc.icount";
      (* NOTE: providing special handling for boxed objects, since they are not
       * references and can not be uniquely identified. Moreover they can mask
       * integers which are used for enumerating objects *)
      gen_cc "let refer ref obj =
        if not (Obj.is_int (Obj.repr obj))
        then Piqloc.addrefret ref obj
        else obj";
      ios "let rec"; iod " and " defs;
      ios "\n";
      (*
      ios "end\n";
      *)
    ]


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_piqdef
