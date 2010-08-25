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
 * Typefull parser generator for decoding piq data from wire (Protocol
 * Buffers wire) format.
 *)

open Piqi_common
open Iolist


(* reuse several functions *)
open Piqic_erlang_types
open Piqic_erlang_out


let rec gen_parse_type erlang_type wire_type x =
  match x with
    | `any ->
        if !top_modname = "piqtype"
        then ios "parse_any"
        else ios "piqtype:parse_any"
    | (#T.piqdef as x) ->
        let modname = gen_parent x in
        modname ^^ ios "parse_" ^^ ios (piqdef_erlname x)
    | _ -> (* gen parsers for built-in types *)
        iol [
            ios "piqirun:";
            ios (gen_erlang_type_name x erlang_type);
            ios "_of_";
            ios (W.get_wire_type_name x wire_type);
        ]

and gen_parse_typeref ?erlang_type ?wire_type (t:T.typeref) =
  gen_parse_type erlang_type wire_type (piqtype t)


let erl_str_escape x =
  (* FIXME, TODO: Erlang *)
  String.escaped x


(* XXX: parse defaults once at boot time rather than each time when we need to
 * parse a field *)
let gen_default = function
  | None -> ios "'undefined'"
  | Some {T.Any.binobj = Some x} ->
      iol [
        ios "<<"; ioq (erl_str_escape x); ios ">>";
      ]
  | _ ->
      assert false (* binobj should be defined by that time *)


let esc x = ios "_" ^^ ios (String.capitalize x)


let rest i =
  ios "R" ^^ ios (string_of_int !i)


let gen_field_cons f =
  let open Field in
  let fname = erlname_of_field f in
  (* field construction code *)
  iol [ ios fname; ios " = "; esc fname; ]


let gen_field_parser i f =
  let open Field in
  let fname = erlname_of_field f in
  let mode = gen_mode f in
  let fcons =
    match f.typeref with
      | Some typeref ->
          (* field constructor *)
          iol [
            (* "parse_(req|opt|rep)_field" function invocation *)
            ios "piqirun:parse_" ^^ ios mode ^^ ios "_field(";
              gen_code f.code; ios ", ";
              ios "fun "; gen_parse_typeref typeref; ios "/1, ";
              rest i; ios ", ";
              gen_default f.default;
            ios ")";
          ]
      | None ->
          (* flag constructor *)
          iol [ 
            ios "piqirun:parse_flag(";
              gen_code f.code; ios ", ";
              rest i;
            ios ")";
          ]
    in
  incr i;
  (* field parsing code *)
  iol [ ios "{"; esc fname; ios ", "; rest i; ios "} = "; fcons; ]


let gen_record r =
  let name = some_of r.R#erlang_name in
  (* preorder fields by their field's codes *)
  let fields = order_fields r.R#field in

  let fconsl = (* field constructor list *)
    List.map gen_field_cons fields
  in
  let i = ref 0 in
  let fparserl = (* field parsers list *)
    List.map (gen_field_parser i) fields
  in
  iol [
    ios "parse_"; ios name; ios "(X) -> "; indent;
      ios "R0 = piqirun:parse_record(X),"; eol;
      iod ",\n    " fparserl; ios ","; eol;
      ios "piqirun:check_unparsed_fields("; rest i; ios "),"; eol;
      ios "#"; ios (scoped_name name); ios "{"; indent;
      iod ",\n        " fconsl;
      unindent; eol;
      ios "}.";
      unindent; eol;
  ]


let gen_const c =
  let open Option in
  let code_str = gen_code c.code in
  iol [
    ios "{varint, "; code_str; ios "} -> "; ios (some_of c.erlang_name);
  ]


let gen_enum e =
  let open Enum in
  let consts = List.map gen_const e.option in
  let cases = consts @ [
    ios "{varint, Y} -> piqirun:error_enum_const(Y)";
    ios "_ -> piqirun:error_enum_obj(X)";
  ]
  in
  iol [
    ios "parse_" ^^ ios (some_of e.erlang_name); ios "(X) ->"; indent;
    ios "case X of"; indent;
      iod ";\n        " cases;
      unindent; eol;
      ios "end.";
    unindent; eol;
  ]


let rec gen_option o =
  let open Option in
  match o.erlang_name, o.typeref with
    | Some ename, None -> (* boolean true *)
        iol [
          ios "{"; gen_code o.code; ios ", {varint, 1}}"; ios " -> "; ios ename;
        ]
    | None, Some ((`variant _) as t) | None, Some ((`enum _) as t) ->
        iol [
          ios "{"; gen_code o.code; ios ", Obj} -> ";
            gen_parse_typeref t; ios "(Obj)";
        ]
    | _, Some t ->
        let ename = erlname_of_option o in
        iol [
          ios "{"; gen_code o.code; ios ", Obj} -> ";
            ios "{"; ios ename; ios ", ";
              gen_parse_typeref t; ios "(Obj)";
            ios "}";
        ]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = List.map gen_option v.option in
  let cases = options @ [
    ios "{Code, Obj} -> piqirun:error_variant(Obj, Code)";
  ]
  in
  iol [
    ios "parse_" ^^ ios (some_of v.erlang_name); ios "(X) ->"; indent;
      ios "case piqirun:parse_variant(X) of"; indent;
        iod ";\n        " cases;
        unindent; eol;
      ios "end.";
    unindent; eol;
  ]


let gen_alias a =
  let open Alias in
  iol [
    ios "parse_" ^^ ios (some_of a.erlang_name); ios "(X) ->"; indent;
      gen_parse_typeref
              a.typeref ?erlang_type:a.erlang_type ?wire_type:a.wire_type;
      ios "(X).";
    unindent; eol;
  ]


let gen_list l =
  let open L in
  iol [
    ios "parse_" ^^ ios (some_of l.erlang_name); ios "(X) ->"; indent;
      ios "piqirun:parse_list(fun "; gen_parse_typeref l.typeref; ios "/1, X).";
    unindent; eol;
  ]


let gen_def = function
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t
  | `alias t -> gen_alias t


let gen_alias a = 
  let open Alias in
  if a.typeref = `any && not !depends_on_piq_any
  then []
  else [gen_alias a]


let gen_def = function
  | `alias x -> gen_alias x
  | x -> [gen_def x]


let gen_defs (defs:T.piqdef list) =
  let defs = flatmap gen_def defs in
  iod "\n" defs


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_piqdef
