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


(*
 * Typefull generator generator for encoding piq data into wire (Protocol
 * Buffers wire) format.
 *)

open Piqi_common
open Iolist


(* reuse several functions *)
open Piqic_erlang_types


module W = Piqi_wire


let gen_code = Piqic_common.gen_code


let gen_erlang_type_name t ot =
  gen_piqtype t ot


let gen_parent x =
  match get_parent x with
    | `import x -> (* imported name *)
        let piqi = some_of x.Import.piqi in
        let erlang_modname = some_of piqi.P#erlang_module in
        ios erlang_modname ^^ ios ":"
    | _ -> iol []


let rec gen_gen_type erlang_type wire_type x =
  match x with
    | `any ->
        if !top_modname = "piqtype"
        then ios "gen_any"
        else ios "piqtype:gen_any"
    | (#T.piqdef as x) ->
        let modname = gen_parent x in
        modname ^^ ios "gen_" ^^ ios (piqdef_erlname x)
    | _ -> (* gen generators for built-in types *)
        iol [
          ios "piqirun:";
          ios (gen_erlang_type_name x erlang_type);
          ios "_to_";
          ios (W.get_wire_type_name x wire_type);
        ]

and gen_gen_typeref ?erlang_type ?wire_type t =
  gen_gen_type erlang_type wire_type (piqtype t)


let gen_mode f =
  match f.F#mode with
    | `required -> "req"
    | `optional -> "opt"
    | `repeated -> "rep"


let gen_field rname f =
  let open Field in
  let fname = erlname_of_field f in
  let ffname = (* fully-qualified field name *)
    iol [ios "X#"; ios rname; ios "."; ios fname]
  in 
  let mode = gen_mode f in
  let fgen =
    match f.typeref with
      | Some typeref ->
          (* field generation code *)
          iol
            [
              ios "piqirun:gen_" ^^ ios mode ^^ ios "_field(";
                gen_code f.code; ios ", ";
                ios "fun "; gen_gen_typeref typeref; ios "/2, ";
                ffname;
              ios ")"
            ]
      | None ->
          (* flag generation code *)
          iod " " [
            ios "piqirun:gen_flag(";
              gen_code f.code; ios ", ";
              ffname;
            ios ")";
          ]
  in fgen


let gen_record r =
  let rname = scoped_name (some_of r.R#erlang_name) in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fgens = (* field generators list *)
    List.map (gen_field rname) fields
  in (* gen_<record-name> function delcaration *)
  iol
    [
      ios "gen_" ^^ ios (some_of r.R#erlang_name); ios "(Code, X) ->"; indent;
        ios "piqirun:gen_record(Code, ["; indent;
          iod ",\n        " fgens;
          unindent; eol;
        ios "]).";
        unindent; eol;
    ]


let gen_const c =
  let open Option in
  iol [
    ios (some_of c.erlang_name); ios " -> ";
      ios "piqirun:encode_varint_field(Code, "; gen_code c.code; ios ")"
  ]


let gen_enum e =
  let open Enum in
  let consts = List.map gen_const e.option in
  iol
    [
      ios "gen_" ^^ ios (some_of e.erlang_name);
      ios "(Code, X) ->"; indent;
        ios "case X of"; indent;
        iod ";\n        " consts;
        unindent; eol;
        ios "end.";
      unindent; eol;
    ]


let gen_inner_option pattern outer_option =
  let open Option in
  let o = some_of outer_option in
  let t = some_of o.typeref in
  let res =
    iol [
      pattern; ios " -> ";
        gen_gen_typeref t; ios "("; gen_code o.code; ios ", X)";
    ]
  in [res]


let rec gen_option outer_option o =
  let open Option in
  match o.erlang_name, o.typeref with
    | Some ename, None -> (* gen true *)
        if outer_option <> None
        then gen_inner_option (ios ename) outer_option
        else
          let res =
            iol [
              ios ename; ios " -> ";
                ios "piqirun:gen_bool("; gen_code o.code; ios ", true)";
            ]
          in [res]
    | None, Some (`variant v) | None, Some (`enum v) ->
        (* recursively generate cases from "included" variants *)
        if outer_option <> None
        then flatmap (gen_option outer_option) v.V.option
        else flatmap (gen_option (Some o)) v.V.option
    | _, Some t ->
        let ename = erlname_of_option o in
        if outer_option <> None
        then gen_inner_option (iol [ ios "{"; ios ename; ios ", _}"]) outer_option
        else
          let res = 
            iol [
              ios "{"; ios ename; ios ", Y} -> ";
                gen_gen_typeref t; ios "("; gen_code o.code; ios ", Y)";
            ]
          in [res]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = flatmap (gen_option None) v.option in
  iol
    [
      ios "gen_" ^^ ios (some_of v.erlang_name);
      ios "(Code, X) ->"; indent;
      ios "piqirun:gen_variant(Code,"; indent;
        ios "case X of"; indent; iod ";\n            " options;
        unindent; eol;
        ios "end";
        unindent; eol;
        ios ").";
        unindent; eol;
    ]


let gen_alias a =
  let open Alias in
  iol [
    ios "gen_" ^^ ios (some_of a.erlang_name);
    ios "(Code, X) ->"; indent;
      gen_gen_typeref a.typeref ?erlang_type:a.erlang_type ?wire_type:a.wire_type;
      ios "(Code, X).";
    unindent; eol;
  ]


let gen_list l =
  let open L in
  iol [
    ios "gen_" ^^ ios (some_of l.erlang_name);
    ios "(Code, X) ->"; indent;
      ios "piqirun:gen_list(Code, ";
        ios "fun "; gen_gen_typeref l.typeref; ios "/2, X).";
    unindent; eol;
  ]


let gen_spec x =
  iol [
    ios "-spec gen_"; ios (piqdef_erlname x); ios "/2 :: (";
      ios "Code :: piqirun_code(), ";
      ios "X :: "; ios_gen_out_typeref (x :> T.typeref); ios ") -> ";
    ios "iolist().";
  ]


let gen_def x =
  let generator =
    match x with
      | `alias t -> gen_alias t
      | `record t -> gen_record t
      | `variant t -> gen_variant t
      | `enum t -> gen_enum t
      | `list t -> gen_list t
  in iol [
    gen_spec x; eol;
    generator;
  ]


let gen_def x =
  let open Alias in
  match x with
    | `alias a ->
        if a.typeref = `any && not !Piqic_common.depends_on_piq_any
        then []
        else [gen_def x]
    | _ ->
        [gen_def x]


let gen_defs (defs:T.piqdef list) =
  let defs = flatmap gen_def defs in
  iod "\n" defs


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_piqdef
