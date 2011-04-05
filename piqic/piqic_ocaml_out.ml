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
open Piqic_ocaml_types


module W = Piqi_wire


let gen_code = Piqic_common.gen_code


let gen_ocaml_type_name t ot =
  gen_piqtype t ot


let gen_parent x =
  match get_parent x with
    | `import x -> (* imported name *)
        let ocaml_modname = some_of x.Import#ocaml_name in
        ios ocaml_modname ^^ ios "."
    | _ -> iol []


let rec gen_gen_type ocaml_type wire_type x =
  match x with
    | `any ->
        if !Piqic_common.is_self_spec
        then ios "(fun code x -> gen__any code x)"
        else ios "(fun code x -> Piqtype.gen__any code x)"
    | (#T.piqdef as x) ->
        let modname = gen_parent x in
        modname ^^ ios "gen__" ^^ ios (piqdef_mlname x)
    | _ -> (* gen generators for built-in types *)
        iol [
          gen_cc "(reference ";
          ios "Piqirun.";
          ios (gen_ocaml_type_name x ocaml_type);
          ios "_to_";
          ios (W.get_wire_type_name x wire_type);
          gen_cc ")";
        ]

and gen_gen_typeref ?ocaml_type ?wire_type t =
  gen_gen_type ocaml_type wire_type (piqtype t)


let gen_mode f =
  match f.F#mode with
    | `required -> "req"
    | `optional when f.F#default <> None -> "req" (* optional + default *)
    | `optional -> "opt"
    | `repeated -> "rep"


let gen_field rname f =
  let open Field in
  let fname = mlname_of_field f in
  let ffname = (* fully-qualified field name *)
    iod "." [ios "x"; ios rname; ios fname]
  in 
  let mode = gen_mode f in
  let fgen =
    match f.typeref with
      | Some typeref ->
          (* field generation code *)
          iod " "
            [ 
              ios "Piqirun.gen_" ^^ ios mode ^^ ios "_field";
                gen_code f.code;
                gen_gen_typeref typeref;
                ffname
            ]
      | None ->
          (* flag generation code *)
          iod " " [
            ios "Piqirun.gen_flag"; gen_code f.code; ffname;
          ]
  in (fname, fgen)


let gen_record r =
  (* fully-qualified capitalized record name *)
  let rname = capitalize (some_of r.R#ocaml_name) in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fgens = (* field generators list *)
    List.map (gen_field rname) fields
  in
  (* field names *)
  let fnames, _ = List.split fgens in

  let esc x = ios "_" ^^ ios x in

  (* field generator code *)
  let fgens_code = List.map
    (fun (name, gen) -> iol [ ios "let "; esc name; ios " = "; gen ])
    fgens
  in (* gen_<record-name> function delcaration *)
  iod " "
    [
      ios "gen__" ^^ ios (some_of r.R#ocaml_name); ios "code x =";
        gen_cc "refer x;";
        iod " in " fgens_code;
        ios "in";
        ios "Piqirun.gen_record code"; 
        ios "["; iod ";" (List.map esc fnames); ios "]";
    ]


let gen_const c =
  let open Option in
  iod " " [
    ios "|"; gen_pvar_name (some_of c.ocaml_name); ios "->";
      ios "Piqirun.gen_varint32 code";
      gen_code c.code ^^ ios "l"; (* ocaml int32 literal *)
  ]


let gen_enum e =
  let open Enum in
  let consts = List.map gen_const e.option in
  iod " "
    [
      ios "gen__" ^^ ios (some_of e.ocaml_name);
      ios "code x =";
        gen_cc "refer x;";
        ios "match x with";
        iol consts;
    ]


let rec gen_option o =
  let open Option in
  match o.ocaml_name, o.typeref with
    | Some mln, None -> (* gen true *)
        iod " " [
          ios "|"; gen_pvar_name mln; ios "->";
            gen_cc "refer x;";
            ios "Piqirun.gen_bool"; gen_code o.code; ios "true";
        ]
    | None, Some ((`variant _) as t) | None, Some ((`enum _) as t) ->
        iod " " [
          ios "| (#" ^^ ios (piqdef_mlname t); ios " as x) ->";
            gen_gen_typeref t; gen_code o.code; ios "x";
        ]
    | _, Some t ->
        let mln = mlname_of_option o in
        iod " " [
          ios "|"; gen_pvar_name mln; ios "x ->";
            gen_gen_typeref t; gen_code o.code; ios "x";
        ]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = List.map gen_option v.option in
  iod " "
    [
      ios "gen__" ^^ ios (some_of v.ocaml_name);
      ios "code (x:" ^^ ios_gen_typeref (`variant v) ^^ ios ") =";
      gen_cc "refer x;";
      ios "Piqirun.gen_record code [(match x with"; iol options; ios ")]";
    ]


let gen_alias a =
  let open Alias in
  iod " " [
    ios "gen__" ^^ ios (some_of a.ocaml_name);
    ios "code x =";
      gen_gen_typeref a.typeref ?ocaml_type:a.ocaml_type ?wire_type:a.wire_type;
      ios "code x";
  ]


let gen_gen_list t =
  iol [
    gen_cc "reference ";
    ios "(Piqirun.gen_list (" ^^ gen_gen_typeref t ^^ ios "))"
  ]


let gen_list l =
  let open L in
  iod " " [
    ios "gen__" ^^ ios (some_of l.ocaml_name);
    ios "code x =";
    gen_gen_list l.typeref; ios "code x";
  ]


let gen_def = function
  | `alias t -> gen_alias t
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t


(* generate gen_<name>/1 functions *)
let gen_def_1 x =
  let name = ios (piqdef_mlname x) in
  iol [
    ios "let gen_"; name; ios " x = ";
      ios"gen__"; name; ios " (-1) x";
    ios "\n";
  ]


let gen_defs (defs:T.piqdef list) =
  if defs = []
  then iol []
  else
    let defs_2 = List.map gen_def defs in
    let defs_1 = List.map gen_def_1 defs in
    iod " " [
      gen_cc "let next_count = Piqloc.next_ocount";
      (* NOTE: providing special handling for boxed objects, since they are not
       * references and can not be uniquely identified. Moreover they can mask
       * integers which are used for enumerating objects *)
      gen_cc "let refer obj =
        let count = next_count () in
        if not (Obj.is_int (Obj.repr obj))
        then Piqloc.addref obj count";
      gen_cc "let reference f code x = refer x; f code x";
      ios "let rec"; iod " and " defs_2;
      ios "\n\n";
      iol defs_1;
      ios "\n";
    ]


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_piqdef
