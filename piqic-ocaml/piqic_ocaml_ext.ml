(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

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
 * generation of interfaces for multi-format JSON/XML/Protobuf/Piq serialization
 *)

module C = Piqic_common
open C
open Iolist


let gen_init_piqi modname =
  (* init embedded piqi spec *)
  iol [
    ios "let piqi = "; ios modname; ios ".piqi";
    eol;eol;
    ios "let _ = Piqirun_ext.init_piqi piqi";
    eol;eol;
  ]


let typedef_scoped_name context typedef =
  let piqi = context.piqi in
  let modname = some_of piqi.P#modname in
  let name = C.typedef_name typedef in
  modname ^ "/" ^ name


let gen_init_piqtype context typedef =
  let name = C.typedef_mlname typedef in
  let scoped_name = typedef_scoped_name context typedef in
  iod " " [
    ios "let _" ^^ ios name ^^ ios "_piqtype =";
      ios "Piqirun_ext.find_piqtype"; ioq scoped_name;
    eol;
  ]


let gen_convert name input_format output_format data =
  let piqtype = "_" ^ name ^ "_piqtype" in
  iod " " [
    ios "Piqirun_ext.convert";
      ios piqtype; ios input_format; ios output_format; ios data;
  ]


let gen_parse modname typedef =
  let name = C.typedef_mlname typedef in
  iod " " [
    ios "let parse_" ^^ ios name;
        ios "?opts"; ios "x (format :Piqirun_ext.input_format) =";
      ios "let x_pb ="; gen_convert name "format" "`pb" "x"; ios "?opts";
      ios "in";
      ios "let buf = Piqirun.init_from_string x_pb";
      ios "in";
      ios modname ^^ ios ".parse_" ^^ ios name; ios "buf";
      eol;
  ]


let gen_gen modname typedef =
  let name = C.typedef_mlname typedef in
  iod " " [
    ios "let gen_" ^^ ios name;
        ios "?opts"; ios "x (format :Piqirun_ext.output_format) =";
      ios "let buf = "; ios modname ^^ ios ".gen_" ^^ ios name; ios "x";
      ios "in";
      ios "let x_pb = Piqirun.to_string buf";
      ios "in";
      gen_convert name "`pb" "format" "x_pb"; ios "?opts";
      eol;
  ]


let gen_print typedef =
  let name = C.typedef_mlname typedef in
  iod " " [
    ios "let print_" ^^ ios name; ios "x =";
      ios "Pervasives.print_endline (gen_" ^^ ios name; ios "x `piq)";
    eol;
    ios "let prerr_" ^^ ios name; ios "x =";
      ios "Pervasives.prerr_endline (gen_" ^^ ios name; ios "x `piq)";
    eol;
  ]


let gen_piqi context =
  let piqi = context.piqi in
  let modname = some_of piqi.P#ocaml_module in
  let typedefs = piqi.P#typedef in

  (* XXX, TODO: skipping built-in typedefs for now; in theory we could generate
   * piqi_piqi_ext.ml and include it as a part of piqilib to make serialization
   * for built-in types to work *)
  let typedefs = List.filter (fun x -> not (C.is_builtin_typedef x)) typedefs in

  let type_initializers = List.map (gen_init_piqtype context) typedefs in
  let parsers = List.map (gen_parse modname) typedefs in
  let generators = List.map (gen_gen modname) typedefs in
  let printers = List.map gen_print typedefs in

  iol [
    gen_init_piqi modname; eol; eol;
    iol type_initializers; eol; eol;
    iol parsers; eol; eol;
    iol generators; eol; eol;
    iol printers; eol; eol;
  ]

