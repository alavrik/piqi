(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
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
 * Piq interface compiler for OCaml (parts that couldn't be put into
 * piqi_ocaml.ml, because this would break piqicc bootstrap)
 *)

module C = Piqi_common
open C


(*
 * set ocaml names if not specified by user
 *)

let _ =
  (* normalize Piqi identifiers unless overrided by the command-line option *)
  Piqic_common.flag_normalize := true


(* ocaml name of piqi name *)
let ocaml_name n =
  let n =
    if !Piqic_common.flag_normalize
    then Piqi_name.normalize_name n
    else n
  in
  dashes_to_underscores n


let ocaml_lcname n = (* lowercase *)
  String.uncapitalize (ocaml_name n)


let ocaml_ucname n = (* uppercase *)
  String.capitalize (ocaml_name n)


let mlname n =
  Some (ocaml_lcname n)


(* variant of mlname for optional names *)
let mlname' n =
  match n with
    | None -> n
    | Some n -> mlname n


let mlname_field x =
  let open Field in (
    if x.ocaml_array && x.mode <> `repeated
    then C.error x ".ocaml-array flag can be used only with repeated fields";

    if x.ocaml_name = None then x.ocaml_name <- mlname' x.name
  )


let mlname_record x =
  let open Record in
  (if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
   List.iter mlname_field x.field)


let mlname_option x =
  let open Option in
  if x.ocaml_name = None then x.ocaml_name <- mlname' x.name


let mlname_variant x =
  let open Variant in
  (if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
   List.iter mlname_option x.option)


let mlname_enum x =
  let open Enum in
  (if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
   List.iter mlname_option x.option)


let mlname_alias x =
  let open Alias in
  if x.ocaml_name = None then x.ocaml_name <- mlname x.name


let mlname_list x =
  let open L in
  if x.ocaml_name = None then x.ocaml_name <- mlname x.name


let mlname_piqdef = function
  | `record x -> mlname_record x
  | `variant x -> mlname_variant x
  | `enum x -> mlname_enum x
  | `alias x -> mlname_alias x
  | `list x -> mlname_list x


let mlname_defs (defs:T.piqdef list) =
  List.iter mlname_piqdef defs


let mlmodname n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  Some (ocaml_ucname n ^ "_piqi")


let rec mlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    if piqi.ocaml_module = None
    then piqi.ocaml_module <- mlmodname (some_of piqi.modname);

    mlname_defs piqi.P#resolved_piqdef;
    mlname_defs piqi.P#imported_piqdef;
    mlname_imports piqi.P#resolved_import;
  end

and mlname_imports imports = List.iter mlname_import imports

and mlname_import import =
  let open Import in
  begin
    match import.piqi with
      | None -> () (* unresolved meaning that is called from piqic_expand.ml *)
      | Some piqi -> (* normal "piqic ocaml" mode -- naming the dependencies *)
          mlname_piqi piqi;
          if import.ocaml_name = None
          then import.ocaml_name <- Some (ocaml_ucname (some_of import.name))
  end


open Iolist


let piqic (piqi: T.piqi) =
  Piqic_common.piqic_common piqi;

  (* set ocaml names that are not specified by user *)
  (match !C.boot_piqi with
    | None -> ()
    | Some x -> mlname_piqi x
  );
  mlname_piqi piqi;

  (* set current module's name *)
  Piqic_ocaml_types.top_modname := some_of piqi.P#ocaml_module;

  (* NOTE: generating them in this order explicitly in order to avoid
   * right-to-left evaluation if we put this code inside the list *)
  let c1 = Piqic_ocaml_types.gen_piqi piqi in
  let c2 = Piqic_ocaml_in.gen_piqi piqi in
  let c3 = Piqic_ocaml_out.gen_piqi piqi in
  let c4 =
    if !Piqic_common.flag_gen_defaults
    then Piqic_ocaml_defaults.gen_piqi piqi
    else iol []
  in
  let code = iol [ c1; c2; c3; c4 ] in
  code

