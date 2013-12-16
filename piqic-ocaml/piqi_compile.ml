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

(* TODO: this is a copy-paste from piqi-tools/piqi_compile.ml: we need to move
 * this to the piqilib library *)


module C = Piqi_common  
open C


let get_self_spec_piqtype self_spec typename =
  let piqi_def =
    try Piqi_db.find_local_typedef self_spec.P#resolved_typedef typename
    with Not_found ->
      Printf.eprintf
        "invalid self-spec read from: no definition named %s\n"
        (U.quote typename);
      piqi_error "piqi compile: invalid self-spec"
  in
  (piqi_def: T.typedef :> T.piqtype)


(* make piqi/piqi-list top-level record from the list of piqi piqobjs; we do it
 * by converting the list of piqobjs to the binary representation of piqi-list
 * and then reading it back and piqobj *)
let make_piqi_list_piqobj piqi_list_piqtype (piqi_piqobj_list: Piqobj.obj list) :Piqobj.obj =
  trace "making piqi-list\n";
  trace_enter ();
  trace "converting piqi piqobj list to protobuf piqi-list\n";
  let piqi_binobj_list = List.map
    (fun piqobj ->
      let obuf = Piq.piqobj_to_protobuf (-1) piqobj in
      Piqirun.to_string obuf
    )
    piqi_piqobj_list
  in
  let obuf = Piqirun.gen_list Piqirun.gen_string_field (-1) piqi_binobj_list in
  let s = Piqirun.to_string obuf in
  let ibuf = Piqirun.init_from_string s in
  trace "converting piqi-list from protobuf to piqobj\n";
  let piqi_list_piqobj =
    (* don't resolve defaults -- they should be resolved already *)
    C.with_resolve_defaults false
    (fun () -> Piqobj_of_protobuf.parse_obj piqi_list_piqtype ibuf)
  in
  trace_leave ();
  piqi_list_piqobj


(* NOTE: we also have a similar but more general code in piqi_convert_cmd.ml *)
let rec get_piqi_deps piqi =
  let imports =
    List.map (fun x -> some_of x.T.Import#piqi) piqi.P#resolved_import
  in
  (* get all imports' dependencies recursively *)
  let import_deps =
    U.flatmap (fun piqi ->
        U.flatmap get_piqi_deps piqi.P#included_piqi
      ) imports
  in
  (* remove duplicate entries *)
  let deps = U.uniqq (import_deps @ imports) in
  deps @ [piqi]


let compile ?(strict=false) self_spec piqi =
  trace "getting all imported dependencies\n";
  let piqi_list = get_piqi_deps piqi in

  (* get necessary piqtypes from the self-spec *)
  let piqi_piqtype = get_self_spec_piqtype self_spec "piqi" in
  let piqi_list_piqtype = get_self_spec_piqtype self_spec "piqi-list" in

  trace "converting modules to internal representation\n";
  Config.flag_strict := strict;

  (* convert all modules to internal representation *)
  let piqobj_list = List.map (fun piqi ->
      Piqi.piqi_to_piqobj piqi
        ~custom_piqtype:piqi_piqtype
        ~add_codes:true
    ) piqi_list
  in

  trace "generating pb\n";
  let piqobj = make_piqi_list_piqobj piqi_list_piqtype piqobj_list in
  let buf = Piq.gen_pb (Piq.Piqobj piqobj) in
  Piqirun.to_string buf

