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
 * This module contain functionality that is shared between several
 * language-specific piqi compilers.
 *)

module C = Piqi_common
open C
open Iolist


let gen_code = function
  | None -> assert false
  | Some code -> ios (Int32.to_string code)


(* generate default value for a built-in type *)
let gen_builtin_default_value wire_type t =
  let gen_obj code x =
    match x with
      | #T.piqdef | `any -> assert false
      | `int -> Piqobj_to_wire.gen_int code 0L ?wire_type
      | `float -> Piqobj_to_wire.gen_float code 0.0 ?wire_type
      | `bool -> Piqobj_to_wire.gen_bool code false
      | `string | `binary | `text | `word ->
          Piqobj_to_wire.gen_string code ""
  in
  Piqirun.gen_binobj gen_obj t


(* indication whether there is a defintion that uses "piq_any" type *)
let depends_on_piq_any = ref false


(* indication whether the module that is being processed is a Piqi self-spec,
 * i.e. it is "piqi.org/piqtype" or includes it *)
let is_self_spec = ref false


let is_boot_def def =
  match get_parent def with
    | `piqi p -> is_boot_piqi p
    | _ -> false


let get_boot_defs seen_defs def =
  let get_boot_def = function
    | #T.piqdef as x when is_boot_def x ->
        if List.memq x seen_defs (* previously seen boot def? *)
        then []
        else [x]
    | _ -> []
  in
  let get_boot_def_opt = function
    | Some x -> get_boot_def x
    | None -> []
  in
  match def with
    | `record x -> flatmap (fun x -> get_boot_def_opt x.F#typeref) x.R#field
    | `variant x -> flatmap (fun x -> get_boot_def_opt x.O#typeref) x.V#option
    | `list x -> get_boot_def x.L#typeref
    | `enum _ -> []
    | `alias a -> get_boot_def a.A#typeref


(* get all boot defintions used by (i.e. reacheable from) the module's
 * definitions *)
let get_boot_dependencies piqi =
  if !C.piqi_boot = None
  then []
  else
    let rec aux accu root_defs =
      let new_boot_defs = flatmap (get_boot_defs accu) root_defs in
      if new_boot_defs = []
      then accu
      else
        let accu = uniqq (new_boot_defs @ accu) in
        aux accu new_boot_defs
    in
    aux [] piqi.P#resolved_piqdef


let piqic_common piqi =
  (* if no definition uses "piq_any" type, "piqtype" module not be included in
   * order to avoid unnecessary dependency on Piqtype module *)
  depends_on_piq_any := Piqi_common.depends_on_piq_any piqi;

  (* indication whether the module that is being processed is a Piqi self-spec,
   * i.e. it is "piqi.org/piqi" or includes it *)
  if not !is_self_spec
  then is_self_spec := Piqi_common.is_self_spec piqi;

  (* implicitly add defintions from the boot module to the current module *)
  let boot_defs = get_boot_dependencies piqi in

  (* NOTE: alternatively, we could just include all boot defintions like that,
   * but this will produce some unnecessary (unused) parsers, generators and
   * type defintions:
   * let boot_defs = (some_of !boot_piqi).P#resolved_piqdef in
   *)
  piqi.P#resolved_piqdef <- boot_defs @ piqi.P#resolved_piqdef;
  ()


let rec get_piqi_deps piqi =
  if C.is_boot_piqi piqi
  then [] (* boot Piqi is not a dependency *)
  else
    let imports =
      List.map (fun x -> some_of x.T.Import#piqi) piqi.P#resolved_import
    in
    (* get all imports' dependencies recursively *)
    let import_deps =
      flatmap (fun piqi ->
          flatmap get_piqi_deps piqi.P#included_piqi
        ) imports
    in
    (* remove duplicate entries *)
    let deps = C.uniqq (import_deps @ imports) in
    deps @ [piqi]


let encode_embedded_piqi piqi =
  (* XXX: or just use piqi.orig_piqi and also get includes in get_piqi_deps? *)
  let res_piqi = Piqi.expand_piqi piqi in
  (* add the Module's name even if it wasn't set *)
  res_piqi.P#modname <- piqi.P#modname;
  (* generate embedded object (i.e. without field header) *)
  let iodata = T.gen_piqi res_piqi in
  Piqirun.to_string iodata


(* build a list of all import dependencies including the specified module and
 * encode each Piqi module in the list using Protobuf encoding *)
let build_piqi_deps piqi =
  let deps = get_piqi_deps piqi in
  List.map encode_embedded_piqi deps


(* common command-line arguments processing *)
let flag_normalize = ref false
let flag_gen_defaults = ref false
let flag_embed_piqi = ref false


let arg__normalize =
  "--normalize", Arg.Bool (fun x -> flag_normalize := x),
    "<true|false> normalize identifiers (default: true)"

let arg__gen_defaults =
    "--gen-defaults", Arg.Set flag_gen_defaults,
      "generate default value constructors for generated types"

let arg__embed_piqi =
    "--embed-piqi", Arg.Set flag_embed_piqi,
      "embed Piqi modules encoded in binary format in the generated code"


let init () =
  (* We have to load embedded Piqi boot modules, because the piqic spec is wider
   * than piqicc spec, which means that the default Piqtype.boot_piqi, created
   * using the narrower piqicc spec, misses some fields. *)
  Piqi.load_embedded_boot_piqi ()

