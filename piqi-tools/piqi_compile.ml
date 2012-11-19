(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011, 2012 Anton Lavrik

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

(* use self-spec to compile %.piqi into a portable piqi-bundle *)


module C = Piqi_common  
open C


(* command-line arguments *)
let output_format = ref ""
let input_self_spec = ref ""
let flag_strict = ref false


(* NOTE: "piqi compile" command-line interface should be idealy as stable
 * (backward-compatible) as as the Piqi self-spec; this way users can rely on
 * the tool without closely tracking piqi releases *)
let usage = "Usage: piqi compile [options] <.piqi file>\nOptions:"


let arg__t =
  "-t", Arg.Set_string output_format,
  "pb|json|xmlpiq output format (default=piq)"

(* The reason we require self-spec to be in .pb format is because it is faster
 * to parse it this way, not because it is more portable than any other format.
 * Also, it is better to have a deterministic interface when we expect input in
 * only one format -- we don't want to autodetect it when reading from stdin *)
let arg__self_spec =
  "--self-spec", Arg.Set_string input_self_spec,
  "<.pb file> input self-spec in .pb format; use '-' for stdin"

let arg__strict =
  let (name, _setter, descr) = Piqi_main.arg__strict in
  (* overrid the original setter but keep the option name and the description;
   * we do this, because although it means the same, it is applied at a later
   * stage -- we control it manually below *)
  (name, Arg.Set flag_strict, descr)


let speclist = Piqi_main.common_speclist @
  [
    arg__strict;
    Piqi_main.arg_o;
    arg__t;
    Piqi_main.arg__include_extension;
    arg__self_spec;
  ]


(* make piqi/piqi-bundle top-level record from the list of piqi piqobjs
 *
 * XXX: would it be easier to make a piqi-bundle ast and then make piqobj from
 * that ast instead of constructing the piqobj manually? *)
let make_piqi_bundle_piqobj (piqi_piqobj_list: Piqobj.obj list) :Piqobj.obj =
  (* get the "piqi" record descriptor; TODO: this can be done once at startup *)
  let piqi_bundle_record =
    try
      let piqi_spec = some_of !Piqi.piqi_spec in
      (* remove "embedded" prefix from the module name *)
      piqi_spec.P#modname <- Some "piqi";
      match Piqi_db.find_local_typedef piqi_spec.P#resolved_typedef "piqi-bundle" with
        | `record x -> x
        | _ -> assert false
    with Not_found ->
      assert false
  in
  (* get the "piqi" field descriptor; TODO: this can be done once at startup *)
  let piqi_bundle_field =
    List.find (fun f ->
      match f.F#piqtype with
        | Some t when t == !Piqi.piqi_spec_def -> true
        | _ -> false)
    piqi_bundle_record.R#field
  in
  (* make the bundle piqobj using the obtained descriptors *)
  let make_field piqobj =
    Piqobj.Field#{
      t = piqi_bundle_field; (* field descriptor *)
      obj = Some piqobj;
    }
  in
  `record Piqobj.Record#{
    t = piqi_bundle_record; (* record descriptor *)
    field = List.map make_field piqi_piqobj_list;
    unparsed_piq_fields_ref = None;
  }


let compile piqi piqtype och =
  trace "getting all imported dependencies\n";
  let piqi_list = Piqi_convert_cmd.get_piqi_deps piqi ~only_imports:true in

  trace "converting modules to internal representation\n";
  (* We need to resolve all defaults before converting to JSON or XML because
   * they are dynamic encoding and their de-serializers no notion of default
   * values *)
  C.resolve_defaults := (match !output_format with
      | "json" | "xml" -> true
      | _ -> false);
  Config.flag_strict := !flag_strict;

  (* convert all modules to internal representation *)
  let piqobj_list = List.map (fun piqi ->
      Piqi.piqi_to_piqobj piqi ?custom_piqtype:piqtype ~add_codes:true
    ) piqi_list
  in

  trace "writing output\n";
  match !output_format with
    | "piq" | "" ->
        (* XXX: instead of creating a bundle, writing modules in regular .piq
         * notation *)
        let write_piq piqobj =
          let ast =
            U.with_bool Piqobj_to_piq.is_external_mode true
            (fun () -> Piqobj_to_piq.gen_obj piqobj)
          in
          let ast = Piq.piqi_ast_to_piq ast in
          Piq_gen.to_channel och ast;
          Pervasives.output_char och '\n'
        in
        List.iter write_piq piqobj_list
    | format ->
        let writer =
          match format with
            | "json" -> Piq.write_json
            | "pb" -> Piq.write_pb
            | "xml" -> Piq.write_xml
            | x -> piqi_error ("unknown output format " ^ U.quote x)
        in
        let piqobj = make_piqi_bundle_piqobj piqobj_list in
        writer och (Piq.Piqobj piqobj)


let run_c ifile och =
  let piqi = Piqi.load_piqi ifile in
  let piqtype =
    if !input_self_spec <> "" (* regular compilation mode mode with explicit --self-spec *)
    then (
      trace "reading self-spec from %s\n" !input_self_spec;
      trace_enter ();
      Piqi_main.close_input ();
      let ich = Piqi_main.open_input !input_self_spec in
      let buf = Piqirun.init_from_channel ich in
      let self_spec =
        try
          (* TODO: we can read piqi directly using Piqi_piqi.parse_piqi, because
           * self-spec is guaranteed to not have any incompatibilities with
           * piqi_lang including functions and other parts. This makes "piqi
           * compile" start faster than if we used "Piqi.piqi_of_pb buf" *)
          Piqi.piqi_of_pb buf (* NOTE: not caching the loaded module *)
        with exn ->
          Printf.eprintf "error: failed to read self-spec from %s:\n" !input_self_spec;
          raise exn (* try to give more details about what when wrong *)
      in
      let piqi_def =
        try Piqi_db.find_local_typedef self_spec.P#resolved_typedef "piqi"
        with Not_found ->
          Printf.eprintf "invalid self-spec read from %s: no definition named \"piqi\"\n" !input_self_spec;
          piqi_error "piqi c: invalid self-spec"
      in
      trace_leave ();
      Some (piqi_def: T.typedef :> T.piqtype)
    )
    else (
      trace "--self-spec argument is missing; using the default embedded self-spec to compile\n";
      None
    )
  in
  compile piqi piqtype och


let run () =
  Piqi_main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1;

  Piqi_json.init (); (* we need it for converting to JSON *)

  (* always generate extended piqi any; the goal is to standardise on the
   * representation, make command-line interface simpler and don't give users
   * unnecessary choices *)
  Piqi_config.gen_extended_piqi_any := true;

  let och = Piqi_main.open_output !Piqi_main.ofile in
  run_c !Piqi_main.ifile och

 
let _ =
  Piqi_main.register_command run "compile" "use self-spec to compile %.piqi into a portable piqi-bundle"

