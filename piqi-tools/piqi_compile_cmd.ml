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

(* use self-spec to compile %.piqi into a portable piqi-list *)


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


let compile self_spec piqi och =
  trace "getting all imported dependencies\n";
  let piqi_list = Piqi_compile.get_piqi_deps piqi in

  (* get necessary piqtypes from the self-spec *)
  let filename = !input_self_spec in

  let piqi_piqtype =
    if self_spec == C.some_of !Piqi.piqi_spec  (* is it the default embedded self-spec? *)
    then None
    else Some (Piqi_compile.get_self_spec_piqtype self_spec "piqi" ~filename)
  in
  let piqi_list_piqtype = Piqi_compile.get_self_spec_piqtype self_spec "piqi-list" ~filename in

  trace "converting modules to internal representation\n";
  (* We need to resolve all defaults before converting to JSON or XML because
   * they are dynamic encoding and their de-serializers no notion of default
   * values *)
  C.resolve_defaults := (match !output_format with
      | "json" | "xml" -> true
      | _ -> false);
  Config.flag_strict := !flag_strict;

  (* convert all modules to internal representation *)
  let piqobj_list = List.map
    (fun piqi -> Piqi.piqi_to_piqobj piqi ?piqi_piqtype ~add_codes:true)
    piqi_list
  in

  trace "writing output\n";
  match !output_format with
    | "piq" | "" ->
        (* XXX: instead of creating piqi-list, writing modules in regular .piq
         * notation *)
        let write_piq piqobj =
          let ast =
            U.with_bool Piqobj_to_piq.is_external_mode true
            (fun () -> Piqobj_to_piq.gen_obj piqobj)
          in
          let ast = Piqi_convert.piqi_ast_to_piq ast in
          Piq_gen.to_channel och ast;
          Pervasives.output_char och '\n'
        in
        List.iter write_piq piqobj_list
    | format ->
        let writer =
          match format with
            | "json" -> Piqi_convert.write_json
            | "pb" -> Piqi_convert.write_pb
            | "xml" -> Piqi_convert.write_xml
            | x -> piqi_error ("unknown output format " ^ U.quote x)
        in
        let piqobj = Piqi_compile.make_piqi_list_piqobj piqi_list_piqtype piqobj_list in
        writer och (Piqi_convert.Piqobj piqobj)


let load_self_spec filename =
  let ich = Piqi_main.open_input filename in
  let buf = Piqirun.init_from_channel ich in
  Piqi_compile.load_self_spec buf ~filename


let run_c () =
  let ich = Piqi_command.open_input !Piqi_main.ifile in
  let piqi = Piqi.load_piqi !Piqi_main.ifile ich in
  Piqi_command.close_input ();

  let self_spec =
    if !input_self_spec <> "" (* regular compilation mode mode with explicit --self-spec *)
    then (
      load_self_spec !input_self_spec
    )
    else (
      trace "--self-spec argument is missing; using the default embedded self-spec to compile\n";
      C.some_of !Piqi.piqi_spec
    )
  in
  let och = Piqi_command.open_output !Piqi_main.ofile in
  compile self_spec piqi och


let run () =
  Piqi_main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1;

  Piqi_json.init (); (* we need it for converting to JSON *)

  (* always generate extended piqi any; the goal is to standardise on the
   * representation, make command-line interface simpler and don't give users
   * unnecessary choices *)
  Piqi_config.gen_extended_piqi_any := true;

  run_c ()

 
let _ =
  Piqi_main.register_command run "compile" "use self-spec to compile %.piqi into a portable piqi-list"

