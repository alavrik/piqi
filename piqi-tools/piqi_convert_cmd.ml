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


module C = Piqi_common  
open C


module Main = Piqi_main
open Main


(* command-line arguments *)
let input_encoding = ref ""
let output_encoding = ref ""
let typename = ref ""
let flag_add_defaults = ref false
let flag_embed_piqi = ref false
let flag_json_omit_null_fields = ref true


let usage = "Usage: piqi convert [options] [input file] [output file]\nOptions:"


let arg__t =
    "-t", Arg.Set_string output_encoding,
    "piq|wire|pb|json|piq-json|xml output encoding (piq is used by default)"

let arg__piqtype =
    "--piqtype", Arg.Set_string typename,
    "<typename> type of converted object when converting from .pb, plain .json or .xml"

let arg__add_defaults =
    "--add-defaults", Arg.Set flag_add_defaults,
    "add default field values to converted records"

let arg__json_omit_null_fields =
    "--json-omit-null-fields", Arg.Bool (fun x -> flag_json_omit_null_fields := x),
    "true|false omit null fields in JSON output (default=true)"


let speclist = Main.common_speclist @
  [
    Piqi_main.arg__strict;
    arg_o;

    "-f", Arg.Set_string input_encoding,
    "piq|wire|pb|json|piq-json|xml input encoding";

    arg__t;
    arg__piqtype;
    arg__add_defaults;
    arg__json_omit_null_fields;

    "--embed-piqi", Arg.Set flag_embed_piqi,
    "embed Piqi dependencies, i.e. Piqi specs which the input depends on";

    Piqi_main.arg__include_extension;

    arg__;
  ]


let first_load = ref true

let load_piqi fname :Piq.obj =
  if !first_load
  then
    begin
      first_load := false;
      (* NOTE, XXX: here also loading, processing and validating all the
       * module's dependencies *)
      let piqi = Piqi.load_piqi fname in
      Piq.Piqi piqi
    end
  else
    raise Piq.EOF (* mimic the behaviour of Piq. loaders *)


let resolve_typename () =
  if !typename <> ""
  then Some (Piqi_convert.find_piqtype !typename)
  else None


(* ensuring that Piq.load_pb is called exactly one time *)
let load_pb piqtype wireobj :Piq.obj =
  if !first_load
  then
    begin
      first_load := false;
      Piq.load_pb piqtype wireobj
    end
  else
    (* XXX: print a warning if there are more input objects? *)
    raise Piq.EOF


let first_write_pb = ref true

let write_pb ch (obj: Piq.obj) =
  if !first_write_pb
  then
    begin
      first_write_pb := false;
      Piq.write_pb ch obj
    end
  else
    piqi_error "converting more than one object to \"pb\" is not allowed"


(* write only data and skip Piqi specifications and data hints *)
let write_plain ~is_piqi_input writer ch (obj: Piq.obj) =
  match obj with
    | Piq.Piqi _ when not is_piqi_input ->
        (* ignore embedded Piqi specs if we are not converting .piqi *)
        ()
    | Piq.Piqtype _ ->
        (* ignore default type names *)
        ()
    | _ -> writer ch obj


let make_reader load_f input_param =
  (fun () -> load_f input_param)


let make_reader input_encoding =
  match input_encoding with
    | "pb" when !typename = "" ->
        piqi_error "--piqtype parameter must be specified for \"pb\" input encoding"
    | "pb" ->
        let piqtype = some_of (resolve_typename ()) in
        let wireobj = Piq.open_pb !ifile in
        make_reader (load_pb piqtype) wireobj

    | "json" | "piq-json" ->
        let json_parser = Piqi_json.open_json !ifile in
        make_reader (Piq.load_piq_json_obj (resolve_typename ())) json_parser

    | "xml" when !typename = "" ->
        piqi_error "--piqtype parameter must be specified for \"xml\" input encoding"
    | "xml" ->
        let piqtype = some_of (resolve_typename ()) in
        let xml_parser = Piqi_xml.open_xml !ifile in
        make_reader (Piq.load_xml_obj piqtype) xml_parser

    | "piq" ->
        let piq_parser = Piq.open_piq !ifile in
        make_reader (Piq.load_piq_obj (resolve_typename ())) piq_parser

    | "piqi" when !typename <> "" ->
        piqi_error "--piqtype parameter is not applicable to \"piqi\" input encoding"
    | "piqi" ->
        make_reader load_piqi !ifile

    | "wire" ->
        let buf = Piq.open_wire !ifile in
        make_reader (Piq.load_wire_obj (resolve_typename ())) buf
    | x ->
        piqi_error ("unknown input encoding: " ^ x)


let make_writer ?(is_piqi_input=false) output_encoding =
  (* XXX: We need to resolve all defaults before converting to JSON or XML since
   * they are dynamic encoding, and it would be too unreliable and inefficient
   * to let a consumer decide what a default value for a field should be in case
   * if the field is missing. *)
  C.resolve_defaults := !flag_add_defaults ||
    (match output_encoding with
      | "json" | "piq-json" | "xml" -> true
      | _ -> false);
  match output_encoding with
    | "" (* default output encoding is "piq" *)
    | "piq" -> Piq.write_piq
    | "wire" -> Piq.write_wire
    | "json" ->
        write_plain Piq.write_json ~is_piqi_input
    | "piq-json" ->
        Piq.write_piq_json
    | "pb" ->
        write_plain write_pb ~is_piqi_input
    | "xml" ->
        write_plain Piq.write_xml ~is_piqi_input
    | _ ->
        piqi_error "unknown output encoding"


let seen = ref [] (* the list of seen elements *)

let is_seen x = List.memq x !seen

let add_seen x = seen := x :: !seen

let check_update_unseen x =
  let is_unseen = not (is_seen x) in
  (* add unseen element to the list of seen ones *)
  if is_unseen then add_seen x;
  is_unseen (* return true for yet unseen elements *)

let remove_update_seen l =
  List.filter check_update_unseen l


let rec get_piqi_deps piqi ~only_imports =
  if C.is_boot_piqi piqi
  then [] (* boot Piqi is not a dependency *)
  else
    (* get all includes and includes from all included modules -- only *)
    let includes = if only_imports then [piqi] else piqi.P#included_piqi in
    (* get all dependencies from imports *)
    let import_deps =
      flatmap (fun x ->
          let piqi = some_of x.T.Import#piqi in
          flatmap (get_piqi_deps ~only_imports) piqi.P#included_piqi)
        piqi.P#resolved_import
    in
    (* NOTE: imports go first in the list of dependencies *)
    let l = import_deps @ includes in
    (* remove duplicate entries *)
    C.uniqq l


let get_parent_piqi (t: T.piqtype) =
  let piqdef =
    match t with
      | #T.piqdef as x -> x
      | _ -> assert false
  in
  C.get_parent_piqi piqdef


let get_dependencies (obj :Piq.obj) ~only_imports =
  let deps =
    match obj with
      | Piq.Piqi piqi ->
          (* add piqi itself to the list of seen *)
          add_seen piqi;
          let deps = get_piqi_deps piqi ~only_imports in
          (* remove the Piqi itself from the list of deps *)
          List.filter (fun x -> x != piqi) deps
      | _ -> (
          let piqtype =
            match obj with
              | Piq.Piqtype name ->
                  Piqi_db.find_piqtype name
              | Piq.Typed_piqobj obj | Piq.Piqobj obj ->
                  Piqobj_common.type_of obj
              | _ -> assert false
          in
          let piqi = get_parent_piqi piqtype in
          (* get dependencies for yet unseen (and not yet embedded) piqi *)
          if is_seen piqi
          then []
          else get_piqi_deps piqi ~only_imports
      )
  in
  (* filter out already seen deps along with updating the list of seen deps *)
  remove_update_seen deps


let validate_options input_encoding =
  if !flag_embed_piqi
  then (
    if input_encoding = "piqi"
    then (
      if !output_encoding = "pb"
      then piqi_error "can't --embed-piqi when converting .piqi to .pb"
    )
    else ( (* input_encoding <> "piqi" *)
      match !output_encoding with
        | "json" | "xml" | "pb" ->
          piqi_warning
            "--embed-piqi doesn't have any effect when converting to .pb, .json or .xml; use .wire or .piq-json"
        | _ -> ()
    )
  )


let convert_file () =
  Piqi_convert.init ();
  Piqi_convert.set_options
    (Piqi_convert.make_options
      ~json_omit_null_fields:!flag_json_omit_null_fields
      ~use_strict_parsing:!Piqi_config.flag_strict
      ()
    );
  let input_encoding =
    if !input_encoding <> ""
    then !input_encoding
    else Piqi_file.get_extension !ifile
  in
  validate_options input_encoding;
  let reader = make_reader input_encoding in
  let is_piqi_input = (input_encoding = "piqi") in
  let writer = make_writer !output_encoding ~is_piqi_input in
  (* open output file *)
  let ofile =
    match !ofile with
      | "" when !output_encoding = "" -> "" (* print "piq" to stdout by default *)
      | "" when !ifile <> "" && !ifile <> "-" ->
          let output_extension =
            match !output_encoding with
              | "piq-json" -> "json"
              | x -> x
          in
          !ifile ^ "." ^ output_extension
      | x -> x
  in
  let och = Main.open_output ofile in

  let is_piq_output =
    match !output_encoding with
      | "" | "piq" -> true
      | _ -> false
  in
  (* main convert cycle *)
  try 
    trace "piqi convert: main loop\n";
    while true
    do
      let obj = reader () in

      (match obj with
        | Piq.Piqi piqi ->
            Piqi_db.add_piqi piqi;
            (* Preserve location information so that exising location info for
             * Piqi modules won't be discarded by subsequent Piqloc.reset()
             * calls. *)
            Piqloc.preserve ();
        | _ ->
            (* reset location db to allow GC to collect previously read objects
             *)
            Piqloc.reset ()
      );

      if !flag_embed_piqi
      then (
        trace "piqi convert: embedding Piqi\n";
        (* write yet unwirtten object's dependencies *)
        let deps = get_dependencies obj ~only_imports:(not is_piq_output) in
        List.iter (fun x -> writer och (Piq.Piqi x)) deps
      );

      (* write the output object *)
      writer och obj
    done
  with
    Piq.EOF -> ()


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  convert_file ()

 
let _ =
  Main.register_command run "convert"
    "convert data files between various encodings (piq, wire, pb, json, piq-json, xml)"

