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


module C = Piqi_common  
open C


module Main = Piqi_main
open Main


(* command-line arguments *)
let input_encoding = ref ""
let output_encoding = ref ""
let typename = ref ""
let flag_add_defaults = ref false


let usage = "Usage: piqi convert [options] [input file] [output file]\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;

    "-f", Arg.Set_string input_encoding,
    "piq|wire|pb input encoding";

    "-t", Arg.Set_string output_encoding,
    "piq|wire|pb output encoding (piq is used by default)";

    "--piqtype", Arg.Set_string typename,
    "<typename> type of converted object when converting from .pb";

    "--add-defaults", Arg.Set flag_add_defaults,
    "add field default values while converting records";

     arg__;
  ]


let get_piqtype typename =
  if not (Piqi_name.is_valid_typename typename)
  then 
    piqi_error ("invalid type name: " ^ typename);

  Piqi.init (); (* TODO: get rid of it, see Piq.init for details *)

  try Piqi_db.find_piqtype typename
  with Not_found ->
    piqi_error ("unknown type: " ^ typename)


(* TODO:
 * load piqi, convert it to ast then parse to obj using piqi.org/piqi/piqi
 * OR
 * load piqi, convert it back to obj using embedded piqi.org/piqi/piqi
 * OR
 * load piqi, while loading piqi memorise inetrmediate piqobj
 *
 * embed piqi.org/piqi anyway *)
let do_load_piqi fname =
  let ast = Piqi.read_piqi fname in
  let piqtype = get_piqtype "piqi.org/piqi/piqi" in
  let obj = Piqobj_of_piq.parse_obj piqtype ast in
  obj


let first_load = ref true

let load_piqi fname :Piq.obj =
  if !first_load
  then
    begin
      first_load := false;
      let obj = do_load_piqi fname in
      Piq.Typed_piqobj obj
    end
  else
    raise Piq.EOF (* mimic the behaviour of Piq. loaders *)


(* ensuring that Piq.load_pb is called exactly one time *)
let load_pb piqtype wireobj :Piq.obj =
  if !first_load
  then
    begin
      first_load := false;
      let obj = Piq.load_pb piqtype wireobj in
      Piq.Typed_piqobj obj
    end
  else
    raise Piq.EOF


let make_reader load_f input_param =
  (fun () -> load_f input_param)


let init_json_writer enc =
  Piqi_json.init ();
  (* XXX: We need to resolve all defaults before converting to JSON
   * since it is a dynamic encoding *)
  flag_add_defaults := true;
  ()


let init_json_reader enc =
  Piqi_json.init ();
  if !typename <> ""
  then
    let piqtype = get_piqtype !typename in
    Piq.default_piqtype := Some piqtype
  else ()


let get_reader = function
  | "pb" when !typename = "" ->
      piqi_error "--piqtype parameter must be specified for \"pb\" input encoding"
  | "pb" ->
      let piqtype = get_piqtype !typename in
      let wireobj = Piq.open_pb !ifile in
      make_reader (load_pb piqtype) wireobj
  | "json" | "piq-json" ->
      init_json_reader ();
      let json_parser = Piqi_json.open_json !ifile in
      make_reader Piq.load_json_obj json_parser
  | _ when !typename <> "" ->
      piqi_error "--piqtype parameter is applicable only to \"pb\" or \"json\" input encodings"
  | "piq" ->
      let piq_parser = Piq.open_piq !ifile in
      make_reader Piq.load_piq_obj piq_parser
  | "piqi" -> 
      make_reader load_piqi !ifile
  | "wire" ->
      let buf = Piq.open_wire !ifile in
      make_reader Piq.load_wire_obj buf
  | _ ->
      piqi_error "unknown input encoding"


let first_write_pb = ref true

let write_pb ch (obj: Piq.obj) =
  match obj with
    | Piq.Typed_piqobj obj ->
        if !first_write_pb
        then
          begin
            first_write_pb := false;
            Piq.write_pb ch obj
          end
        else
          piqi_error "converting multiple objects to \"bp\" is disallowed"
    | _ ->
          piqi_error "only typed object can be converted to \"pb\""


let convert_file () =
  let input_encoding =
    if !input_encoding <> ""
    then !input_encoding
    else Piqi_file.get_extension !ifile
  in
  let reader = get_reader input_encoding in
  let writer =
    match !output_encoding with
      | "" (* default output encoding is "piq" *)
      | "piq" -> Piq.write_piq
      | "wire" -> Piq.write_wire
      | "json" ->
          init_json_writer ();
          Piq.write_json
      | "piq-json" ->
          init_json_writer ();
          output_encoding := "json";
          Piq.write_piq_json
      | "pb" -> write_pb
      | _ -> piqi_error "unknown output encoding"
  in
  let ofile =
    match !ofile with
      | "" when !output_encoding = "" -> "" (* print "piq" to stdout by default *)
      | "" when !ifile <> "" && !ifile <> "-" ->
          !ifile ^ "." ^ !output_encoding
      | x -> x
  in
  let och = Main.open_output ofile in

  (* XXX, TODO: unify this parameter across all readers, i.e. make it global *)
  Piqobj_of_wire.resolve_defaults := !flag_add_defaults;
  Piqobj_of_piq.resolve_defaults := !flag_add_defaults;

  (* main convert cycle *)
  try 
    while true
    do
      let obj = reader () in
      (* reset location db to allow GC to collect previously read objects *)
      Piqloc.reset ();
      writer och obj
    done
  with
    Piq.EOF -> ()


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  convert_file ()

 
let _ =
  Main.register_command run "convert"
    "convert data files between various encodings (piq, wire, pb, json, piq-json)"

