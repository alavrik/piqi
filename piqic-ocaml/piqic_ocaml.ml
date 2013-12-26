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
 * Piqi compiler for OCaml
 *)

module C = Piqic_common
open C
open Iolist


(* command-line flags *)
let flag_pp = ref false
let flag_gen_defaults = ref true  (* deprecated -- always enabled by default *)
let flag_embed_piqi = ref false
let flag_strict = ref false


let arg__pp =
  "--pp", Arg.Set flag_pp,
    "pretty-print output using CamlP4 (camlp4o)"

let arg__normalize_names =
  "--normalize-names", Arg.Bool (fun x -> Piqic_common.flag_normalize_names := x),
    "true|false turn CamlCase-style names into \"camel-case\" (default = true)"


let arg__gen_defaults =
  "--gen-defaults", Arg.Set flag_gen_defaults,
    "(depreacted) always enabled: generate default value constructors for generated types"

let arg__embed_piqi =
  "--embed-piqi", Arg.Set flag_embed_piqi,
    "embed Piqi modules encoded in binary format in the generated code"

let arg__strict =
  let (name, _setter, descr) = Piqi_main.arg__strict in
  (* override the original setter but keep the option name and the description;
   * we do this, because although it means the same, it is applied at a later
   * stage -- we control it manually below *)
  (name, Arg.Set flag_strict, descr)


(* build a list of all import dependencies including the specified module and
 * encode each Piqi module in the list using Protobuf encoding *)
let gen_embedded_piqi piqi_list =
  let l = List.map (fun x -> Piqirun.to_string (Piqi_piqi.gen_piqi x)) piqi_list in
  let l = List.map (fun s -> ioq (String.escaped s)) l in
  iol [
    ios "let piqi = ["; iod ";" l; ios "]"
  ]


let gen_ml context =
  let top_modname = C.top_modname context in
  iol [
    Piqic_ocaml_types.gen_piqi context;
    Piqic_ocaml_in.gen_piqi context;
    Piqic_ocaml_out.gen_piqi context;

    ios "include "; ios top_modname; eol;

    Piqic_ocaml_defaults.gen_piqi context;

    if !flag_embed_piqi
    then iol [ gen_embedded_piqi context.modules ]
    else iol [];
  ]


let ocaml_pretty_print ifile ofile =
  let cmd =
    if ofile = "-"
    then Printf.sprintf "camlp4o %s" ifile
    else Printf.sprintf "camlp4o -o %s %s" ofile ifile 
  in
  let res = Sys.command cmd in
  if res <> 0
  then Piqi_common.piqi_error ("command execution failed: " ^ cmd)


let gen_output_file ofile code =
  if not !flag_pp
  then
    let ch = Piqi_main.open_output ofile in
    Iolist.to_channel ch code;
    Piqi_main.close_output ()
  else
    begin
      (* prettyprint generated OCaml code using Camlp4 *)
      let tmp_file = ofile ^ ".tmp.ml" in
      (try
        let tmp_ch = open_out tmp_file in
        Iolist.to_channel tmp_ch code;
        close_out tmp_ch;
      with Sys_error s ->
        Piqi_common.piqi_error ("error writing temporary file: " ^ s));
      ocaml_pretty_print tmp_file ofile;
      Piqi_main.add_tmp_file tmp_file;
    end


let piqic context =
  let code = gen_ml context in

  (* chdir to the output directory *)
  Piqi_main.chdir_output !Piqi_main.odir;

  let piqi = context.piqi in
  let modname = some_of piqi.P#ocaml_module in
  let ofile = String.uncapitalize modname ^ ".ml" in
  gen_output_file ofile code


let load_self_spec () =
  Piqi_common.trace "reading embedded self-spec\n";
  Piqi_common.trace_enter ();
  let self_spec_bin = List.hd Piqi_piqi.piqi in
  let buf = Piqirun.init_from_string self_spec_bin in
  let self_spec =
    try
      (* TODO: we can read piqi directly using Piqi_piqi.parse_piqi, because
       * self-spec is guaranteed to not have any incompatibilities with
       * piqi_lang including functions and other parts. This makes "piqi
       * compile" start faster than if we used "Piqi.piqi_of_pb buf" *)
      Piqi.piqi_of_pb buf (* NOTE: not caching the loaded module *)
    with exn ->
      Printf.eprintf "error: failed to load embedded self-spec\n";
      raise exn (* try to give more details about what when wrong *)
  in
  Piqi_common.trace_leave ();
  self_spec


(* this is the same as calling "piqi compile ..." and reading its output but,
 * instead, we are just using the library functions *)
let piqi_compile_piqi ifile =
  let self_spec = load_self_spec () in
  let piqi = Piqi.load_piqi ifile in
  Piqi_compile.compile self_spec piqi ~strict:!flag_strict


let load_piqi_list ifile =
  let bin = piqi_compile_piqi ifile in
  (* read the compiled piqi bundle *)
  let buf = Piqirun.init_from_string bin in
  let bundle = Piqi_piqi.parse_piqi_bundle buf in
  (* return the list of piqi modules: list of dependencies @ [input module] *)
  bundle.Piqi_piqi.Piqi_bundle.piqi


let piqic_file ifile =
  Piqi_config.add_include_extension "ocaml";

  (* load input .piqi file and its dependencies *)
  let piqi_list = load_piqi_list ifile in
  let context = C.init piqi_list in
  piqic context


let speclist = Piqi_main.common_speclist @
  [
    Piqi_main.arg_C;
    arg__strict;
    Piqi_main.arg__include_extension;  (* -e *)
    arg__normalize_names;
    arg__pp;
    arg__gen_defaults;
    Piqi_main.arg__leave_tmp_files;
    arg__embed_piqi;

    (* TODO: multiformat serialization --ext | --multiformat | --mf *)
  ]


let usage = "Usage: piqic-ocaml [options] <.piqi file>\nOptions:"


let run () =
  Piqi_main.parse_args () ~usage ~speclist;
  piqic_file !Piqi_main.ifile

 
let _ =
  Piqi_main.register run "generate OCaml stubs from %.piqi";
  Piqi_main.run ()

