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
 * Piq interface compiler for OCaml
 *)

module C = Piqi_common
open C
open Iolist


(* command-line flags *)
let flag_pp = ref false


let mlname_func_param func_name param_name param =
  let make_name () =
    Some (func_name ^ "_" ^ param_name)
  in
  match param with
   | None -> ()
   | Some (`alias x) ->
       x.A#ocaml_name <- make_name ()
   | Some (`record x) ->
       x.R#ocaml_name <- make_name ()
   | Some (`variant x) | Some (`enum x) ->
       x.V#ocaml_name <- make_name ()
   | Some (`list x) ->
       x.L#ocaml_name <- make_name ()


let mlname_func x =
  let open T.Func in (
    if x.ocaml_name = None then x.ocaml_name <- Piqic_ocaml_base.mlname x.name;
    let func_name = some_of x.ocaml_name in
    mlname_func_param func_name "input" x.resolved_input;
    mlname_func_param func_name "output" x.resolved_output;
    mlname_func_param func_name "error" x.resolved_error;
  )


let mlname_functions l =
  List.iter mlname_func l


module Main = Piqi_main
open Main


let ocaml_pretty_print ifile ofile =
  let cmd =
    if ofile = "-"
    then Printf.sprintf "camlp4o %s" ifile
    else Printf.sprintf "camlp4o -o %s %s" ofile ifile 
  in
  let res = Sys.command cmd in
  if res <> 0
  then piqi_error ("command execution failed: " ^ cmd)


let gen_embedded_piqi piqi =
  let l = Piqic_common_ext.build_piqi_deps piqi in
  let l = List.map (fun s -> ioq (String.escaped s)) l in
  iol [
    ios "let piqi = ["; iod ";" l; ios "]"
  ]


let gen_output_file ofile code =
  if not !flag_pp
  then
    let ch = Main.open_output ofile in
    Iolist.to_channel ch code;
    Main.close_output ()
  else
    begin
      (* prettyprint generated OCaml code using Camlp4 *)
      let tmp_file = ofile ^ ".tmp.ml" in
      (try
        let tmp_ch = open_out tmp_file in
        Iolist.to_channel tmp_ch code;
        close_out tmp_ch;
      with Sys_error s ->
        piqi_error ("error writing temporary file: " ^ s));
      ocaml_pretty_print tmp_file ofile;
      Main.add_tmp_file tmp_file;
    end


let piqic piqi =
  (* naming function parameters first, because otherwise they will be overriden
   * in Piqic_ocaml_base.mlname_defs *)
  mlname_functions piqi.P#resolved_func;

  (* call piq interface compiler for ocaml *)
  let code = Piqic_ocaml_base.piqic piqi in
  let code =
    if !Piqic_common.flag_embed_piqi
    then iol [ code; gen_embedded_piqi piqi ]
    else code
  in

  (* chdir to the output directory *)
  Main.chdir_output !odir;

  let ofile =
    match !ofile with
      | "" ->
          let modname = some_of piqi.P#ocaml_module in
          String.uncapitalize modname ^ ".ml"
      | x -> x
  in
  gen_output_file ofile code


let piqic_file ifile =
  Piqic_common.init ();
  (* load input .piqi file *)
  let piqi = Piqi.load_piqi ifile in
  piqic piqi


let usage = "Usage: piqic ocaml [options] <.piqi file>\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    arg_C;

    "--pp", Arg.Set flag_pp,
      "pretty-print output using CamlP4 (camlp4o)"; 
    Piqic_common.arg__gen_defaults;
    Piqic_common.arg__normalize;
    Piqic_common.arg__embed_piqi;
    arg__leave_tmp_files;
  ]


let run () =
  Main.parse_args () ~usage ~speclist;
  piqic_file !ifile

 
let _ =
  Main.register_command run "ocaml" "generate OCaml stubs from %.piqi"

