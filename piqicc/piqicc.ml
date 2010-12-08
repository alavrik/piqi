(*pp camlp4o -I $PIQI_ROOT/camlp4 pa_labelscope.cmo pa_openin.cmo *)
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


(*
 * piq interface compiler compiler
 *)


module C = Piqi_common
open Piqi_common
open Iolist


let read_file fname =
  let ch = Pervasives.open_in fname in
  let len = Pervasives.in_channel_length ch in
  let res = String.create len in
  Pervasives.really_input ch res 0 len;
  close_in ch;
  res


let embed_module ch piqi =
  let modname = some_of piqi.P#modname in
  let fname = 
    try
      Piqi_file.find_piqi modname
    with
      (* TODO: remove this nasty hack *)
      Not_found -> !Piqi_config.boot_file
  in
  let content = read_file fname in

  let content = String.escaped content in
  let code = iol [
    ios "let _ = add_embedded_piqi ";
    ios "("; ioq modname; ios ", "; ioq content; ios ")";
    eol;
  ]
  in
  Iolist.to_channel ch code


let embed_boot_modules ch =
  debug "embded boot modules(0)\n";
  let code = iod " " [
      (* the list of embedded modules *)
      ios "let embedded_piqi = ref []";
      ios "let add_embedded_piqi x = embedded_piqi := x :: !embedded_piqi";
      eol;
  ]
  in
  Iolist.to_channel ch code;

  let piqi =
    if !Piqi_config.boot_file <> ""
    then
      (* boot module is already loaded *)
      some_of !C.boot_piqi
    else
      (* fall back to using the default boot module *)
      Piqi.load_piqi_module "piqi.org/piqi-boot"
  in
  (* the list of all included modules including the current one *)
  let modules = piqi.P#included_piqi in
  (* Embed in a way that the root module will be at the end of the list and all
   * dependencies are added before *)
  List.iter (embed_module ch) (List.rev modules)


(* piq interface compiler compile *)
let piqicc ch piqi_fname piqi_impl_fname =
  trace "piqicc(0)";
  (* reload the boot module from file if it was specified using --boot option *)
  Piqi.load_boot_piqi ();

  trace "piqicc: loading piqi spec from: %s\n" piqi_fname;
  let piqi = Piqi.load_piqi piqi_fname in

  trace "piqicc: loading piqi-impl spec from: %s\n" piqi_impl_fname;
  let piqi_impl = Piqi.load_piqi piqi_impl_fname in

  trace "piqicc: piqi compiling compiler\n";
  (* TODO,XXX:
    * report invalid module name?
    * check & report piqi incompatibility
  *)
  let boot_piqi = some_of !C.boot_piqi in

  (* prepare embedded Piqi spec *)
  let piqi = P#{
    (some_of piqi.original_piqi) with
      (* using piqi.org/piqtype instead of piqi.org/piqi to generate hashcodes
       * otherwise, serial wire codes would be generated *)
      modname = Some "piqi.org/piqtype";
      ocaml_module = None; (* XXX *)

      (* unresolved, but expanded piqdef list *)
      piqdef = boot_piqi.P#extended_piqdef @ piqi.P#extended_piqdef;
      includ = [];
      import = [];
      extend = [];

      (*
      custom_field = [];

      extended_piqdef = [];
      resolved_piqdef = [];
      imported_piqdef = [];
      resolved_import = [];
      included_piqi = [];
      original_piqi = None;
      *)
  }
  in
  let piqi_binobj = Piqirun.gen_binobj T.gen_piqi piqi in

  let code = iod " " [
    ios "let parse_piqi_binobj x = ";
      ios "let _name, piqwire = Piqirun.parse_binobj x in";
      ios "parse_piqi piqwire";
    eol;

    ios "let piqi = ";
      ios "let piqi_binobj = "; ioq (String.escaped piqi_binobj);
      ios "in parse_piqi_binobj piqi_binobj";
    eol;
  ]
  in
  (* call piq interface compiler for ocaml *)
  (* TODO: move it to Piqic_config module *)
  Piqic_ocaml_types.cc_mode := true;
  (* Override supplied module name *)
  let piqi_impl = P#{piqi_impl with ocaml_module = Some "Piqtype"} in
  Piqic_ocaml.piqic piqi_impl ch;
  Iolist.to_channel ch code;

  embed_boot_modules ch


module Main = Piqi_main
open Main


(* command-line options *)
let piqi_file = ref ""
let piqi_impl_file = ref ""


let usage = "Usage: piqicc [--boot ...] --piqi ... --impl ...\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    (* XXX: arg_C; *)
    "--boot", Arg.Set_string Piqi_config.boot_file,
      "<.piqi file> use specific boot module; if not specified, piqi.org/piqi-boot will be used";
    "--piqi", Arg.Set_string piqi_file,
      "<.piqi file> specify the Piqi language spec";
    "--impl", Arg.Set_string piqi_impl_file,
      "<.piqi file> specify spec for internal representation";
  ]


let piqicc_file () =
  let error s =
    Printf.eprintf "Error: %s\n\n" s;
    Arg.usage speclist usage;
    die ""
  in
  if !piqi_file = "" then error "'--piqi' parameter is missing";
  if !piqi_impl_file = "" then error "'--impl' parameter is missing";

  let ch = Main.open_output !ofile in
  piqicc ch !piqi_file !piqi_impl_file


let run () =
  Main.parse_args () ~usage ~speclist ~min_arg_count:0 ~max_arg_count:0;
  piqicc_file ()

 
let _ =
  Main.register run "piqi compiler compiler"

