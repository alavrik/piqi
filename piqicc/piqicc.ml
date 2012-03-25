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
 * piq interface compiler compiler
 *)


module C = Piqi_common
open Piqi_common
open Iolist


let read_file fname =
  let ch = Pervasives.open_in_bin fname in
  let len = Pervasives.in_channel_length ch in
  let res = String.create len in
  Pervasives.really_input ch res 0 len;
  close_in ch;
  res


let embed_module ch ?fname piqi =
  let modname = some_of piqi.P#modname in
  let fname =
    match fname with
      | Some x -> x
      | None ->
          (* TODO: store file name in the Piqi structure while loading *)
          Piqi_file.find_piqi modname
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


let embed_boot_modules ch boot_fname =
  debug "embded boot modules(0)\n";
  let code = iod " " [
      (* the list of embedded modules *)
      ios "let embedded_piqi = ref []";
      ios "let add_embedded_piqi x = embedded_piqi := x :: !embedded_piqi";
      eol;
  ]
  in
  Iolist.to_channel ch code;

  let piqi_boot = some_of !C.piqi_boot in
  (* the list of all included modules including the current one *)
  let modules = piqi_boot.P#included_piqi in
  (* Embed in a way that the root module will be at the end of the list and all
   * dependencies are added before *)
  embed_module ch piqi_boot ~fname:boot_fname;
  List.iter (embed_module ch) (List.tl (List.rev modules))


(* assign field/option/enum codes as a hash(name); by default serial wire codes
 * would be used *)
let add_hashcodes defs =
  (* add hash-based field and option codes instead of auto-enumerated ones *)
  Piqi_wire.add_hashcodes defs;
  (* check for hash conflicts and pre-order fields by hash codes *)
  Piqi_wire.process_defs defs;
  ()


(* piq interface compiler compile *)
let piqicc ch boot_fname lang_fname impl_fname =
  trace "piqicc(0)\n";

  (* reload the boot module from file if it was specified using --boot option *)
  trace "piqicc: loading piqi-boot spec from: %s\n" boot_fname;
  Piqi.load_boot_piqi boot_fname;

  trace "piqicc: loading piqi-lang spec from: %s\n" lang_fname;
  let piqi = Piqi.load_piqi lang_fname in

  trace "piqicc: loading piqi-impl spec from: %s\n" impl_fname;
  let piqi_impl = Piqi.load_piqi impl_fname in

  trace "piqicc: piqi compiling compiler\n";
  (* TODO: check & report piqi incompatibility *)

  (* find the Piqi module that corresponds to the Piqi language
   * self-specification -- a basic input sanity check *)
  let _ =
    try Piqi_db.find_piqi "piqi.org/piqi-lang"
    with Not_found ->
      piqi_error "missing imported module piqi.org/piqi-lang"
  in
  (* find the Piqi module that corresponds to the Piqi self-specification *)
  let piqi_spec =
    try Piqi_db.find_piqi "piqi.org/piqi"
    with Not_found ->
      piqi_error "missing imported module piqi.org/piqi"
  in
  let piqi_boot = some_of !C.piqi_boot in

  (* prepare embedded Piqi language spec *)
  let piqi_lang = P#{
    (Piqi.expand_piqi piqi) with
      modname = piqi.P#modname;
      ocaml_module = None; (* XXX *)
  }
  in
  (* prepare embedded Piqi self-specification *)
  let piqi_spec = P#{
    (Piqi.expand_piqi piqi_spec) with
      modname = piqi_spec.P#modname;
      ocaml_module = None; (* XXX *)
      custom_field = []; (* XXX *)
  }
  in
  (* prepare embedded Piqi boot spec *)
  let piqi_boot = P#{
    (Piqi.expand_piqi piqi_boot) with
      modname = piqi_boot.P#modname;
      ocaml_module = None; (* XXX *)
  }
  in

  (* convert piqi to binobj using piqi self-definition that we've just loaded *)
  let piqi_def = Piqi_db.find_local_piqdef piqi "piqi" in
  let piqi_type = (piqi_def: T.piqdef :> T.piqtype) in
  let gen_piqi_binobj piqi =
    add_hashcodes piqi.P#piqdef;
    (* Old method that does not account for possible Piqi lang
     * self-specification refactoring
     *
     Piqirun.gen_binobj T.gen__piqi piqi
     *)
    Piqloc.pause ();
    let piqi_ast = Piqi_pp.piqi_to_ast piqi ~simplify:true in

    (* XXX: setting this option in order to delay, and then ignore all parsing
     * warnings *)
    Piqobj_of_piq.delay_unknown_warnings := true;

    let piqobj = Piqobj_of_piq.parse_obj piqi_type piqi_ast in

    let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
    Piqi.check_unknown_fields unknown_fields ["ocaml-name"; "ocaml-type"];

    let res = Piqobj_to_wire.gen_binobj piqobj in
    Piqloc.resume ();
    res
  in

  trace "gen_piqi_binobj piqi_lang\n";
  let piqi_binobj = gen_piqi_binobj piqi_lang in
  trace "gen_piqi_binobj piqi_spec\n";
  let piqi_spec_binobj = gen_piqi_binobj piqi_spec in
  trace "gen_piqi_binobj piqi_boot\n";
  let piqi_boot_binobj = gen_piqi_binobj piqi_boot in

  let code = iod " " [
    ios "let parse_piqi_binobj x = ";
      ios "Piqirun.parse_binobj parse_piqi x";
    eol;

    ios "let piqi_lang = ";
      ios "let piqi_lang_binobj = "; ioq (String.escaped piqi_binobj);
      ios "in parse_piqi_binobj piqi_lang_binobj";
    eol;

    ios "let piqi_spec = ";
      ios "let piqi_spec_binobj = "; ioq (String.escaped piqi_spec_binobj);
      ios "in parse_piqi_binobj piqi_spec_binobj";
    eol;

    ios "let piqi_boot = ";
      ios "let piqi_boot_binobj = "; ioq (String.escaped piqi_boot_binobj);
      ios "in parse_piqi_binobj piqi_boot_binobj";
    eol;
  ]
  in
  (* call piq interface compiler for ocaml *)
  (* TODO: move it to Piqic_config module *)
  Piqic_ocaml_types.cc_mode := true;
  (* generate default values for generated OCaml types *)
  Piqic_common.flag_gen_defaults := true;
  (* Override supplied module name *)
  let piqi_impl = P#{piqi_impl with ocaml_module = Some "Piqtype"} in
  let code = iol [
    Piqic_ocaml.gen_ocaml_code piqi_impl;
    code;
  ]
  in
  Iolist.to_channel ch code;

  embed_boot_modules ch boot_fname


module Main = Piqi_main
open Main


(* command-line options *)
let piqi_boot_file = ref ""
let piqi_lang_file = ref ""
let piqi_impl_file = ref ""


let usage = "Usage: piqicc --boot ... --piqi ... --impl ...\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    (* XXX: arg_C; *)
    "--boot", Arg.Set_string piqi_boot_file,
      "<.piqi file> specify a Piqi boot module";
    "--lang", Arg.Set_string piqi_lang_file,
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
  if !piqi_lang_file = "" then error "'--lang' parameter is missing";
  if !piqi_impl_file = "" then error "'--impl' parameter is missing";
  if !piqi_boot_file = "" then error "'--boot' parameter is missing";

  let ch = Main.open_output !ofile in
  piqicc ch !piqi_boot_file !piqi_lang_file !piqi_impl_file


let run () =
  Main.parse_args () ~usage ~speclist ~min_arg_count:0 ~max_arg_count:0;
  piqicc_file ()

 
let _ =
  Main.register run "piqi compiler compiler"

