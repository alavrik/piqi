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
      Piqi.find_piqi modname
    with
      (* TODO: remove this nasty hack *)
      _ -> !Piqi_config.boot_file
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


let is_piqdef_def = function
  | `variant {V.name = "piqdef"} -> true
  | _ -> false


let gen_piqdef from_type to_type (x:T.piqdef) =
  (* piqdef -> binobj --(convert_binobj)--> binobj' *)
  let binobj = Piqirun_gen.gen_binobj T.gen_piqdef x in
  let binobj' = Piqi.convert_binobj from_type to_type binobj in

  let repr = String.escaped binobj' in
  ioq repr


(* piq interface compiler compile *)
let piqicc ch fname =
  let piqi = Piqi.load_piqi fname in

  trace "piqi compiling compiler from: %s\n" fname;
  (* TODO,XXX:
    * report failed to find piqi definition error
    * report invalid module name?
    * check & report piqi incompatibility
  *)
  (* current definition *)
  (* XXX: handle Not_found *)
  let piqdef_def = List.find is_piqdef_def T.piqdef_list in
  let piqdef_type = (piqdef_def :> T.piqtype) in

  (* new definition *)
  (* XXX: handle Not_found *)
  let piqdef_def' = List.find is_piqdef_def piqi.P#resolved_piqdef in
  let piqdef_type' = (piqdef_def' :> T.piqtype) in

  (* unresolved, but expanded piqdef list *)
  let boot_piqi = some_of !C.boot_piqi in
  let piqdef_list = boot_piqi.P#extended_piqdef @ piqi.P#extended_piqdef in

  let piqdef_list' =
    List.map (gen_piqdef piqdef_type piqdef_type') piqdef_list
  in
  let code = iod " " [
    ios "let parse_piqdef_binobj x = ";
      ios "let _name, piqwire = Piqirun_parser.parse_binobj x in";
      ios "parse_piqdef piqwire";
    eol;

    ios "let piqdef_list : piqdef list =";
      ios "let piqdef_list_repr = [";
          iod "; " piqdef_list';
        ios "]";
      ios "in List.map parse_piqdef_binobj piqdef_list_repr";
    eol;
  ] in
  (* call piq interface compiler for ocaml *)
  (* TODO: move it to Piqic_config module *)
  Piqic_ocaml_types.cc_mode := true;
  (* Override supplied module name *)
  let piqi = P#{piqi with ocaml_module = Some "Piqtype"} in
  Piqic_ocaml.piqic piqi ch;
  Iolist.to_channel ch code;

  embed_boot_modules ch


module Main = Piqi_main
open Main


let piqicc_file ifile =
  let ch = Main.open_output !ofile in
  piqicc ch ifile


let usage = "Usage: piqicc [options] <.piqi file>\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    arg_C;
    "--boot", Arg.Set_string Piqi_config.boot_file,
      "use specific boot module";
  ]


let run () =
  Main.parse_args () ~usage ~speclist;
  piqicc_file !ifile

 
let _ =
  Main.register run "piqi compiler compiler"

