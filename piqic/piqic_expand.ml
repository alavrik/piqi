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
 * Similar to "piqi expand" but also sets some fields such as erlang-name for
 * type, field, option and function names.
 *)


module C = Piqi_common  
open C


module Main = Piqi_main
open Main


let rec erlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    Piqic_erlang.erlname_piqi piqi; (* run the original erlname procedure *)

    Piqic_erlang.erlname_functions piqi.P#func;
    Piqic_erlang.erlname_defs piqi.P#piqdef;
  end


let rec mlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    Piqic_ocaml.mlname_piqi piqi; (* run the original mlname procedure *)

    Piqic_ocaml.mlname_functions piqi.P#func;
    Piqic_ocaml.mlname_defs piqi.P#piqdef;
  end


(* command-line flags *)
let flag_erlang = ref false
let flag_ocaml = ref false
let flag_binary_output = ref false


let expand_file filename =
  if !flag_erlang then Piqic_erlang.init ();
  if !flag_ocaml then Piqic_ocaml.init ();

  let piqi = Piqi.load_piqi filename in
  let res_piqi = Piqi.expand_piqi piqi in

  (* chdir to the output directory *)
  Main.chdir_output !odir;

  let ch = Main.open_output !ofile in

  (* add the Module's name even if it wasn't set, this is required for custom
   * naming *)
  res_piqi.P#modname <- piqi.P#modname;

  if !flag_erlang then erlname_piqi res_piqi;
  if !flag_ocaml then mlname_piqi res_piqi;

  if not !flag_binary_output
  then
    Piqi_pp.prettyprint_piqi ch res_piqi
  else
    let code = T.gen_piqi res_piqi in
    Piqirun.to_channel ch code


let usage = "Usage: piqic expand [options] <.piqi file> [output file]\nOptions:"


(* This getopt spec must be a superset of Erlang spec, because otherwise,
 * piqic-erlang-ext and piqic-erlang-rpc will break, when called with options
 * unrecognized by "piqic expand".
 *
 * Recognized but unsupported options such as gen_defaults will be simply
 * ignored.
 *)
let speclist = Piqic_erlang.speclist @
  [
    arg_o;

    "-b", Arg.Set flag_binary_output,
      "Output expanded Piqi module encoded as a Protobuf-encoded binary";

    "--erlang", Arg.Set flag_erlang,
      "Expand Erlang names only";

    "--ocaml", Arg.Set flag_ocaml,
      "Expand OCaml names only";
  ]


let run () =
  Main.parse_args () ~usage ~speclist;

  let expand_all = not (!flag_erlang || !flag_ocaml) in
  if expand_all then (flag_ocaml := true; flag_erlang := true);

  expand_file !ifile

 
let _ =
  Main.register_command run "expand"
    "similar to \"piqi expand\" but also sets piqic-specific names"

