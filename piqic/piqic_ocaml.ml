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
 * piq interface compiler for OCaml
 *)

open Piqi_common
open Piqic_common


(*
 * set ocaml names if not specified by user
 *)


(* ocaml name of piqi name *)
let ocaml_name n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  dashes_to_underscores n


let ocaml_lcname n = (* lowercase *)
  String.uncapitalize (ocaml_name n)


let ocaml_ucname n = (* uppercase *)
  String.capitalize (ocaml_name n)


let ocaml_name n = (* preserve the original name *)
  ocaml_name (String.copy n)


let mlname n =
  Some (ocaml_lcname n)


(* variant of mlname for optional names *)
let mlname' n =
  (* XXX: ocaml name must be resolved here or later as it is implemented now? *)
  match n with
    | None -> n
    | Some n -> mlname n


(* XXX: check name validity *)
let check_ocaml_name name ocaml_name =
  if name = None && ocaml_name <> None
  then ()
  (* XXX
     error ocaml_name
       "language-specific names can not be used without piqi name"
  *)


let mlname_field x =
  let open Field in
  if x.ocaml_name = None then x.ocaml_name <- mlname' x.name


let mlname_record x =
  let open Record in
  (if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
   List.iter mlname_field x.field)


let mlname_option x =
  let open Option in
  if x.ocaml_name = None then x.ocaml_name <- mlname' x.name


let mlname_variant x =
  let open Variant in
  (if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
   List.iter mlname_option x.option)


let mlname_enum x =
  let open Enum in
  (if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
   List.iter mlname_option x.option)


let mlname_alias x =
  let open Alias in
  if x.ocaml_name = None then x.ocaml_name <- mlname x.name


let mlname_list x =
  let open L in
  if x.ocaml_name = None then x.ocaml_name <- mlname x.name


let mlname_piqdef = function
  | `record x -> mlname_record x
  | `variant x -> mlname_variant x
  | `enum x -> mlname_enum x
  | `alias x -> mlname_alias x
  | `list x -> mlname_list x


let mlname_defs (defs:T.piqdef list) =
  List.iter mlname_piqdef defs

let mlmodname n =
  Some (ocaml_ucname (some_of n))


let rec mlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    if piqi.ocaml_module = None then piqi.ocaml_module <- mlmodname piqi.modname;
    mlname_defs piqi.P#resolved_piqdef;
    mlname_defs piqi.P#imported_piqdef;
    mlname_imports piqi.P#resolved_import;
  end

and mlname_imports imports = List.iter mlname_import imports

and mlname_import import =
  let open Import in
  begin
    mlname_piqi (some_of import.piqi);
    import.ocaml_name <- Some (ocaml_ucname (some_of import.name))
  end


open Iolist


let depends_on_piq_any x =
  let is_any x = (unalias (piqtype x)) = `any in
  let is_any_opt = function
    | Some x -> is_any x
    | None -> false
  in
  match x with
    | `record x -> List.exists (fun x -> is_any_opt x.F#typeref) x.R#field
    | `variant x -> List.exists (fun x -> is_any_opt x.O#typeref) x.V#option
    | `list x -> is_any x.L#typeref
    | `enum _ -> false
    | `alias _ -> false (* don't check aliases, we do unalias instead *)


let piqic (piqi: T.piqi) ch =
  (* set ocaml names which are not specified by user *)
  mlname_piqi piqi;

  (* if no definition uses "piq_any" type, piq_any aliase will be excluded in
   * order to avoid unnecessary dependency on Piqtype module *)
  Piqic_ocaml_types.depends_on_piq_any :=
    List.exists depends_on_piq_any piqi.P#resolved_piqdef;

  (* set current module's name *)
  Piqic_ocaml_types.top_modname := some_of piqi.P#ocaml_module;

  (* NOTE: generating them in this order explicitly in order to avoid
   * right-to-left evaluation if we put this code inside the list *)
  let c1 = Piqic_ocaml_types.gen_piqi piqi in
  let c2 = Piqic_ocaml_in.gen_piqi piqi in
  let c3 = Piqic_ocaml_out.gen_piqi piqi in

  let code = iol [ c1; c2; c3 ]
  in
  Iolist.to_channel ch code


module Main = Piqi_main
open Main


(* command-line flags *)
let flag_pp = ref false


let ocaml_pretty_print ifile ofile =
  let cmd =
    if ofile = "-"
    then Printf.sprintf "camlp4o %s" ifile
    else Printf.sprintf "camlp4o -o %s %s" ofile ifile 
  in
  let res = Sys.command cmd in
  if res <> 0
  then piqi_error ("command execution failed: " ^ cmd)


let piqic_file ifile =
  (* obtain modname of the file by cutting out the directory and extension
   * parts *)
  let modname = Piqi_file.basename ifile in

  (* open output .ml file *)
  (*
  let modname = some_of piqi.T.Piqi.ocaml_name in
  let modname = String.lowercase modname in
  let ofile = modname ^ ".ml" in
  *)
  (* load input .piqi file *)
  let piqi = Piqi.load_piqi ifile in

  (* chdir to the output directory *)
  Main.chdir_output !odir;

  let ofile =
    match !ofile with
      | "" -> modname ^ ".ml"
      | x -> x
  in

  (* call piq interface compiler for ocaml *)
  if not !flag_pp
  then
    let ch = Main.open_output ofile in
    piqic piqi ch
  else
    begin
      (* prettyprint generated OCaml code using Camlp4 *)
      let tmp_file = modname ^ ".tmp.ml" in
      Main.add_tmp_file tmp_file;
      (try
        let tmp_ch = open_out tmp_file in
        piqic piqi tmp_ch;
        close_out tmp_ch;
      with Sys_error s ->
        piqi_error ("error writing temporary file: " ^ s));
      ocaml_pretty_print tmp_file ofile
    end


let usage = "Usage: piqic ocaml [options] <.piqi file>\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    arg_C;

    "--pp", Arg.Set flag_pp,
      "pretty-print output using CamlP4 (camlp4o)"; 

    arg__leave_tmp_files;
  ]


let run () =
  Main.parse_args () ~usage ~speclist;
  piqic_file !ifile

 
let _ =
  Main.register_command run "ocaml" "generate OCaml stubs from %.piqi"

