(*pp camlp4o -I $PIQI_ROOT/camlp4 pa_labelscope.cmo pa_openin.cmo *)
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


(* This module implements a Piqi tools server. It reads commands from stdin and
 * writes results to stdout.
 *
 * Piqi_call is an example of a client that uses this interface *)


module C = Piqi_common
open C

module I = Piqi_tools


(* read one binobj from the input buffer *)
let parse_binobj_part buf =
  let field = Piqirun.parse_field buf in
  match field with
    | (2, piqobj) -> (* anonymous binobj *)
        None, piqobj
    | (1, nameobj) -> (* named binobj *)
        let name = Piqirun.parse_string nameobj in
        let field = Piqirun.parse_field buf in
        (match field with
          | (2, piqobj) -> Some name, piqobj
          | _ -> assert false (* TODO: provide a proper handling *)
        )
    | _ -> assert false (* TODO: provide a proper handling *)


let write_binobj ch gen_obj ?name x =
  let obj = gen_obj 2 x in
  let code =
    match name with
      | Some name -> Piqirun.OBuf.iol [ Piqirun.gen_string 1 name; obj ]
      | None -> obj
  in
  Piqirun.OBuf.to_channel ch code


(* reads one command from the input buffer *)
let read_command buf =
  match parse_binobj_part buf with
    | (Some name, data) -> (name, data)
    | _ -> failwith "invalid command format"


(* write result structure to the output buffer *)
let write_result_common status gen data =
  write_binobj stdout gen data ~name:status;
  flush stdout

let return_empty () =
  write_result_common "ok" Piqirun.gen_string ""

let return_ok gen data =
  write_result_common "ok" gen data

let return_error gen data =
  write_result_common "error" gen data

let return_piqi_error err =
  write_result_common "piqi-error" Piqirun.gen_string err


let fname = "stdin" (* XXX *)


let parse_piq s =
  let piq_parser = Piq_parser.init_from_string fname s in
  let obj = Piq.load_piq_obj piq_parser in
  (* XXX: check eof? *)
  obj


let parse_json piqtype s =
  let json_parser = Piqi_json_parser.init_from_string ~fname s in
  Piq.default_piqtype := Some piqtype;
  let obj = Piq.load_json_obj json_parser in
  (* XXX: check eof? *)
  obj


let parse_pb piqtype s =
  let buf = Piqirun.init_from_string s in
  let obj = Piq.load_pb piqtype buf in
  (* XXX: check eof? *)
  obj


let parse_wire s =
  let buf = Piqirun.IBuf.of_string s in
  let obj = Piq.load_wire_obj buf in
  (* XXX: check eof? *)
  obj


let gen_piq obj =
  let ast = Piq.gen_piq obj in
  (* XXX: add a newline? *)
  Piq_gen.to_string ast


let gen_json obj =
  let json = Piq.gen_json obj in
  (* XXX: add a newline? *)
  (* XXX: make pretty-printing optional? *)
  Piqi_json_gen.pretty_to_string json


let gen_piq_json obj =
  let json = Piq.gen_piq_json obj in
  (* XXX: add a newline? *)
  (* XXX: make pretty-printing optional? *)
  Piqi_json_gen.pretty_to_string json


let gen_pb obj =
  let buf = Piq.gen_pb obj in
  Piqirun.to_string buf


let gen_wire obj =
  let buf = Piq.gen_wire obj in
  Piqirun.to_string buf


let convert args =
  let open I.Convert_input in
  (* TODO: reset wire types and codes in Piq module before reading and writing
   * wire *)
  let input = args.data in
  let piqtype = Piqi_convert.find_piqtype args.type_name in
  let piqobj =
    match args.input_format with
      | `piq  -> parse_piq input (* piqtype? *)
      | `json | `piq_json -> parse_json piqtype input
      | `pb -> parse_pb piqtype input
      | `wire -> parse_wire input
  in
  let output =
    match args.output_format with
      | `piq  -> gen_piq piqobj
      | `json -> gen_json piqobj
      | `piq_json -> gen_piq_json piqobj
      | `pb -> gen_pb piqobj
      | `wire -> gen_wire piqobj
  in
  `ok I.Convert_output#{ data = output }


let convert args =
  try convert args
  with
    | C.Error (loc, s)  -> `error (strerr loc s)
    | Piqi_error s -> `error s


exception Break


let do_args f x =
  try f x
  with exn ->
    return_piqi_error
      ("error while parsing arguments: " ^ Printexc.to_string exn);
    raise Break


let do_run f x =
  try f x
  with exn ->
    return_piqi_error
      ("error while running function: " ^ Printexc.to_string exn);
    raise Break


let run_command name data =
  match name with
    | "convert" -> (
        let args = do_args I.parse_convert_input data in
        match do_run convert args with
          | `ok res -> return_ok I.gen_convert_output res
          | `error err -> return_error I.gen_convert_error err
        )
    | "" ->
        (* return Piqi module and all the dependencies *)
        let gen_piqi code l =
          (* encode Piqi as a list of Piqi each encoded as binobj *)
          Piqirun.gen_list Piqirun.gen_string code l
        in
        return_ok gen_piqi I.piqi
    | _ ->
        return_piqi_error ("unknown function: " ^ name)


let main_loop () =
  let ibuf = Piqirun.IBuf.of_channel stdin in
  while true
  do
    let name, data =
      try
        read_command ibuf
      with exn -> (
        return_piqi_error
          ("error while reading command: " ^ Printexc.to_string exn);
        exit 1
      )
    in
    try run_command name data
    with Break -> ()
  done


let start_server () =
  main_loop ()


module Main = Piqi_main
open Main


let usage = "Usage: piqi server [options]\nOptions:"


let speclist = Main.common_speclist @
  [
    (* TODO: flag for disallowing implicit loading of Piqi modules from files *)
  ]

let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:0;
  start_server ()

 
let _ =
  Main.register_command run "server"
    "Piqi-tools server -- reads Piqi commands from stdin and writes results to stdout"

