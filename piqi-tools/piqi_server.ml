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


let receive_packet buf =
  (* read a length delimited block *)
  Piqirun.parse_block buf


let send_packet ch data =
  (* generate a length delimited block *)
  let data = Piqirun.gen_block data in
  Piqirun.to_channel ch data;
  flush ch


(* read one request from the input buffer *)
let receive_request buf =
  let buf = receive_packet buf in
  Piqi_rpc.parse_request buf


(* write serialized response structure to stdout *)
let send_response response =
  send_packet stdout response


let return_ok_empty () =
  let res = Piqi_rpc.gen_response (-1) `ok_empty in
  send_response res

let return_ok data =
  let data = Piqirun.to_string data in
  let res = Piqi_rpc.gen_response (-1) (`ok data) in
  send_response res

let return_error data =
  let data = Piqirun.to_string data in
  let res = Piqi_rpc.gen_response (-1) (`error data) in
  send_response res

let return_piqi_error err =
  let res = Piqi_rpc.gen_response (-1) (`piqi_error err) in
  send_response res


let fname = "input" (* XXX *)


let parse_piq_common get_next ~is_piqi_input =
  let rec aux () =
    let obj = get_next () in
    match obj with
      | Piq.Piqtype _ -> aux () (* skip default type *)
      | Piq.Piqi _ when not is_piqi_input -> aux () (* skip embedded piqi *)
      | Piq.Piqobj obj -> Piq.Typed_piqobj obj
      | _ -> obj (* Typed_piqobj or Piqi *)
  in aux ()


(* NOTE: parsers always return either Piqi or Typed_piqobj *)


let parse_piq s ~is_piqi_input =
  let piq_parser = Piq_parser.init_from_string fname s in
  let get_next () = Piq.load_piq_obj piq_parser in
  let obj = parse_piq_common get_next ~is_piqi_input in
  (* XXX: check eof? *)
  obj


let gen_piq obj =
  let ast = Piq.gen_piq obj in
  (* XXX: add a newline? *)
  Piq_gen.to_string ast


let parse_wire s ~is_piqi_input =
  let buf = Piqirun.IBuf.of_string s in
  let get_next () = Piq.load_wire_obj buf in
  let obj = parse_piq_common get_next ~is_piqi_input in
  (* XXX: check eof? *)
  obj


let gen_wire obj =
  let buf = Piq.gen_wire obj in
  Piqirun.to_string buf


let parse_json piqtype s =
  let json_parser = Piqi_json_parser.init_from_string ~fname s in
  let obj = Piq.load_json_obj piqtype json_parser in
  (* XXX: check eof? *)
  obj


let gen_json obj =
  let json = Piq.gen_json obj in
  (* XXX: add a newline? *)
  (* XXX: make pretty-printing optional? *)
  Piqi_json_gen.pretty_to_string json


let parse_pb piqtype s =
  let buf = Piqirun.init_from_string s in
  let obj = Piq.load_pb piqtype buf in
  (* XXX: check eof? *)
  obj


let gen_pb obj =
  let buf = Piq.gen_pb obj in
  Piqirun.to_string buf


let parse_obj typename input_format data =
  (* TODO: reset wire types and codes in Piq module before reading and writing
   * wire *)
  let piqtype = Piqi_convert.get_piqtype typename in
  let is_piqi_input = (typename = "piqi") in
  let piqobj =
    match input_format with
      | `piq  -> parse_piq data ~is_piqi_input
      | `json -> parse_json piqtype data
      | `pb -> parse_pb piqtype data
      | `wire -> parse_wire data ~is_piqi_input
  in piqobj


(* "convert" call handler *)
let convert args =
  let open I.Convert_input in
  let piqobj =
    (* XXX: We need to resolve all defaults before converting to JSON *)
    if args.output_format = `json then C.resolve_defaults := true;

    parse_obj args.type_name args.input_format args.data
  in
  let output =
    match args.output_format with
      | `piq  -> gen_piq piqobj
      | `json -> gen_json piqobj
      | `pb -> gen_pb piqobj
      | `wire -> gen_wire piqobj
  in
  `ok I.Convert_output#{ data = output }


(* common error handler *)
let with_handle_piqi_errors f x =
  try f x
  with
    | C.Error (loc, s)  -> `error (strerr loc s)
    | Piqi_error s -> `error s


let convert args =
  with_handle_piqi_errors convert args


let add_one_piqi input_format data =
  (* It is enough to just read the Piqi spec in order to get it processed and
   * cached in the Piqi database automatically *)
  ignore (parse_obj "piqi" input_format data)


(* "add-piqi" call handler *)
let add_piqi args =
  let open I.Add_piqi_input in (
    List.iter (add_one_piqi args.format) args.data;
    `ok_empty
  )


let add_piqi args =
  with_handle_piqi_errors add_piqi args


exception Break


let gen_error exn =
  Printexc.to_string exn ^ " ; backtrace: " ^ Printexc.get_backtrace ()


let do_args f data =
  let buf = Piqirun.init_from_string data in
  try f buf
  with exn ->
    return_piqi_error
      ("error while parsing arguments: " ^ gen_error exn);
    raise Break


let do_run f x =
  try f x
  with exn ->
    return_piqi_error
      ("error while running function: " ^ gen_error exn);
    raise Break


let execute_request req =
  let open Piqi_rpc.Request in
  match req.name, req.data with
    | "convert", Some data -> (
        let args = do_args I.parse_convert_input data in
        match do_run convert args with
          | `ok res -> return_ok (I.gen_convert_output (-1) res)
          | `error err -> return_error (I.gen_convert_error (-1) err)
        )
    | "add-piqi", Some data -> (
        let args = do_args I.parse_add_piqi_input data in
        match do_run add_piqi args with
          | `ok_empty -> return_ok_empty ()
          | `error err -> return_error (I.gen_add_piqi_error (-1) err)
        )
    | "", None ->
        (* return the Piqi module and all the dependencies encoded as a list of
         * Piqi each encoded using Protobuf binary format *)
        let output = Piqirun.gen_list Piqirun.gen_string (-1) I.piqi in
        return_ok output
    | name, _ ->
        return_piqi_error ("invalid request: " ^ name)


let main_loop () =
  let ibuf = Piqirun.IBuf.of_channel stdin in
  while true
  do
    let request =
      try
        receive_request ibuf
      with exn -> (
        return_piqi_error
          ("error while reading command: " ^ Printexc.to_string exn);
        exit 1
      )
    in
    try execute_request request
    with Break -> ()
  done


let start_server () =
  Piqi_json.init ();
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

