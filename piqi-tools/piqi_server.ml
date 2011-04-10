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

module I = Piqi_tools_piqi


(* serialize and send one length-delimited packet to the output channle *)
let send_packet ch data =
  (* generate a length delimited block *)
  let data = Piqirun.gen_block data in
  Piqirun.to_channel ch data;
  flush ch


(* receive one length-delimited packet from the input channle *)
let receive_packet ch =
  (* read a length delimited block *)
  let buf = Piqirun.IBuf.of_channel ch in
  Piqirun.parse_block buf


(* encode and write request/response structure to the output channel *)
let send_request ch request =
  let request = Piqi_rpc_piqi.gen_request request in
  send_packet ch request


let send_response ch response =
  let response = Piqi_rpc_piqi.gen_response response in
  send_packet ch response


(* read and decode one request/response structure from the input channel *)
let receive_request ch =
  let buf = receive_packet ch in
  Piqi_rpc_piqi.parse_request buf


let receive_response ch =
  let buf = receive_packet ch in
  Piqi_rpc_piqi.parse_response buf


(* utility functions for constructing Piqi_rpc responses *)
let return_ok_empty () =
  `ok_empty

let return_ok data =
  let data = Piqirun.to_string data in
  `ok data

let return_error data =
  let data = Piqirun.to_string data in
  `error data

let return_rpc_error err =
  `rpc_error err


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


let gen_json ?(pp=true) obj =
  let json = Piq.gen_json obj in
  if pp
  then
    Piqi_json_gen.pretty_to_string json
  else
    Piqi_json_gen.to_string json


let parse_pb piqtype s =
  let buf = Piqirun.init_from_string s in
  let obj = Piq.load_pb piqtype buf in
  (* XXX: check eof? *)
  obj


let gen_pb obj =
  let buf = Piq.gen_pb obj in
  Piqirun.to_string buf


let parse_xml piqtype s =
  let xml_parser = Piqi_xml.init_from_string ~fname s in
  let obj = Piq.load_xml_obj piqtype xml_parser in
  (* XXX: check eof? *)
  obj


let gen_xml obj =
  let xml = Piq.gen_xml obj in
  (* XXX: make pretty-printing optional? *)
  Piqi_xml.xml_to_string xml


let parse_obj typename input_format data =
  let piqtype = Piqi_convert.get_piqtype typename in
  let is_piqi_input = (typename = "piqi") in
  let piqobj =
    match input_format with
      | `piq  -> parse_piq data ~is_piqi_input
      | `json -> parse_json piqtype data
      | `pb -> parse_pb piqtype data
      | `xml -> parse_xml piqtype data
      (*
      | `wire -> parse_wire data ~is_piqi_input
      *)
  in piqobj


(* "convert" call handler *)
let convert args =
  let open I.Convert_input in
  let piqobj =
    (* XXX: We need to resolve all defaults before converting to JSON or XML *)
    C.with_resolve_defaults
      (args.output_format = `json || args.output_format = `xml)
      (parse_obj args.type_name args.input_format) args.data
  in
  let output =
    match args.output_format with
      | `piq  -> gen_piq piqobj
      | `json -> gen_json piqobj ~pp:args.pretty_print
      | `pb -> gen_pb piqobj
      | `xml -> gen_xml piqobj
      (*
      | `wire -> gen_wire piqobj
      *)
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
  match parse_obj "piqi" input_format data with
    | Piq.Piqi piqi ->
        (match input_format with
          | `pb | `json | `xml ->
              (* cache Piqi spec *)
              Piqi_db.add_piqi piqi
          | `piq (* | `wire *) ->
              (* for Piq and Wire formats is enough to just read the Piqi spec
               * in order to get it processed and cached in the Piqi database
               * automatically *)
              ()
        )
    | _ -> assert false


(* "add-piqi" call handler *)
let add_piqi args =
  let open I.Add_piqi_input in (
    List.iter (add_one_piqi args.format) args.data;
    `ok_empty
  )


let add_piqi args =
  with_handle_piqi_errors add_piqi args


exception Break of Piqi_rpc_piqi.response


let do_args f data =
  let data =
    match data with
      | Some x -> x
      | None ->
          let response = return_rpc_error `missing_input in
          raise (Break response)
  in
  let buf = Piqirun.init_from_string data in
  try f buf
  with exn ->
    let response = return_rpc_error (`invalid_input (string_of_exn exn)) in
    raise (Break response)


let do_run f x =
  try f x
  with exn ->
    let response = return_rpc_error
      (`internal_error ("error while running function: " ^ string_of_exn exn))
    in
    raise (Break response)


let execute_request req =
  let open Piqi_rpc_piqi.Request in
  match req.name, req.data with
    | "convert", data -> (
        let args = do_args I.parse_convert_input data in
        match do_run convert args with
          | `ok res -> return_ok (I.gen_convert_output res)
          | `error err -> return_error (I.gen_convert_error err)
        )
    | "add-piqi", data -> (
        let args = do_args I.parse_add_piqi_input data in
        match do_run add_piqi args with
          | `ok_empty -> return_ok_empty ()
          | `error err -> return_error (I.gen_add_piqi_error err)
        )
    | "ping", None ->
        return_ok_empty ()
    | "", None ->
        (* return the Piqi module and all the dependencies encoded as a list of
         * Piqi each encoded using Protobuf binary format *)
        let output = Piqirun.gen_list Piqirun.gen_string (-1) I.piqi in
        return_ok output
    | "", Some _ ->
        return_rpc_error
          (`invalid_input "no input is expected for 'get_piqi' command")
    | name, _ ->
        return_rpc_error `unknown_function


(* read one request from stdin, execute the request, and write the response to
 * stdout *)
let main_loop () =
  while true
  do
    let request =
      try
        receive_request stdin
      with exn -> (
        let response = return_rpc_error
          (`protocol_error
            ("error while reading command: " ^ Printexc.to_string exn))
        in
        send_response stdout response;
        exit 1
      )
    in
    let response =
      try execute_request request
      with Break x -> x
    in
    send_response stdout response;

    (* reset location db to allow GC to collect previously read objects *)
    Piqloc.reset ();
    (* XXX: run garbage collection on the minor heap to free all memory used for
     * the request -- testing has not reveal any performance penalty for doing
     * this *)
    Gc.minor ();
  done


let start_server () =
  Piqi_convert.init ();
  (* exit on SIGPIPE without printing a message about uncaught exception *)
  Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ ->
    (* have to close all channels explicilty to prevent getting an uncaught
     * sigpipe during execution of at_exit *)
    close_in_noerr stdin;
    close_out_noerr stdout;
    close_out_noerr stderr;
    exit 0
  ));
  main_loop ()


module Main = Piqi_main
open Main


let usage = "Usage: piqi server [options]\nOptions:"


let speclist = Main.common_speclist @
  [
    (* XXX: disable warnings by default in order to prevent printing them on
     * stderr? *)
  ]

let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:0;

  (* reset Piqi module lookup paths in order to prevent them from getting loaded
   * from the filesystem; now, they can only be loaded using "add_piqi" RPC call
   *)
  Config.reset_paths ();

  start_server ()

 
let _ =
  Main.register_command run "server"
    "Piqi-tools RPC-server -- reads commands from stdin and writes results to stdout"

