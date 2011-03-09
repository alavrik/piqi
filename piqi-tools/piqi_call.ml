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


(* This module implements a Piqi RPC client. It reads function's arguments from
 * command-line options, sends commands via Unix pipe or HTTP to servers and
 * prints responses.
 *)


module C = Piqi_common
open C


(* command-line arguments *)
let output_encoding = Piqi_convert.output_encoding
let flag_get_piqi = ref false


(* values of HTTP headers when making Piqi-RPC requests *)
let content_type = "application/x-protobuf"
let accept = content_type
let user_agent = "Piqi/" ^ Piqi_version.version


let string_of_rpc_error = function
  | `unknown_function -> "unknown function"
  | `missing_input -> "missing input"
  | `invalid_input err -> "invalid input: " ^ err
  | `invalid_output err -> "invalid output: " ^ err
  | `internal_error err -> "internal error: " ^ err
  | `service_unavailable err -> "service unavailable: " ^ err
  | `protocol_error err -> "protocol error: " ^ err


let call_local_server ((ich, och) as _handle) func_name data =
  trace "piqi_call: calling %s\n" func_name;
  let request = Piqi_rpc.Request#{name = func_name; data = data} in
  Piqi_server.send_request och request;
  match Piqi_server.receive_response ich with
    | `rpc_error err ->
        piqi_error ("local rpc error: " ^ string_of_rpc_error err)
    | x -> x


let get_content_type status headers =
  match Piqi_http.get_header "content-type" headers with
    | None when status = 204 -> "" (* "No data" therefore no Content-Type *)
    | None ->
        piqi_error
          "HTTP rpc error: Content-Type header is missing in server's response"
    | Some x -> x


let bad_content_type ct =
  piqi_error (Printf.sprintf
    "HTTP rpc error: unexpected Content-Type for status code 200: %s" ct)


let call_http_server url body =
  trace "piqi_call: calling %s\n" url;
  let status, headers, body =
    Piqi_http.post url ~accept ~content_type ~user_agent ?body
  in
  let ct = get_content_type status headers in
  match status with
    | 204 -> (* "No Data" *)
        `ok_empty
    | 200 when ct = content_type -> (* "OK" *)
        `ok body
    | 200 ->
        bad_content_type ct
    | 500 when ct = content_type ->
        (* "Internal Server Error" with the application Content-Type header
         * means application error *)
        `error body
    | _ ->
        (* all other HTTP codes either mean `rpc_error or some other HTTP
         * transport or server-related error *)
        piqi_error (Printf.sprintf
          "HTTP rpc error: (status code = %d\n)\n%s" status body)


(* returns the last element of the list *)
let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::t -> last t


let init_piqi_common data =
  trace "piqi_call: init Piqi modules returned by the server\n";
  let buf = Piqirun.init_from_string data in
  let bin_piqi_list = Piqirun.parse_list Piqirun.parse_string buf in
  (* decode piqi modules *)
  let piqi_list =
      List.map (fun x ->
          let buf = Piqirun.init_from_string x in
          T.parse_piqi buf
        ) bin_piqi_list
  in
  (* initialize the client with the server's piqi modules *)
  List.iter (fun piqi ->
      Piqi.process_piqi piqi
    ) piqi_list;
  (* return the last element of the list, it defines the interface to the
   * server *)
  last piqi_list


let get_local_piqi handle =
  trace "piqi_call: get Piqi\n";
  (* issue a special command to get Piqi modules from the server *) 
  let func_name = "" and data = None in
  match call_local_server handle func_name data with
    | `ok data ->
        init_piqi_common data
    | `error _ | `ok_empty ->
        piqi_error "local rpc error: invalid response to get_piqi request"
    | `rpc_error _ -> assert false (* checked earlier *)


let get_http_piqi path =
  trace "piqi_call: get Piqi from %s\n" path;
  (* issue a HTTP GET request get Piqi modules from the server *)
  let status, headers, body =
    Piqi_http.get path ~accept ~user_agent
  in
  let ct = get_content_type status headers in
  match status with
    | 200 when ct = content_type ->
        init_piqi_common body
    | 200 ->
        bad_content_type ct
    | _ ->
        piqi_error (Printf.sprintf
          "HTTP rpc error: unexpected response to get_piqi request (status code = %d)\n%s" status body)


let find_function piqi name =
  trace "piqi_call: find function %s\n" (quote name);
  try List.find (fun x -> x.T.Func.name = name) piqi.P#resolved_func
  with Not_found ->
    piqi_error ("server doesn't implement function: " ^ name)


let encode_input_data f args =
  trace "piqi_call: preparing input data\n";
  let t = f.T.Func#resolved_input in
  match t, args with
    | None, [] -> None
    | None, _ ->
        piqi_error "function doesn't expect input arguments"
    | Some piqtype, _ ->
        trace "piqi_call: parsing arguments\n";
        (* XXX: C.resolve_defaults := true; *)
        let piqobj = Piqi_getopt.parse_args (piqtype :> T.piqtype) args in
        let iodata = Piqobj_to_wire.gen_embedded_obj piqobj in
        let res = Piqirun.to_string iodata in
        Some res


let decode_response f output =
  trace "piqi_call: decoding response\n";
  match f.T.Func#resolved_output, output with
    | None, `ok_empty -> `ok_empty
    | Some _, `ok_empty ->
        piqi_error "unexpected empty result from server"
    | None, `ok _ ->
        piqi_error "unexpected non-empty result from server"
    | Some piqtype, `ok data ->
        let buf = Piqirun.init_from_string data in
        let obj = Piqobj_of_wire.parse_obj (piqtype :> T.piqtype) buf in
        `ok obj
    | _, `error data -> (
        match f.T.Func#resolved_error with
          | None -> piqi_error "unexpected error result from server"
          | Some piqtype ->
              let buf = Piqirun.init_from_string data in
              let obj = Piqobj_of_wire.parse_obj (piqtype :> T.piqtype) buf in
              `error obj
        )
    | _, `rpc_error _ -> assert false (* checked earlier *)


let with_open_pipe shell_command f =
  let handle = Unix.open_process shell_command in
  let res =
    try
      let res = f handle in
      `ok res
    with exn ->
      `error exn
  in
  let status = Unix.close_process handle in
  match status, res with
    | Unix.WEXITED 0, `ok x -> x
    | Unix.WEXITED 0, `error exn -> raise exn
    | Unix.WEXITED 127, _ ->
        piqi_error ("shell command couldn't be executed: " ^ shell_command)
    | Unix.WEXITED n, _ ->
        piqi_error ("server exited with error code " ^ (string_of_int n))
    | Unix.WSIGNALED n, _ ->
        piqi_error ("server was killed by signal " ^ (string_of_int n))
    | Unix.WSTOPPED n, _ ->
        piqi_error ("server was stopped by signal " ^ (string_of_int n))


(* NOTE: in future, we may implement a full http client that will open
 * connection once for all subsequent requests *)
let with_open_http f =
  try f ()
  with
    Piqi_http.Error s -> piqi_error ("HTTP rpc error: " ^ s)


let local_call (server_command, func_name) args =
  let f handle =
    let piqi = get_local_piqi handle in

    let f = find_function piqi func_name in
    let data = encode_input_data f args in

    let response = call_local_server handle func_name data in
    decode_response f response
  in
  with_open_pipe server_command f


let http_call (url, base_url, func_name) args =
  let f () =
    let piqi = get_http_piqi base_url in

    let f = find_function piqi func_name in
    let data = encode_input_data f args in

    let response = call_http_server url data in
    decode_response f response
  in
  with_open_http f


let local_get_piqi server_command =
  with_open_pipe server_command (fun handle -> get_local_piqi handle)


let http_get_piqi url =
  with_open_http (fun () -> get_http_piqi url)


let is_http_url url =
  try
    String.sub url 0 7 = "http://" ||
    String.sub url 0 8 = "https://"
  with _ -> false


let parse_url url =
  if is_http_url url
  then
    match Piqi_name.split_name url with
      | Some path, func ->
          (* remove a trailing '/' character if the url contains it *)
          let path, func =
            if func = ""
            then
              match Piqi_name.split_name path with
                | Some path, func -> path, func
                | _ -> piqi_error ("invalid HTTP URL: " ^ url)
            else path, func
          in
          if Piqi_name.is_valid_name func (* XXX: don't check? *)
          then `http (url, path, func)
          else piqi_error ("invalid function name in HTTP URL: " ^ func)
      | _ ->
          piqi_error ("invalid HTTP URL: " ^ url)
  else
    match Piqi_name.split_name url with
      | Some server, func ->
          if Piqi_name.is_valid_name func (* XXX: don't check? *)
          then `local (server, func)
          else piqi_error ("invalid function name in local URL: " ^ func)
      | _ ->
          piqi_error ("invalid local URL: " ^ url)


let call url args =
  match parse_url url with
    | `local x -> local_call x args
    | `http x -> http_call x args


let get_piqi url =
  if is_http_url url
  then http_get_piqi url
  else local_get_piqi url


let gen_result ch writer res =
  match res with
    | `ok_empty -> ()
    | `ok obj ->
        writer ch (Piq.Typed_piqobj obj)
    | `error obj ->
        trace "piqi_call: remote function returned error:\n";
        writer stderr (Piq.Typed_piqobj obj);
        exit 1


module Main = Piqi_main
open Main


let run_call url =
  let ch = open_output !ofile in
  if not !flag_get_piqi
  then
    let args = Piqi_getopt.getopt_piq () in
    let writer = Piqi_convert.make_writer !output_encoding in
    let res = call url args in
    gen_result ch writer res
  else
    let is_piqi_input = true in
    let writer = Piqi_convert.make_writer !output_encoding ~is_piqi_input in
    let piqi = get_piqi url in
    writer ch (Piq.Piqi piqi)


let usage = "Usage: piqi call [options] <HTTP or local URL> -- [arguments]\nOptions:"


(* URL: <server>/<method> *)
let url = ref ""
let custom_anon_fun s = url := s


let speclist = Main.common_speclist @
  [
    arg_o;

    Piqi_convert.arg__t;

    "--get-piqi", Arg.Set flag_get_piqi,
    "instead of calling a function, only get Piqi modules from a Piqi-RPC server";

    Piqi_getopt.arg__rest;
  ]


let run () =
  Main.parse_args ()
    ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1 ~custom_anon_fun;
  Piqi_getopt.init ();
  run_call !url

 
let _ =
  Main.register_command run "call"
    "Piqi-RPC client -- call remote function with command-line arguments as input"

