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


(* values of HTTP headers when making Piqi-RPC requests *)
let content_type = "application/x-protobuf"
let accept = content_type


let send_request (_,och) request =
  Piqi_server.send_packet och request


let receive_response (buf,_) =
  let buf = Piqi_server.receive_packet buf in
  Piqi_rpc.parse_response buf


let string_of_rpc_error = function
  | `unknown_function -> "unknown function"
  | `missing_input -> "missing input"
  | `invalid_input err -> "invalid input: " ^ err
  | `invalid_output err -> "invalid output: " ^ err
  | `internal_error err -> "internal error: " ^ err
  | `service_unavailable err -> "service unavailable: " ^ err
  | `protocol_error err -> "protocol error: " ^ err


let call_local_server handle request =
  trace "piqi call: making local call\n";
  send_request handle request;
  match receive_response handle with
    | `rpc_error err ->
        piqi_error ("rpc error: " ^ string_of_rpc_error err)
    | x -> x


let call_http_server url body =
  trace "piqi call: making HTTP call\n";
  let status, headers, body =
    Piqi_http.post url ~accept ~content_type ?body
  in
  let ct =
    match Piqi_http.get_header "content-type" headers with
      | None when status = 204 -> "" (* "No data" therefore no Content-Type *)
      | None ->
          piqi_error
            "piqi HTTP call: Content-Type is missing in server's response"
      | Some x -> x
  in
  (* TODO: catch Piqi_http.Error *)
  match status with
    | 204 -> (* "No Data" *)
        `ok_empty
    | 200 -> (* "OK" *)
        if ct = content_type
        then `ok body
        else
        piqi_error ("piqi-RPC error: " ^ body)
    | 500 when ct = content_type ->
        (* "Internal Server Error" with the application Content-Type header
         * means application error *)
        `error body
    | _ ->
        (* all other HTTP codes either mean `rpc_error or some other HTTP
         * transport or server-related error *)
        piqi_error
          (Printf.sprintf "HTTP error: %d\n\n%s\n" status body)


(* returns the last element of the list *)
let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::t -> last t


let init_piqi_common data =
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


let init_local_piqi handle =
  trace "piqi local call: get Piqi\n";
  (* issue a special command to get Piqi modules from the server *) 
  let request = Piqi_rpc.Request#{name = ""; data = None} in
  let input = Piqi_rpc.gen_request (-1) request in
  match call_local_server handle input with
    | `ok data ->
        init_piqi_common data
    | `error _ | `ok_empty ->
        piqi_error "invalid response to Piqi modules request"
    | `rpc_error _ -> assert false (* checked earlier *)


let init_http_piqi path =
  trace "piqi http call: get Piqi from %s\n" path;
  (* issue a HTTP GET request get Piqi modules from the server *)
  let status, _headers, body = Piqi_http.get path ~accept in
  (* TODO: check content type of the response *)
  (* TODO: catch Piqi_http.Error *)
  match status with
    | 200 ->
        init_piqi_common body
    | _ ->
        piqi_error
          (Printf.sprintf "unexpected HTTP response: %d\n\n%s\n" status body)


let find_function piqi name =
  trace "piqi call: find function %s\n" (quote name);
  try List.find (fun x -> x.T.Func.name = name) piqi.P#resolved_func
  with Not_found ->
    piqi_error ("server doesn't implement function: " ^ name)


let prepare_input_data f args =
  trace "piqi call: preparing input data\n";
  let t = f.T.Func#resolved_input in
  match t, args with
    | Some _, None -> piqi_error "missing input arguments"
    | None, Some _ -> piqi_error "function doesn't expect input arguments"
    | None, None -> None
    | Some piqtype, Some ast ->
        trace "piqi call: parsing parameters\n";
        (* XXX: C.resolve_defaults := true; *)
        let piqobj = Piqobj_of_piq.parse_obj (piqtype :> T.piqtype) ast in
        let iodata = Piqobj_to_wire.gen_embedded_obj piqobj in
        let res = Piqirun.to_string iodata in
        Some res


let decode_response ot et output =
  trace "piqi call: decoding response\n";
  match ot, output with
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
        match et with
          | None -> piqi_error "unexpected error result from server"
          | Some piqtype ->
              let buf = Piqirun.init_from_string data in
              let obj = Piqobj_of_wire.parse_obj (piqtype :> T.piqtype) buf in
              `error obj
        )
    | _, `rpc_error _ -> assert false (* checked earlier *)


let gen_output ch obj =
  let ast = Piq.gen_piq (Piq.Piqobj obj) in
  Piq_gen.to_channel ch ast;
  output_char ch '\n'


let gen_error obj =
  let ch = stderr in (* XXX *)
  let ast = Piq.gen_piq (Piq.Piqobj obj) in
  output_string ch "error: ";
  Piq_gen.to_channel ch ast;
  output_char ch '\n'


let handle_response ch f response =
  let res =
    decode_response f.T.Func#resolved_output f.T.Func#resolved_error response
  in
  match res with
    | `ok_empty -> ()
    | `ok obj ->
        gen_output ch obj
    | `error obj ->
        gen_error obj;
        (* TODO: unify error handling of `error (application error) and
         * `rpc_error *)
        exit 1


let local_call ch (server_command, func_name) args =
  (* TODO: detect child process crash *)
  let (in_channel, out_channel) = Unix.open_process server_command in
  let ibuf = Piqirun.IBuf.of_channel in_channel in
  let handle = (ibuf, out_channel) in

  let piqi = init_local_piqi handle in

  let f = find_function piqi func_name in

  let data = prepare_input_data f args in
  let request = Piqi_rpc.Request#{name = func_name; data = data} in
  let input = Piqi_rpc.gen_request (-1) request in

  let response = call_local_server handle input in

  handle_response ch f response


let http_call ch (path, func_name) args =
  let piqi = init_http_piqi path in

  let f = find_function piqi func_name in

  let input = prepare_input_data f args in

  let url = path ^ "/" ^ func_name in
  let response = call_http_server url input in

  handle_response ch f response


let is_remote_url url =
  try
    String.sub url 0 7 = "http://" ||
    String.sub url 0 8 = "https://"
  with _ -> false


let parse_url url =
  if is_remote_url url
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
          then `http (path, func)
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


module Main = Piqi_main
open Main


(* index of the "--" element in argv array *)
let argv_start_index = ref 0


let call url =
  let args = Piqi_getopt.getopt_piq !argv_start_index in
  let ch = open_output !ofile in
  match parse_url url with
    | `local x -> local_call ch x args
    | `http x -> http_call ch x args


let usage = "Usage: piqi call [options] <function url> -- [arguments]\nOptions:"


(* find the position of the first argument after "--" *)
let rest_fun arg =
  if !argv_start_index = 0 (* first argument after first occurence of "--" *)
  then argv_start_index := !Arg.current + 1
  else ()


(* URL: <server>/<method> *)
let url = ref ""
let custom_anon_fun s = url := s


let speclist = Main.common_speclist @
  [
    arg_o;
    "--", Arg.Rest rest_fun,
    "separator between piqi command-line arguments and function arguments";
  ]


let run () =
  Main.parse_args ()
    ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1 ~custom_anon_fun;

  if !argv_start_index = 0 (* "--" is not present in the list of arguments *)
  then argv_start_index := Array.length Sys.argv;

  call !url

 
let _ =
  Main.register_command run "call"
    "Piqi RPC client -- reads command-line arguments ads calls remote functions"

