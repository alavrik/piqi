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


let send_request (_,och) request =
  output_string och request;
  flush och


let receive_response (buf,_) =
  match Piqi_server.parse_binobj_part buf with
    | Some "ok", data ->
        if Piqirun.parse_string data = ""
        then `ok_empty
        else `ok data
    | Some "error", data -> `error data
    | Some "piqi-error", data ->
        let err = Piqirun.parse_string data in
        piqi_error ("remote error: " ^ err)
    | _ ->
        piqi_error "invalid response format"


let call_local_server handle request =
  send_request handle request;
  receive_response handle


(* returns the last element of the list *)
let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::t -> last t


let init_piqi handle =
  (* issue a special command to get piqi modules from the server *) 
  let request = Piqirun.gen_binobj Piqirun.gen_string "" ~name:"" in
  match call_local_server handle request with
    | `ok data ->
        let binobj_list = Piqirun.parse_list Piqirun.parse_string data in
        (* decode piqi modules *)
        let piqi_list =
            List.map (fun x ->
                let _, obj = Piqirun.parse_binobj x in
                T.parse_piqi obj
              ) binobj_list
        in
        (* initialize the client with the server's piqi modules *)
        List.iter (fun piqi ->
            (* XXX: fname argument should be optional *)
            let fname = "" in
            Piqi.process_piqi fname piqi
          ) piqi_list;
        (* return the last element of the list, it defines the interface to the
         * server *)
        last piqi_list
    | `error _ | `ok_empty ->
        piqi_error "invalid response to Piqi request"


let find_function piqi name =
  try List.find (fun x -> x.T.Func.name = name) piqi.P#resolved_func
  with Not_found ->
    piqi_error ("server doesn't implement function: " ^ name)


let encode_request name t args =
  match t, args with
    | Some _, None -> piqi_error "missing input arguments"
    | None, Some _ -> piqi_error "function doesn't expect input arguments"
    | None, None ->
        (* empty payload *)
        Piqirun.gen_binobj Piqirun.gen_string "" ~name
    | Some piqtype, Some ast ->
        prerr_endline "parsing parameters";
        Piqobj_of_piq.resolve_defaults := !Piqi_convert.flag_add_defaults;
        let piqobj = Piqobj_of_piq.parse_obj (piqtype :> T.piqtype) ast in
        Piqirun.gen_binobj Piqobj_to_wire.gen_obj piqobj ~name


let decode_response ot et output =
  match ot, output with
    | None, `ok_empty -> `ok_empty
    | Some _, `ok_empty ->
        piqi_error "unexpected empty result from server"
    | None, `ok _ ->
        piqi_error "unexpected non-empty result from server"
    | Some piqtype, `ok data ->
        let obj = Piqobj_of_wire.parse_obj (piqtype :> T.piqtype) data in
        `ok obj
    | _, `error data ->
        match et with
          | None -> piqi_error "unexpected error result from server"
          | Some piqtype ->
              let obj = Piqobj_of_wire.parse_obj (piqtype :> T.piqtype) data in
              `error obj


let gen_output ch obj =
  let ast = Piq.gen_piq (Piq.Piqobj obj) in
  Piq_gen.to_channel ch ast;
  output_char ch '\n'


let local_call ch (server_command, func_name) args =
  let (in_channel, out_channel) = Unix.open_process server_command in
  let ibuf = Piqirun.IBuf.of_channel in_channel in
  let handle = (ibuf, out_channel) in

  prerr_endline "init Piqi";
  let piqi = init_piqi handle in

  prerr_endline "find function";
  let f = find_function piqi func_name in

  prerr_endline "encoding request";
  let request = encode_request func_name f.T.Func#resolved_input args in
  prerr_endline "making a call";
  let response = call_local_server handle request in
  prerr_endline "decoding response";
  let res =
    decode_response f.T.Func#resolved_output f.T.Func#resolved_error response
  in
  prerr_endline "generating output";
  (match res with
    | `ok_empty -> ()
    | `ok obj ->
        gen_output ch obj
    | `error obj ->
        gen_output stderr obj
  );
  ()


let is_remote_url url =
  try String.sub url 0 7 = "http://" (* XXX: allow https? *)
  with _ -> false


let parse_url url =
  if is_remote_url url
  then `http url (* TODO: validate the remote url *) 
  else
    match Piqi_name.split_name url with
      | Some server, func ->
          if Piqi_name.is_valid_name func (* XXX: don't check? *)
          then `local (server, func)
          else piqi_error ("invalid function name: " ^ func)
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
    | `http _ ->
        piqi_error "HTTP calls are not supported yet"


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

