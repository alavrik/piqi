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


let send_command (_,och) name data =
  let binobj = Piqirun.gen_binobj Piqirun.gen_string data ~name in
  output_string och binobj;
  flush och


let receive_response (buf,_) =
  match Piqi_server.parse_binobj_part buf with
    | Some "ok", data -> `ok data
    | Some "error", data -> `error data
    | Some "piqi-error", data ->
        `piqi_error (Piqirun.parse_string data)
    | _ ->
        failwith "invalid response format"


let init_piqi handle =
  send_command handle "" "";
  match receive_response handle with
    | `ok data ->
        let binobj_list = Piqirun.parse_list Piqirun.parse_string data in
        let piqi_list =
            List.map (fun x ->
                let _, obj = Piqirun.parse_binobj x in
                T.parse_piqi obj
              ) binobj_list
        in
        List.iter (fun piqi ->
            (* XXX: fname argument should be optional *)
            let fname = "" in
            Piqi.process_piqi fname piqi
          ) piqi_list
    | _ ->
        assert false


let local_call ch (server, func) args =
  let (in_channel, out_channel) = Unix.open_process server in
  let ibuf = Piqirun.IBuf.of_channel in_channel in
  let handle = (ibuf, out_channel) in
  init_piqi handle;
  print_endline "success!";
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

