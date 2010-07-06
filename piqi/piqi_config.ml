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


let piqi_dir =
  try Some (Sys.getenv "PIQI_DIR")
  with Not_found -> None


(* .piqi search path; contains CWD and $PIQI_DIR by default *)
let paths =
  let l =
    match piqi_dir with
      | None -> ["."]
      | Some x -> ["."; x]
  in ref l


let add_path x =
  paths := !paths @ [x]


(*
 * command-line options 
 *)
let boot_file = ref ""


(* don't boot, i.e. don't include any of embedded or external boot file
 * definitions into piqi specification which is being processed *)
let noboot = ref false


let flag_no_warnings = ref false
let debug_level = ref 0
let flag_trace =
  try 
    ignore (Sys.getenv "PIQI_TRACE");
    ref true
  with Not_found ->
    ref false


(* this variable controls whether we parse and generate piq AST
 * for/during pretty-printing or for real use *)
let pp_mode = ref false


