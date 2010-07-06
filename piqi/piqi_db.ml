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


module C = Piqi_common  
open C


(* idtable implemented as map: string -> 'a *)
module Idtable =
  struct
    module M = Map.Make(String)

    type 'a t = 'a M.t

    let empty = M.empty

    let add idtable name entry =
      M.add name entry idtable

    let find idtable name =
      M.find name idtable 

    let mem idtable name =
      M.mem name idtable 
  end


(* the map of loaded piqi modules: modname -> piqi *)
module Piqitable = Idtable
let loaded_map = ref Piqitable.empty


(* To be set up later to boot piqi when it gets loaded in Piqi module *) 
let boot_piqi :T.piqi option ref = ref None

(* To be set up later to Piqi.load_piqi_module; we do this since OCaml doesn't
 * support recursive toplevel modules *)
let piqi_loader :(string -> T.piqi) option ref = ref None


let load_piqi_module modname =
  let load = some_of !piqi_loader in
  (* XXX
  (* reset Config.noboot option, we can't use this option for dependent types
   * and modules *)
  let saved_noboot = !Config.noboot in
  Config.noboot := false;
  *)
  let piqi = load modname in
  (*
  Config.noboot := saved_noboot;
  *)
  piqi


let add_piqi modname piqi =
  loaded_map := Piqitable.add !loaded_map modname piqi


let find_piqi modname :T.piqi =
  Piqitable.find !loaded_map modname


let find_local_piqdef piqi name =
  List.find (fun x -> name = piqdef_name x) piqi.P#resolved_piqdef


let find_piqdef name =
  (* XXX: check global type name before loading? *)
  let modname, typename = Piqi_name.split_name name in
  let piqi = 
    match modname with
      | Some m ->
          trace "looking for type: %s\n" name;
          trace_enter ();
          (* XXX: handle load errors *)
          let piqi = load_piqi_module m in
          trace_leave ();
          piqi
      | None -> (* built-in or local type *)
          (* NOTE: local types are not supported yet *)
          some_of !boot_piqi
  in
  find_local_piqdef piqi typename


let find_piqtype name = 
  let def = find_piqdef name in
  (def: T.piqdef :> T.piqtype)


