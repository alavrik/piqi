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


(* This module contains functionality that Piqi module would contain, but if it
 * did, it would break Piqicc bootstrap that doesn't support advanced Piqi
 * functionality such as functions. *)

(* XXX: include this module from Piqi? *)


module C = Piqi_common
open C


let get_functions modules =
  flatmap (fun x -> x.P#func) modules


let get_resolved_functions modules =
  flatmap (fun x -> x.P#resolved_func) modules
