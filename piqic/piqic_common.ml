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


(*
 * This module contain functionality that is shared between several
 * language-specific piqi compilers.
 *)

open Piqi_common
open Iolist


let gen_code = function
  | None -> assert false
  | Some code -> ios (Int32.to_string code)


let check_depends_on_piq_any x =
  let is_any x = (unalias (piqtype x)) = `any in
  let is_any_opt = function
    | Some x -> is_any x
    | None -> false
  in
  match x with
    | `record x -> List.exists (fun x -> is_any_opt x.F#typeref) x.R#field
    | `variant x -> List.exists (fun x -> is_any_opt x.O#typeref) x.V#option
    | `list x -> is_any x.L#typeref
    | `enum _ -> false
    | `alias _ -> false (* don't check aliases, we do unalias instead *)


(* indication whether there is a defintion that uses "piq_any" type *)
let depends_on_piq_any = ref false


let piqic_common piqi =
  (* if no definition uses "piq_any" type, piq_any aliase will be excluded in
   * order to avoid unnecessary dependency on Piqtype module *)
  depends_on_piq_any :=
    List.exists check_depends_on_piq_any piqi.P#resolved_piqdef

