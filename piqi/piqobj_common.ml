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


module R = Piqobj.Record
module F = Piqobj.Field
module V = Piqobj.Variant
module E = Piqobj.Variant
module O = Piqobj.Option
module A = Piqobj.Alias
module Any = Piqobj.Any
module L = Piqobj.List


let type_of (x:Piqobj.obj) :T.piqtype =
  (* XXX: built-in types should be used at that point *)
  match x with
    | `int _ -> `int
    | `uint _ -> `int
    | `float _ -> `float
    | `bool _ -> `bool
    | `string _ -> `string
    | `binary _ -> `binary
    | `text _ -> `text
    | `word _ -> `word
    | `any _ -> `any
    (* custom types *)
    | `record x -> `record x.R#piqtype
    | `variant x -> `variant x.V#piqtype
    | `enum x -> `enum x.E#piqtype
    | `list x -> `list x.L#piqtype
    | `alias x -> `alias x.A#piqtype


let get_parent_piqi (parent:T.namespace) :T.piqi =
  match parent with
    | `import x -> some_of x.Import#piqi
    | `piqi x -> x


(* TODO: move to piqi_common *)
let full_piqi_typename x =
  let name = piqi_typename x in
  match x with
    | #T.piqdef as def ->
        (try
          let parent = get_parent def in
          let piqi = get_parent_piqi parent in
          if piqi == some_of !Piqi_db.boot_piqi
          then name
          else
            let parent_name = some_of piqi.P#modname in
            parent_name ^ "/" ^ name
        with
          Assert_failure _ ->
            (* parent is undefined for boot defs in T.piqdefs *)
            (* TODO: provide a better resolution for this situation, like for
             * example recognizing piqicc mode rather than catching the
             * exception; or, provide a fake piqi (like in Boot.boot) and add it
             * as a parent *)
            name)
    | _ -> (* built-in type *)
        (* XXX: normally built-in types should be used at the time when this
         * funciton is used *)
        name


let full_typename x =
  full_piqi_typename (type_of x)


