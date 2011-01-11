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


module T = Piqtype


module Record = T.Record
module Field = T.Field
module Variant = T.Variant
module Option = T.Option
module Enum = Variant
module Alias = T.Alias


module Import = T.Import
module Includ = T.Includ
module Extend = T.Extend
module Any = T.Any


module R = Record
module F = Field
module V = Variant
module O = Option
module E = Enum
module A = Alias
module L = T.Piqlist
module P = T.Piqi


module Config = Piqi_config
module Iolist = Piqi_iolist


(* lazily loaded representation of piqi-boot.piqi (see piqi.ml for details) *)
let boot_piqi :T.piqi option ref = ref None

let is_boot_piqi p =
  match !boot_piqi with
    | None -> false
    | Some x -> p == x


let some_of = function
  | Some x -> x
  | None -> assert false


let flatmap f l =  List.concat (List.map f l)


let find_dups l =
  let rec aux = function
    | [] -> None
    | h::t ->
        try 
          let dup = List.find (fun x -> x = h) t in
          Some (dup, h)
        with Not_found -> aux t
  in aux l


let quote s = "\"" ^ s ^ "\""


(* substitute character [x] with [y] in string [s] *)
let string_subst_char s x y =
  if not (String.contains s x)
  then s
  else
    (* preserve the original string *)
    let s = String.copy s in
    for i = 0 to (String.length s) - 1
    do
      if s.[i] = x
      then s.[i] <- y
    done; s


let dashes_to_underscores s =
  string_subst_char s '-' '_'


let underscores_to_dashes s =
  string_subst_char s '_' '-'


let list_of_string s =
  let n = String.length s in
  let rec aux i =
    if i < n
    then s.[i] :: (aux (i+1))
    else []
  in aux 0


let string_of_list l =
  let s = String.create (List.length l) in
  let rec aux i = function
    | [] -> ()
    | h::t ->
        s.[i] <- h; aux (i+1) t
  in
  aux 0 l; s


(* NOTE: naive, non-tail recursive. Remove duplicates from the list using
 * reference equality, preserves the initial order *)
let rec uniqq = function
  | [] -> []
  | h::t ->
      let t = uniqq t in
      if List.memq h t then t else h :: t


(* leave the first of the duplicate elements in the list instead of the last *)
let uniqq l =
  List.rev (uniqq (List.rev l))


let rec uniq = function
  | [] -> []
  | h::t ->
      let t = uniq t in
      if List.mem h t then t else h :: t


(* leave the first of the duplicate elements in the list instead of the last *)
let uniq l =
  List.rev (uniq (List.rev l))


let get_parent (piqdef:T.piqdef) :T.namespace =
  let parent =
    match piqdef with
      | `record t -> t.R#parent
      | `variant t -> t.V#parent
      | `enum t -> t.V#parent
      | `alias t -> t.A#parent
      | `list t -> t.L#parent
  in
  some_of parent


let set_parent (def:T.piqdef) (parent:T.namespace) =
  let parent = Some parent in
  match def with
    | `record x -> x.R#parent <- parent
    | `variant x -> x.V#parent <- parent
    | `enum x -> x.E#parent <- parent
    | `alias x -> x.A#parent <- parent
    | `list x -> x.L#parent <- parent


let get_parent_piqi (def:T.piqdef) :T.piqi =
  let parent = get_parent def in
  match parent with
    | `import x -> some_of x.Import#piqi
    | `piqi x -> x


let piqdef_name (piqdef:T.piqdef) =
  match piqdef with
    | `record x -> x.R#name
    | `variant x -> x.V#name
    | `enum x -> x.E#name
    | `alias x -> x.A#name
    | `list x -> x.L#name


let piqi_typename (t:T.piqtype) =
  let open T in
  (* XXX: built-in types should't be used at that stage *)
  match t with
    | `int -> "int"
    | `float -> "float"
    | `bool -> "bool"
    | `string -> "string"
    | `binary -> "binary"
    | `word -> "piq-word"
    | `text -> "piq-text"
    | `any -> "piq-any"
    | #T.piqdef as x -> piqdef_name x


let full_piqi_typename x =
  let name = piqi_typename x in
  match x with
    | #T.piqdef as def ->
        let piqi = get_parent_piqi def in
        if is_boot_piqi piqi
        then name
        else
          let parent_name = some_of piqi.P#modname in
          parent_name ^ "/" ^ name
    | _ -> (* built-in type *)
        (* XXX: normally built-in types should be used at the time when this
         * funciton is used *)
        name


let piqtype (t:T.typeref) :T.piqtype =
  match t with
    | `name _ -> assert false (* type has to be resolved by that moment *)
    | (#T.piqtype as t) -> t


let piqi_typerefname (t:T.typeref) =
  match t with
    | `name x -> x
    | (#T.piqtype as t) -> piqi_typename t


let name_of_field f =
  let open T.Field in
  match f.name, f.typeref with
    | Some n, _ -> n
    | None, Some t -> piqi_typerefname t
    | _ -> assert false


let name_of_option o =
  let open T.Option in
  match o.name, o.typeref with
    | Some n, _ -> n
    | None, Some t -> piqi_typerefname t
    | _ -> assert false


let rec unalias = function
  | `alias t ->
      let t = piqtype t.T.Alias#typeref in
      unalias t
  | t -> t


(* 
 * error reporting, printing and handling
 *)

let string_of_loc (file, line, col) =
  file ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col


let strerr loc s = 
  string_of_loc loc ^ ": " ^ s


(* piq/piqi language error *)
exception Error of Piqloc.loc * string 


(* piqi utility error *)
exception Piqi_error of string

let piqi_error s =
  raise (Piqi_error s)

let piqi_warning s =
  prerr_endline ("Warning: " ^ s)


let error_at loc s =
  (*
  failwith (strerr loc s)
  *)
  raise (Error (loc, s))


let reference f x =
  Piqloc.reference f x


let location obj =
  try
    if !Config.debug_level > 0
    then
      Piqloc.trace_find obj
    else
      Piqloc.find obj
  with
    Not_found -> ("unknown", 0, 0)


let error obj s =
  let loc = location obj in
  error_at loc s


let error_string obj s =
  let loc = location obj in
  strerr loc s


let warning obj s =
  let loc = location obj in
  if not !Config.flag_no_warnings
  then prerr_endline ("Warning: " ^ strerr loc s)


let trace_indent = ref 0

let print_trace_indent () =
  for i = 1 to !trace_indent
  do
    prerr_string "    "
  done


let eprintf_if cond fmt =
  if cond
  then
    begin
      print_trace_indent ();
      Printf.fprintf stderr fmt
    end
  else Printf.ifprintf stderr fmt


let debug fmt =
  eprintf_if (!Config.debug_level > 0) fmt


let trace fmt =
  eprintf_if (!Config.flag_trace || !Config.debug_level > 0)  fmt


let trace_enter () =
  incr trace_indent


let trace_leave x =
  decr trace_indent;
  x

