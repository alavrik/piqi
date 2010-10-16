(* object location DB *)
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


type loc = string * int * int (* file, line, column *)


(* internal locator structure: location can be represented by either location
 * itself or by reference to an object which is registered in the location DB *)
type t = Loc of loc | Ref of Obj.t


let db :(Obj.t * t) list ref = ref []


(* recursively dereference to find the location *)
let find x =
  let rec aux x =
    match List.assq x !db with
      | Loc loc -> loc
      | Ref x -> aux x
  in aux (Obj.repr x)


(* the same, but with printing a message at each recursion step *) 
let trace_find x =
  let rec aux i x =
    match List.assq x !db with
      | Loc loc -> loc
      | Ref x -> (Printf.eprintf "trace_find: %d\n" (Obj.magic x); aux (i + 1) x)
  in aux 0 (Obj.repr x)


let lastloc = ref ("undefined", 0, 0)


let setloc loc = lastloc := loc


let add x =
  db := (Obj.repr x, Loc !lastloc)::!db


let addloc loc x = 
  setloc loc; add x


let addret x = (* add object within the current location and return *)
  add x; x


let addlocret loc x =
  addloc loc x; x


let trace = ref false


exception Piqloc_not_found
exception Piqloc_trace_check_loc


(* check if location for the object exists in the loc database *)
let check_loc x =
  try
    ignore (find x)
  with Not_found ->
    raise Piqloc_not_found


let add_fake_loc ?(label="") x =
  let fake_loc = ("fake" ^ label, 0, 0) in
  db := (Obj.repr x, Loc fake_loc)::!db


let rec check_add_fake_loc ?(label="") x =
  try
    check_loc x
  with Piqloc_not_found ->
    add_fake_loc x ~label


let trace_check_loc x =
  if !trace
  then
    try check_loc x
    with Piqloc_not_found -> raise Piqloc_trace_check_loc


let addref dst src =
  if !trace (* && not (Obj.is_int (Obj.repr dst)) *)
  then check_loc dst;

  if Obj.repr src == Obj.repr dst
  then () (* do nothing *)
  else
    db := (Obj.repr src, Ref (Obj.repr dst))::!db


let addrefret dst src = (* add reference and return *)
  addref dst src; src


(* store resulting object -> source object correspondent in the location DB *)
let reference f x =
  let res = f x in
  addrefret x res


(* input and output wire location counters *)
let icount = ref 0
let ocount = ref 0

let next_icount () =
  let res = !icount in incr icount; res

let next_ocount () =
  let res = !ocount in incr ocount; res


(* Discard location information. This allows GC to reclaim memory used by data
 * objects that are not referenced from anywhere else other than from location
 * db *)
let reset () =
  db := [];
  icount := 0;
  ocount := 0;
  ()

