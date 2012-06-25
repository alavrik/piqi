(*
   Copyright 2009, 2010, 2011, 2012 Anton Lavrik

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
 * commonly used utility functions
 *)

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


let dashes_to_underscores s =
  string_subst_char s '-' '_'


let underscores_to_dashes s =
  string_subst_char s '_' '-'

