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


(*
m4 macro definitions:

changequote(<<,>>)dnl

define(DEFTYPES, <<
  sig
    $1
  end =
  struct
    $1
  end
>>)

define(DEFRECORD, <<$1:
  sig
    type t =
      {$2
      }
  end =
  struct
    type t =
      {$2
      }
  end
>>)

*)


module rec Piqrepr: DEFTYPES(
  <<
    type any = Any.t

    type named = Named.t
    type typed = Typed.t

    type ast =
      [ `int of int64
      | `uint of int64
      | `float of float
      | `bool of bool
      | `ascii_string of string
      | `utf8_string of string
      | `binary of string
      | `name of string
      | `named of named
      | `typename of string
      | `typed of typed
      | `list of ast list
      | `control of ast list
      | `word of string
      | `text of string
      | `raw_word of string
      | `raw_binary of string ]
  >>)
and DEFRECORD(Named,
  <<
    mutable name : string;
    mutable value : Piqrepr.ast;
  >>)
and DEFRECORD(Typed,
  <<
    mutable typename : string;
    mutable value : Piqrepr.any;
  >>)
and DEFRECORD(Any,
  <<
    (* either ast or binobj have to be present; but sometimes it make sense to
       include both of them *)
    mutable binobj : string option; (* binary (wire) -encoded piq object *)
    mutable ast : Piqrepr.ast option;
  >>)
 

include Piqrepr


(* bootstrap stubs *)
let gen__any code x = assert false

let parse_any x = assert false


(*
vim:ft=ocaml
*)
