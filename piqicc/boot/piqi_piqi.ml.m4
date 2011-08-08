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


type wire_type =
  [ `varint
  | `zigzag_varint
  | `fixed32
  | `fixed64
  | `signed_varint
  | `signed_fixed32
  | `signed_fixed64
  | `block (* length-delimited block *) ]


module rec Piqtype: DEFTYPES(
  <<
    type record = Record.t
    type variant = Variant.t
    type enum = Variant.t
    type alias = Alias.t
    type piqlist = Piqlist.t
    type option = Option.t
    type field = Field.t

    type piqi = Piqi.t

    type piqdef =
      [ `record of record
      | `variant of variant
      | `enum of enum
      | `alias of alias
      | `list of piqlist ]

    type piqtype =
      [ piqdef
        (* built-in types *)
      | `int
      | `float
      | `bool
      | `string
      | `binary
      | `text

      | `word
      | `any ]

    type typeref =
      [ `name of string
      | piqtype ]

    type field_mode =
      [ `required
      | `optional
      | `repeated ]

    type namespace =
      [
      | `piqi of Piqtype.piqi
      | `import of Import.t ]

  >>)
and DEFRECORD(Record,
  <<
    mutable name : string;
    mutable field : Piqtype.field list;

    mutable ocaml_name : string option;
    mutable parent : Piqtype.namespace option;
    mutable wire_field : Piqtype.field list; (* fields ordered by wire code *)
  >>)
and DEFRECORD(Field,
  <<
    name : string option;
    mutable typeref : Piqtype.typeref option;
    mode : Piqtype.field_mode;
    default : any option;

    mutable alt_name : string option;
    mutable ocaml_name : string option;
    mutable code : int32 option; (* wire code *)
    mutable wire_packed : bool;
    mutable ocaml_array : bool;
  >>)
and DEFRECORD(Variant,
  <<
    mutable name : string;
    option : Piqtype.option list;

    mutable ocaml_name : string option;
    mutable parent : Piqtype.namespace option;
  >>)
and DEFRECORD(Option,
  <<
    (* NOTE: either name or type have to be present *)
    name : string option;
    mutable typeref : Piqtype.typeref option;

    mutable alt_name : string option;
    mutable ocaml_name : string option;
    mutable code : int32 option; (* wire code *)
  >>)
and DEFRECORD(Alias,
  <<
    mutable name : string;
    mutable typeref : Piqtype.typeref;

    mutable ocaml_name : string option;

    mutable wire_type : wire_type option; 
    mutable ocaml_type : string option; 

    mutable parent : Piqtype.namespace option;
  >>)
and DEFRECORD(Piqlist,
  <<
    mutable name : string;
    mutable typeref : Piqtype.typeref;

    mutable ocaml_name : string option;
    mutable parent : Piqtype.namespace option;

    mutable wire_packed : bool;
    mutable ocaml_array : bool;
  >>)
and DEFRECORD(Includ,
  <<
    modname : string;
    mutable piqi : Piqtype.piqi option;
  >>)
and DEFRECORD(Extend,
  <<
    name : string list;
    quote : any list;
  >>)
and DEFRECORD(Import,
  <<
    modname : string;
    mutable name : string option;
    mutable piqi : Piqtype.piqi option;

    mutable ocaml_name : string option;
  >>)
and DEFRECORD(Piqi,
  <<
    mutable modname : string option;
    mutable ocaml_module : string option;

    mutable piqdef : Piqtype.piqdef list;
    mutable includ : Includ.t list;
    mutable import : Import.t list;
    mutable extend : Extend.t list;

    mutable custom_field : string list;

    mutable extended_piqdef : Piqtype.piqdef list;
    mutable resolved_piqdef : Piqtype.piqdef list;
    mutable imported_piqdef : Piqtype.piqdef list;
    mutable resolved_import : Import.t list;
    mutable included_piqi : Piqtype.piqi list;

    mutable original_piqi : Piqtype.piqi option;
  >>)


include Piqtype


(* bootstrap stubs *)
let parse_piqi x = assert false

let parse_piqdef x = assert false

let gen__piqdef code x = assert false

let embedded_piqi :(string * string) list ref = ref []

(*
vim:ft=ocaml
*)
