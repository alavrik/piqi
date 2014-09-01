(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(** {1 Convenience functions to help build and deconstruct AST fragments.} *)

open Parsetree
open Asttypes
open Ast_helper

(** {2 Misc} *)

val lid: string -> lid

(** {2 Expressions} *)

val evar: string -> expression
val let_in: ?recursive:bool -> value_binding list -> expression -> expression

val constr: string -> expression list -> expression
val record: ?over:expression -> (string * expression) list -> expression
val tuple: expression list -> expression

val nil: unit -> expression
val cons: expression -> expression -> expression
val list: expression list -> expression

val unit: unit -> expression

val func: (pattern * expression) list -> expression
val lam: ?label:string -> ?default:expression -> pattern -> expression -> expression
val app: expression -> expression list -> expression

val str: string -> expression
val int: int -> expression
val char: char -> expression
val float: float -> expression

(** {2 Patterns} *)

val pvar: string -> pattern
val pconstr: string -> pattern list -> pattern
val precord: ?closed:closed_flag -> (string * pattern) list -> pattern
val ptuple: pattern list -> pattern

val pnil: unit -> pattern
val pcons: pattern -> pattern -> pattern
val plist: pattern list -> pattern

val pstr: string -> pattern
val pint: int -> pattern
val pchar: char -> pattern
val pfloat: float -> pattern

val punit: unit -> pattern


(** {2 Types} *)

val tconstr: string -> core_type list -> core_type

(** {2 AST deconstruction} *)

val get_str: expression -> string option
val get_lid: expression -> string option

val has_attr: string -> attributes -> bool
val find_attr: string -> attributes -> payload option
val find_attr_expr: string -> attributes -> expression option
