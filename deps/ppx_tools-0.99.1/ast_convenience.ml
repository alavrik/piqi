(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

open Parsetree
open Asttypes
open Location
open Ast_helper

let may_tuple tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc:None ?attrs:None l)

let lid s = mkloc (Longident.parse s) !default_loc
let tuple l = Exp.tuple l
let constr s args = Exp.construct (lid s) (may_tuple Exp.tuple args)
let nil () = constr "[]" []
let unit () = constr "()" []
let cons hd tl = constr "::" [hd; tl]
let list l = List.fold_right cons l (nil ())
let str s = Exp.constant (Const_string (s, None))
let int x = Exp.constant (Const_int x)
let char x = Exp.constant (Const_char x)
let float x = Exp.constant (Const_float (string_of_float x))
let record ?over l =
  Exp.record (List.map (fun (s, e) -> (lid s, e)) l) over
let func l = Exp.function_ (List.map (fun (p, e) -> Exp.case p e) l)
let lam ?(label = "") ?default pat exp = Exp.fun_ label default pat exp
let app f l = if l = [] then f else Exp.apply f (List.map (fun a -> "", a) l)
let evar s = Exp.ident (lid s)
let let_in ?(recursive = false) b body =
  Exp.let_ (if recursive then Recursive else Nonrecursive) b body

let pvar s = Pat.var (mkloc s !default_loc)
let pconstr s args = Pat.construct (lid s) (may_tuple Pat.tuple args)
let precord ?(closed = Open) l =
  Pat.record (List.map (fun (s, e) -> (lid s, e)) l) closed
let pnil () = pconstr "[]" []
let pcons hd tl = pconstr "::" [hd; tl]
let punit () = pconstr "()" []
let plist l = List.fold_right pcons l (pnil ())
let ptuple l = Pat.tuple l

let pstr s = Pat.constant (Const_string (s, None))
let pint x = Pat.constant (Const_int x)
let pchar x = Pat.constant (Const_char x)
let pfloat x = Pat.constant (Const_float (string_of_float x))

let tconstr c l = Typ.constr (lid c) l

let get_str = function
  | {pexp_desc=Pexp_constant (Const_string (s, _)); _} -> Some s
  | _ -> None

let get_lid = function
  | {pexp_desc=Pexp_ident{txt=id;_};_} ->
    Some (String.concat "." (Longident.flatten id))
  | _ -> None

let find_attr s attrs =
  try Some (snd (List.find (fun (x, _) -> x.txt = s) attrs))
  with Not_found -> None

let expr_of_payload = function
  | PStr [{pstr_desc=Pstr_eval(e, _); _}] -> Some e
  | _ -> None

let find_attr_expr s attrs =
  match find_attr s attrs with
  | Some e -> expr_of_payload e
  | None -> None

let has_attr s attrs =
  find_attr s attrs <> None
