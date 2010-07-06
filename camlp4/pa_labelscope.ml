(*pp camlp4orf *)
(* This code is in public domain. Written by Anton Lavrik.
 *
 * Tested with OCaml 3.11.2; also works with 3.10.2 with some modifications (see
 * comments below).
 *
 * Based on pa_openin.ml written by Alain Frisch and adapted to OCaml 3.10 by
 * Till Varoquaux
 *)

open Camlp4.PreCast
open Syntax
open Ast


let fresh () = Printf.sprintf "LABELSCOPE_%i" (Oo.id (object end))

let local_struct _loc st e =
  let x = fresh () in
  <:expr< let module $x$ = struct $st$; value res = $e$; end in $uid:x$.res >>


let stream_peek_nth n strm =
  let rec loop n =
    function
    | [] -> None
    | [ (x, _) ] -> if n = 1 then Some x else None
    | _ :: l -> loop (n - 1) l
  in
  loop n (Stream.npeek n strm)


let test_label_eq =
  Gram.Entry.of_parser "test_label_eq"
    (
      let rec test lev strm =
        match stream_peek_nth lev strm with
            Some (UIDENT _ | LIDENT _ | KEYWORD "." | KEYWORD "#") ->
              test (lev + 1) strm
          | Some (KEYWORD "=") -> ()
          | _ -> raise Stream.Failure
      in test 1
    )


let test_record_cons =
  Gram.Entry.of_parser "test_record_cons"
    (
      fun strm ->
        match Stream.npeek 5 strm with 
            [(UIDENT _,_); (KEYWORD "#",_); (KEYWORD "{",_); _; _] -> ()
          | [(UIDENT _,_); (KEYWORD ".",_); (UIDENT _,_); (KEYWORD "#",_); (KEYWORD "{",_)] -> ()
          | _ -> raise Stream.Failure
    )


(* In OCaml 3.10.2 the rule uses differernt names: "label_expr" instead of
  "label_expr_list"

DELETE_RULE Gram expr: "{"; test_label_eq; label_expr; "}" END;
*)

DELETE_RULE Gram expr: "{"; test_label_eq; label_expr_list; "}" END;


EXTEND Gram
  GLOBAL: expr label_longident val_longident a_UIDENT a_LIDENT;

  expr: LEVEL "simple" [
    [
      "{"; test_label_eq; le = label_expr_list; "}" ->
        (* Ast.ExRec (_loc, lel, (Ast.ExNil _loc)) *)
        <:expr< { $le$ } >>

    | test_record_cons; i = module_longident; "#"; e = expr LEVEL "simple" ->
        local_struct _loc <:str_item< open $i$ >> e
    ]
  ];

  val_longident: [
    [
      m = a_UIDENT; "#"; f = a_LIDENT -> <:ident< $uid:m$.$lid:f$ >>
    ]
  ];

  label_longident: [
    [
      m = a_UIDENT; "#"; f = a_LIDENT -> <:ident< $uid:m$.$lid:f$ >>
    ]
  ];


(* TODO: #-labels in record patterns *)

END
