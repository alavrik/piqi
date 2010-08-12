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


open Piqi_common


let boot () = 
  let piqi =
    P#{
      modname = Some "piqtype";
      ocaml_module = None;

      piqdef = T.piqdef_list;
      includ = [];
      import = [];
      extend = [];

      custom_field = [];

      extended_piqdef = T.piqdef_list;
      resolved_piqdef = T.piqdef_list;
      imported_piqdef = [];
      resolved_import = [];
      included_piqi = [];
      original_piqi = None;
    }
  in
  (* call piq interface compiler for ocaml *)
  Piqic_ocaml_types.cc_mode := true;
  Piqic_ocaml.piqic piqi stdout;
  print_endline "let embedded_piqi :(string * string) list ref = ref []";
  ()


let boot2 () = 
  (* set piqi boot module *)
  Piqi_config.boot_file := "boot/piqi-boot.piqi";

  let fname = "../piqi.org/piqast.piqi" in
  let piqi = Piqi.load_piqi fname in
  (* call piq interface compiler for ocaml *)
  Piqic_ocaml_types.cc_mode := true;
  Piqic_ocaml.piqic piqi stdout;
  print_endline "let embedded_piqi :(string * string) list ref = ref []";
  ()


(*
let reboot () =
  (*
  try
  *)
    (* set piqi boot module *)
    Piqi_config.boot_file := "piqicc-boot.piqi";

    let fname = "piqicc.piqi" in
    Piqicc.piqicc stdout fname ;
    ()
  (*
  with 
    Error (loc, s) ->
      prerr_endline ("ERROR: " ^ (strerr loc s));
      exit 1
  *)
*)


let _ =
  if !Sys.interactive
  then ()
  else
    match Sys.argv.(0) with
      | "./piqi_boot" -> boot ()
      | "./piqi_boot2" -> boot2 ()
      (*
      | "./piqi_reboot" -> reboot ()
      *)
      | _ -> assert false

