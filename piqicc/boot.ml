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


open Piqi_common


let boot () = 
  (* call piq interface compiler for ocaml *)
  Piqic_ocaml_types.cc_mode := true;
  let code = Piqic_ocaml_base.piqic T.piqi in
  Iolist.to_channel stdout code;
  print_endline "let embedded_piqi :(string * string) list ref = ref []";
  ()


let boot2 () = 
  (*
  Piqi_config.debug_level := 2;
  *)

  (* set piqi boot module *)
  let boot_fname = "boot/piqi-boot.piqi" in
  (* reload the boot module from the file *)
  Piqi.load_boot_piqi boot_fname;

  let fname = "boot/piqast.piqi" in
  let piqi = Piqi.load_piqi fname in

  (* add hash-based field and option codes instead of auto-enumerated ones
   *
   * NOTE: at later boot stages and in general, this is handled automatically
   * in Piqi module
   *)
  Piqi_wire.add_hashcodes piqi.P#resolved_piqdef;
  (* check for hash conflicts and pre-order fields by hash codes *)
  Piqi_wire.add_codes piqi.P#resolved_piqdef;

  (* call piq interface compiler for ocaml *)
  Piqic_ocaml_types.cc_mode := true;
  let code = Piqic_ocaml_base.piqic piqi in
  Iolist.to_channel stdout code;
  print_endline "let embedded_piqi :(string * string) list ref = ref []";
  ()


let _ =
  if !Sys.interactive
  then ()
  else
    match Sys.argv.(0) with
      | "./piqi_boot" -> boot ()
      | "./piqi_boot2" -> boot2 ()
      | _ -> assert false

