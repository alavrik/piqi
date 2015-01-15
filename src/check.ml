(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

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

(* "piqi check" validates input and essentially the same as "piqi convert"
 * except it doesn't produce any output *)

module C = Piqi_common  
open C


(* command-line arguments *)

let usage = "Usage: piqi check [options] [input file]\nOptions:"


let speclist = Main.common_speclist @
  [
    Main.arg__strict;

    Convert.arg_f;
    Convert.arg__type;
    Convert.arg__piq_relaxed_parsing;
    Convert.arg__piq_frameless_input;

    Main.arg__include_extension;
    Main.arg__;
  ]


let check_file () =
  Convert.init ();

  let input_format = Convert.get_input_format () in
  let reader = Convert.make_reader input_format in

  (* main convert cycle *)
  Convert.do_convert reader


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1;
  check_file ()

 
let _ =
  Main.register_command run "check"
    "check data validity for various file formats (piqi, piq, json, xml, pb, pib)"

