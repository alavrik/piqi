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
(* open Piqi_json_common *)


(*
 * set json- names if not specified by user
 *)


(* json name of piqi name *)
let json_name' n =
  dashes_to_underscores n


let json_name n =
  Some (json_name' n)


(* TODO: check name validity *)
let check_json_name json_name =
  ()


(* XXX: use name instead of json_name for foreign types? *)
let piqdef_json_name = function
  | `record t -> t.R#json_name
  | `variant t -> t.V#json_name
  | `enum t -> t.E#json_name
  | `alias t -> t.A#json_name
  | `list t -> t.L#json_name
  | _ ->
      (* this function will be called only for named types (i.e. piqdefs) *)
      assert false


let json_name_of name typeref =
  match name, typeref with
    | Some n, _ -> json_name n
    | None, Some t -> piqdef_json_name t
    | _ -> assert false


let json_name_field x =
  open Field in
  match x.json_name with
    | None -> x.json_name <- json_name_of x.name x.typeref
    | Some n -> check_json_name x.json_name


let json_name_record x =
  open Record in
  (match x.json_name with
     | None -> x.json_name <- json_name x.name
     | Some n -> check_json_name x.json_name
  )


let json_name_option x =
  open Option in
  match x.json_name with
    | None -> x.json_name <- json_name_of x.name x.typeref
    | Some n -> check_json_name x.json_name


let json_name_variant x =
  open Variant in
  (match x.json_name with
     | None -> x.json_name <- json_name x.name
     | Some n -> check_json_name x.json_name
  )


let json_name_alias x =
  open Alias in
  match x.json_name with
    | None -> x.json_name <- json_name x.name
    | Some n -> check_json_name x.json_name


let json_name_list x =
  open L in
  match x.json_name with
    | None -> x.json_name <- json_name x.name
    | Some n -> check_json_name x.json_name


let json_name_piqdef = function
  | `record x -> json_name_record x
  | `variant x | `enum x -> json_name_variant x
  | `alias x -> json_name_alias x
  | `list x -> json_name_list x


(* name fields and options *)
let json_name_record' x =
   List.iter json_name_field x.R#field

let json_name_variant' x =
   List.iter json_name_option x.V#option

let json_name_piqdef' = function
  | `record x -> json_name_record' x
  | `variant x | `enum x -> json_name_variant' x
  | _ -> ()


let json_name_defs defs =
    (* name data structures *)
    List.iter json_name_piqdef defs;
    (* name fields and options *)
    List.iter json_name_piqdef' defs


let json_name_piqi (piqi:T.piqi) =
  open P in
  json_name_defs piqi.P#resolved_piqdef


let init () =
  trace "init JSON\n";
  (* add JSON names when loading Piqi JSON *)
  Piqi.init ();
  Piqi.add_processing_hook json_name_piqi


(* boot code *)
let _ =
  json_name_defs T.piqdef_list


module Main = Piqi_main
open Main


let usage = "Usage: piqi json-pp [options] [<.json file>] [output file]\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    arg__;
  ]


let prettyprint_json ch json =
  Piqi_json_gen.pretty_to_channel ch json


let open_json fname =
  let ch = Piqi_main.open_input fname in
  let json_parser = Piqi_json_parser.init_from_channel ~fname ch in
  json_parser


let read_json_obj json_parser =
  Piqi_json_parser.read_next json_parser


let prettyprint_json ch json_parser =
  let rec aux () =
    match read_json_obj json_parser with
      | None -> ()
      | Some json ->
          prettyprint_json ch json;
          output_string ch "\n\n";
          aux ()
  in aux ()


let prettyprint_file filename =
  let ch = Main.open_output !ofile in
  (* switch parser/generator to pretty-print mode *)
  Config.pp_mode := true;
  let json_parser = open_json filename in
  prettyprint_json ch json_parser


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  prettyprint_file !ifile

 
let _ =
  Main.register_command run "json-pp" "pretty-print %.json"

