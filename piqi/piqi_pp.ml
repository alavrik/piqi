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


let map_words (x:T.ast) f =
  let rec aux = function
    | `word s -> `word (f s)
    | `name s -> `name (f s)
    | `named ({T.Named.value = v} as x) ->
        `named {x with T.Named.value = aux v}
    | `typed ({T.Typed.value = v} as x) ->
        let ast = some_of v.T.Any.ast in
        let v = {v with T.Any.ast = Some (aux ast)} in
       `typed {x with T.Typed.value = v}
    | `list l -> `list (List.map aux l)
    | `control l -> `control (List.map aux l)
    | x -> x
  in aux x


let normalize ast =
  map_words ast Piqi_name.normalize_name


module Main = Piqi_main
open Main


let flag_normalize = ref false
let flag_expand_abbr = ref false


let usage = "Usage: piqi pp [options] [<.piqi|.piq file>] [output file]\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;

    "--normalize-words", Arg.Set flag_normalize,
    "normalize all words while pretty-printing (convert CamelCase to camel-case)";

    "--expand-abbr", Arg.Set flag_expand_abbr,
    "expand built-in syntax abbreviations";

    arg__;
  ]


(* old method for pretty-printing:
let prettyprint_ast ast =
  let code = Piq_gen.print_ast ast in
  Iolist.to_channel ch code
*)


let prettyprint_ast ch ast =
  Piq_gen.to_channel ch ast


let rec prettyprint_list ch ast_list =
  let rec aux = function
    | [] -> ()
    | [x] -> prettyprint_ast ch x
    | h::t ->
          prettyprint_ast ch h;
          output_char ch '\n';
          aux t
  in aux ast_list


let transform_ast path f (ast:T.ast) =
  let rec aux p = function
    | `list l when p = [] -> (* leaf node *)
        (* f replaces, removes element, or splices elements of the list *)
        let res = flatmap f l in
        `list res
    | x when p = [] -> (* leaf node *)
        (* expecting f to replace the existing value, no other modifications
         * such as removal or splicing is allowed in this context *)
        (match f x with [res] -> res | _ -> assert false)
    | `list l ->
        (* haven't reached the leaf node => continue tree traversal *)
        let res = List.map (aux p) l in
        `list res
    | `named {T.Named.name = n; T.Named.value = v} when List.hd p = n ->
        (* found path element => continue tree traversal *)
        let res = T.Named#{name = n; value = aux (List.tl p) v} in
        `named res
    | x -> x
  in
  aux path ast


(* simplify piqi ast: *)
let simplify_piqi_ast (ast:T.ast) =
  let tr = transform_ast in
  (* map piqdef.x -> x *)
  let rm_piqdef =
    tr [] (
      function
        | `named {T.Named.name = "piqdef"; T.Named.value = v} -> [v]
        | x -> [x]
    )
  (* del record/field/mode.required *)
  (* map record/field/mode x -> x *)
  and tr_field_mode_required =
    tr ["record"; "field"] (
      function
        | `named {T.Named.name = "mode"; T.Named.value = `name "required"} -> []
        | `named {T.Named.name = "mode"; T.Named.value = (`name _) as x} -> [x]
        | x -> [x]
    )
  (* map extend/.piq-any x -> x *)
  and tr_extend_piq_any =
    tr ["extend"] (
      function
        | `named {T.Named.name = "piq-any"; T.Named.value = v} -> [v]
        | x -> [x]
    )
  (* //type.name x -> type.x *)
  and tr_type_name_common path =
    tr path (
      function
        | `named ({T.Named.name = "type"; T.Named.value = `named {T.Named.name = "name"; T.Named.value = v}} as x) ->
            let res = `named T.Named#{x with value = v} in
            [res]
        | x -> [x]
    )
  in
  (* map record/field/type.name x -> type x *)
  let tr_record_field_type_name = tr_type_name_common ["record"; "field"]
  (* map variant/option/type.name x -> type x *)
  and tr_variant_option_type_name  = tr_type_name_common ["variant"; "option"]
  (* map enum/option/type.name x -> type x *)
  and tr_enum_option_type_name = tr_type_name_common ["enum"; "option"]
  (* map alias/type.name x -> type x *)
  and tr_alias_type_name = tr_type_name_common ["alias"]
  (* map list/type.name x -> type x *)
  and tr_list_type_name = tr_type_name_common ["list"]
  in
  let (|>) a f = f a in
  ast |>
  rm_piqdef |>
  tr_field_mode_required |>
  tr_extend_piq_any |>
  tr_record_field_type_name |>
  tr_variant_option_type_name |>
  tr_enum_option_type_name |>
  tr_alias_type_name |>
  tr_list_type_name


let prettyprint_piqi_ast ?(simplify=false) ch ast =
  let ast =
    if simplify
    then simplify_piqi_ast ast
    else ast
  in
  match ast with
    | `list l -> prettyprint_list ch l
    | _ -> assert false


let transform_ast ast =
  let ast =
    if !flag_normalize
    then normalize ast
    else ast
  in
  let ast =
    if !flag_expand_abbr
    then Piq_parser.expand ast
    else ast
  in ast


let open_piq fname =
  let ch = Piqi_main.open_input fname in
  let piq_parser = Piq_parser.init_from_channel fname ch in
  piq_parser


let read_piq_obj piq_parser =
  Piq_parser.read_next piq_parser


let prettyprint_piq ch piq_parser =
  let rec aux () =
    match read_piq_obj piq_parser with
      | None -> ()
      | Some ast ->
          let ast = transform_ast ast in
          prettyprint_ast ch ast;
          output_char ch '\n';
          aux ()
  in aux ()


let prettyprint_file filename =
  let ch = Main.open_output !ofile in
  (* switch piq parser/generator to pretty-print mode *)
  Config.pp_mode := true;
  let piq_parser = open_piq filename in
  prettyprint_piq ch piq_parser


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  prettyprint_file !ifile

 
let _ =
  Main.register_command run "pp" "pretty-print %.piqi or %.piq"

