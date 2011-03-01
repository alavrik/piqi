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
 * This module generates Erlang -spec(...) and default implementation for
 * functions.
 *)


open Piqi_common
open Iolist


(* generate Input | Output | Error typename *)
let gen_typename = function
  | `alias x ->
      (* generate the name of the original type that was used in definition *)
      Piqic_erlang_types.ios_gen_out_typeref x.A#typeref
  | (`record _) as t ->
      (* "#" <function-name> ("-input" | "-output" | "-error") "{}" *)
      let name = Piqic_erlang_types.gen_piqtype t None in
      iol [ ios "#"; ios name; ios "{}" ]


let gen_spec f =
  let open T.Func in
  let input, arity =
    match f.resolved_input with
      | None -> ios "'undefined'", "1"
      | Some t -> gen_typename t, "1"
  in
  let output =
    match f.resolved_output, f.resolved_error with
      | None, None -> ios "ok"
      | out, err ->
          let gen_tuple atom t =
            iol [
              ios "{";
                ios atom; ios ", "; gen_typename t;
              ios "}";
            ]
          in
          let output =
            match out with
              | None -> ios "ok"
              | Some t -> gen_tuple "ok" t
          in
          match err with
            | None -> output
            | Some t ->
                iol [ output; ios " | "; gen_tuple "error" t ]
  in
  iol [
    ios "-spec "; ios (some_of f.erlang_name); ios "/"; ios arity; ios " :: ";
      ios "("; input; ios ") -> "; output; ios "."
  ]


let gen_function_specs (piqi:T.piqi) =
  let specs = List.map gen_spec piqi.P#resolved_func in
  iod "\n" specs


let gen_default_impl modname f =
  let open T.Func in
  let fname = some_of f.erlang_name in
  let input =
    match f.resolved_input with
      | None -> ios "_"
      | Some t -> ios "_Input"
  in
  let output =
    match f.resolved_output with
      | None -> ios "ok";
      | Some x ->
          iol [
            ios "{ok, ";
              ios modname; ios ":default_"; ios fname; ios "_output()";
            ios "}";
          ]
  in
  iol [
    ios fname; ios "("; input; ios ") -> "; output; ios "."
  ]


let gen_function_default_impls (piqi:T.piqi) =
  let modname = some_of piqi.P#erlang_module in
  let impls = List.map (gen_default_impl modname) piqi.P#resolved_func in
  iod "\n" impls

