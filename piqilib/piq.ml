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

(* handling of Piq-specific Piqi properties *)


module C = Piqi_common
open C


(* check correspondent between primitive Piqi type and Piq representation format
 *)
let check_piq_format obj piq_format piqtype =
  let piqtype = C.unalias piqtype in
  match piq_format, piqtype with
    | `word, `string -> ()
    | `text, `string -> ()
    | _ when C.is_typedef piqtype ->
        error obj
          ("piq-format can not be defined for non-primitive type " ^ U.quote (C.piqi_typename piqtype))
    | _ ->
        error obj
          ("invalid piq-format for type " ^ U.quote (C.piqi_typename piqtype))


let rec resolve_piq_format (piqtype: T.piqtype) =
  (* upper-level setting overrides lower-level setting *)
  match piqtype with
    | `alias x ->
        let piq_format = x.A.piq_format in
        if piq_format <> None
        then piq_format
        else
          (* try looking in lower-level aliases *)
          resolve_piq_format (some_of x.A.piqtype)
    | _ ->
        None (* piq format can not be defined for non-primitive types *)


let check_resolve_piq_format obj piq_format piqtype =
  match piq_format, piqtype with
    | Some f, Some t -> (* already defined, just check *)
        check_piq_format obj f t;
        piq_format
    | None, Some t ->
        resolve_piq_format t
    | Some t, None ->
        error obj "piq-format can not be defined when there is no type"
    | None, None ->
        None


let resolve_field_piq_format x =
  let open F in
  x.piq_format <- check_resolve_piq_format x x.piq_format x.piqtype


let resolve_option_piq_format x =
  let open O in
  x.piq_format <- check_resolve_piq_format x x.piq_format x.piqtype


let resolve_typedef_piq_format = function
  | `record r ->
      List.iter resolve_field_piq_format r.R.field
  | `variant v ->
      List.iter resolve_option_piq_format v.V.option
  | `alias a ->
      a.A.piq_format <- check_resolve_piq_format a a.A.piq_format a.A.piqtype
  | `list l ->
      l.L.piq_format <- check_resolve_piq_format l l.L.piq_format l.L.piqtype
  | `enum _ ->
      ()


let process_field_piq_positional record_piq_positional x =
  let open F in
  begin
    (match x.name, x.typename with
      | Some _, None ->  (* flag *)
          if x.piq_positional = Some true
          then error x "flags can not be positional"
      | _ -> ()
    );

    (* inherit the record-level setting when the local per-field setting is
     * missing *)
    if x.piq_positional = None
    then x.piq_positional <- record_piq_positional
  end


let process_typedef_piq_positional = function
  | `record x ->
      List.iter (process_field_piq_positional x.R.piq_positional) x.R.field
  | _ -> ()


let check_name x =
  if not (Piqi_name.is_valid_name x)
  then error x ("invalid piq alias name: " ^ U.quote x)
  else ()


let check_opt_name = function
  | None -> ()
  | Some x -> check_name x


let check_field_piq_alias x =
  check_opt_name x.F.piq_alias


let check_option_piq_alias x =
  check_opt_name x.O.piq_alias


let check_typedef_piq_alias = function
  | `record x ->
      List.iter check_field_piq_alias x.R.field
  | `variant x ->
      List.iter check_option_piq_alias x.V.option
  | _ -> ()


let process_typedefs typedefs =
  (* resolve Piq representation format settings *)
  List.iter resolve_typedef_piq_format typedefs;
  (* stuff related to .piq-positional property *)
  List.iter process_typedef_piq_positional typedefs;
  (* validate .piq-alias names
   * TODO, XXX: check for all sorts of duplicates; warn if .name masks
   * .piq-alias *)
  List.iter check_typedef_piq_alias typedefs;
  ()


(* for internal use only: string -> piq_ast *)
let piq_of_string s :piq_ast =
  let piq_parser = Piq_parser.init_from_string "embedded" s in
  let res =
    try Piq_parser.read_all piq_parser
    with C.Error ((_, lnum', cnum'), error) ->
      (* string location can be missing when we parse from Piq embedded some
       * other representation, e.g. Protobuf or XML *)
      let (fname, lnum, cnum) =
        try Piqloc.find s
        with Not_found -> ("embedded", 1, -1)
      in
      (* XXX, TODO: adjust location -- this does't work well if there are
       * escaped characters in a Json string; in particular, newlines (which are
       * always escaped in json strings) will throw things off significantly *)
      let loc = (fname, lnum + lnum' - 1, cnum + cnum') in
      C.error_at loc ("error parsing embedded Piq: " ^ error)
  in
  match res with
    | [x] -> x
    | _::o::_ ->
        C.error o "string includes more than one Piq value"
    | [] ->
        C.error s "string doesn't have Piq data"

let _ =
  Piqobj.piq_of_string := (fun x -> piq_of_string x);
  Piqobj.string_of_piq := (fun x -> Piq_gen.to_string x ~nl:false)


let rec to_portable_ast (ast:piq_ast) :Piq_piqi.piq_node =
  let loc =
      try
        let (file, line, column) = Piqloc.find ast in
        Some Piq_piqi.Loc.({file = file; line = line; column = column})
      with
        Not_found -> None
  in
  let piq =
    match ast with
      | `int (x, _) -> `int x
      | `uint (x, _) -> `uint x
      | `float (x, _) -> `float x
      | `bool x -> `bool x
      | `string (x, _) -> `string x
      | `raw_string x -> `raw_string x
      | `word x -> `word x
      | `text x -> `text x
      | `binary (x, _) -> `binary x
      | `name x -> `name x
      | `typename x -> `typename x
      | `named {Piq_ast.Named.name = name; Piq_ast.Named.value = value} ->
          `named Piq_piqi.Named.({name = name; value = to_portable_ast value})
      | `typed {Piq_ast.Typed.typename = typename; Piq_ast.Typed.value = value} ->
          `typed Piq_piqi.Typed.({typename = typename; value = to_portable_ast value})
      | `list l ->
          `list (List.map to_portable_ast l)
      | `form (`name name, args) ->
          `splice {Piq_piqi.Splice.name = name; Piq_piqi.Splice.item = List.map to_portable_ast args}
      (* XXX, TODO: any shouldn't be used in plain Piq ASTs? *)
      | `form _ -> assert false
      | `any _ -> assert false
  in
  Piq_piqi.Piq_node.({piq = piq; loc = loc})


let addloc loc ast =
  (* TODO: this is not enough -- need to add location to the option values
   * themselves *)
  Piqloc.setloc loc;
  (match ast with
    | `int (x, _) -> Piqloc.add x
    | `uint (x, _) -> Piqloc.add x
    | `float (x, _) -> Piqloc.add x
    | `bool x -> ()
    | `string (x, _) -> Piqloc.add x
    | `raw_string x -> Piqloc.add x
    | `word x -> Piqloc.add x
    | `text x -> Piqloc.add x
    | `binary (x, _) -> Piqloc.add x
    | `name x -> Piqloc.add x
    | `typename x -> Piqloc.add x
    | `named {Piq_ast.Named.name = name; Piq_ast.Named.value = value} ->
        Piqloc.add name
    | `typed {Piq_ast.Typed.typename = typename; Piq_ast.Typed.value = value} ->
        Piqloc.add typename
    | `list l ->
        Piqloc.add l
    | `form ((`name s) as name, args) ->
        Piqloc.add s;
        Piqloc.add name
    (* XXX, TODO: any shouldn't be used in plain Piq ASTs? *)
    | `form (_, _) -> assert false
    | `any _ -> assert false
  );
  Piqloc.add ast


let rec of_portable_ast (piq_node:Piq_piqi.piq_node) :piq_ast =
  let rec aux piq_node =
    let ast =
      match piq_node.Piq_piqi.Piq_node.piq with
        | `int x -> `int (x, "")
        | `uint x -> `uint (x, "")
        | `float x -> `float (x, "")
        | `bool x -> `bool x
        | `string x -> `string (x, "")
        | `raw_string x -> `raw_string x
        | `word x -> `word x
        | `text x -> `text x
        | `binary x -> `binary (x, "")
        | `name x -> `name x
        | `typename x -> `typename x
        | `named {Piq_piqi.Named.name = name; Piq_piqi.Named.value = value} ->
            `named Piq_ast.Named.({name = name; value = aux value})
        | `typed {Piq_piqi.Typed.typename = typename; Piq_piqi.Typed.value = value} ->
            `typed Piq_ast.Typed.({typename = typename; value = aux value})
        | `list l ->
            `list (List.map aux l)
        | `splice {Piq_piqi.Splice.name = name; Piq_piqi.Splice.item = items} ->
            `form (`name name, List.map aux items)
    in
    let open Piq_piqi.Loc in
    (match piq_node.Piq_piqi.Piq_node.loc with
      | None -> ()
      | Some {file = file; line = line; column = column} ->
          let loc = (file, line, column) in
          addloc loc ast
    );
    ast
  in
  aux piq_node
