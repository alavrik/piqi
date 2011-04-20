(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
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


module C = Piqi_common  
open C


let make_param_name func param_name =
  (* construct the type name as a concatentation of the function name and
   * -input|output|error *)
  let func_name = func.T.Func#name in
  let type_name = func_name ^ "-" ^ param_name in
  (* create a location reference for the newly constructed type name *)
  Piqloc.addrefret func_name type_name


let make_param_record name x =
  let res =
    R#{
      (* TODO: provide a better solution for creating new records with some
       * optional/repeated extension fields set to None/[] by default *)
      T.default_record () with

      name = name;
      field = x.T.Anonymous_record.field;
    }
  in
  Piqloc.addref x res;
  `record (Piqi.copy_record res) (* preserve the original fields *)


let make_param_alias name x =
  let res =
    A#{
      T.default_alias () with

      name = name;
      typeref = `name x;
      is_func_param = true; (* mark the new alias as function parameter *)
    }
  in
  Piqloc.addref x res;
  `alias res


(* convert function parameter to a type:
 *  - if the function parameter is a name, convert it to correspondent alias
 *  - if the function parameter is an anonymous record, convert it to
 *    correspondent record
 *)
let resolve_param piqi idtable func param_name param =
  let type_name = make_param_name func param_name in
  let def =
    match param with
      | `name x ->
          (* make an alias from name reference *)
          make_param_alias type_name x
      | `anonymous_record x ->
          (* make a normal record with name = type_name from anonymous record *)
          make_param_record type_name x
  in
  Piqloc.addref param def;

  (* check and resolve a newly created alias or record *)
  ignore (Piqi.resolve_defs ~piqi idtable [def]);

  def


(* ughh. this is ugly *)
let resolve_param piqi idtable func param_name param =
  match param with
    | None -> None
    | Some param ->
        let res = resolve_param piqi idtable func param_name param in
        Some res


let process_func piqi idtable f =
  let open T.Func in
  begin
    Piqi.check_name f.name;
    let i = resolve_param piqi idtable f "input" f.input
    and o = resolve_param piqi idtable f "output" f.output
    and e = resolve_param piqi idtable f "error" f.error
    in T.Func#{
      f with
      resolved_input = i;
      resolved_output = o;
      resolved_error = e;
    }
  end


let check_dup_names idtable funs =
  let open P in
  let names = List.map (fun x -> x.T.Func#name) funs in
  Piqi.check_dup_names "function" names;
  (* check that function names do not conflict with type names *)
  ()
  (* XXX: This check turned out to be an annoying limitation. Commenting it out
   * for now: *)
  (*
  List.iter
    (fun name ->
      try
        let def = Piqi.Idtable.find idtable name in
        error name
          ("function name " ^ quote name ^ " conflicts with type name\n" ^
           error_string def "defined here")
      with Not_found -> ()
    ) names
  *)


let get_func_defs f =
  let open T.Func in
  let get_param = function
    | None -> []
    | Some x -> [ (x :> T.piqdef) ]
  in
  List.concat [
    get_param f.resolved_input;
    get_param f.resolved_output;
    get_param f.resolved_error;
  ]


let process_piqi idtable (piqi :T.piqi) =
  let open P in (
    (* get all functions from this module and included modules *)
    let funs = Piqi_ext.get_functions piqi.included_piqi in
    (* check for duplicate function names *)
    check_dup_names idtable funs;
    (* process functions and create a local copy of them *)
    let resolved_funs = List.map (process_func piqi idtable) funs in
    (* add function type definitions to Piqi resolved defs *)
    piqi.resolved_func <- resolved_funs;
    let fun_defs = flatmap get_func_defs resolved_funs in
    if fun_defs <> []
    then piqi.resolved_piqdef <- piqi.resolved_piqdef @ fun_defs
  )


(* boot code *)
let _ =
  (* register a hook for processing functions when a Piqi module is loaded *)
  Piqi.register_processing_hook process_piqi


(* we don't really need this call, but it is used in order to prevent exclusion
 * of this module during library linking *)
let init () = ()

