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
      name = name;
      field = x.T.Anonymous_record.field;

      proto_name = None; json_name = None; parent = None;
      wire_field = [];
    }
  in
  Piqloc.addref x res;
  `record res


let make_param_alias name x =
  let res =
    A#{
      name = name;
      typeref = `name x;

      wire_type = None; proto_name = None; proto_type = None;
      json_name = None; parent = None;
    }
  in
  Piqloc.addref x res;
  `alias res


(* verify that param is either a record or an alias pointing to a record *)
let verify_param t =
  match unalias t with
    | `record _ -> ()
    | _ -> error t "function parameter must point to a record"


(* convert function parameter to a type:
 *  - if the function parameter is a name, convert it to correspondent alias
 *  - if the function parameter is an anonymous record, convert it to
 *    correspondent record
 *)
let resolve_param idtable func param_name param =
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
  (* check and resolve a newly created alias or record *)
  (* TODO: set up parent namespace to local piqi defs *)
  ignore (Piqi.resolve_defs idtable [def]);

  (* verify that newly created alias is pointing to a record *)
  verify_param def;
  def


(* ughh. this is ugly *)
let resolve_param idtable func param_name param =
  match param with
    | None -> None
    | Some param ->
        let res = resolve_param idtable func param_name param in
        Some res


let process_func idtable f =
  let open T.Func in
  begin
    Piqi.check_name f.name;
    f.resolved_input <- resolve_param idtable f "input" f.input;
    f.resolved_output <- resolve_param idtable f "output" f.output;
    f.resolved_error <- resolve_param idtable f "error" f.error;
  end


let process_piqi idtable (piqi :T.piqi) =
  let open P in
  match piqi.func with
    | [] -> ()
    | l ->
        (* TODO: load functions from included modules *)
        (* TODO: check for duplicate function names *)
        List.iter (process_func idtable) l


(* boot code *)
let _ =
  (* register a hook for processing functions when a Piqi module is loaded *)
  Piqi.register_processing_hook process_piqi

