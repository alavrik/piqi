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


module R = Record
module F = Field
module V = Variant
module O = Option
module E = Variant
module A = Alias
module L = Piqlist


let piqdef_list =
  [
    `record
      R#{
        name = "piqi";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "module";
              typeref = Some `word;
              mode = `optional;
              default = None;
              ocaml_name = Some "modname"; code = None;
            };
            F#{
              name = Some "ocaml-module";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "piqdef");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "include");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "import");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "extend");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "custom-field";
              typeref = Some `word;
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };


            F#{
              name = Some "extended-piqdef";
              typeref = Some (`name "piqdef");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "resolved-piqdef";
              typeref = Some (`name "piqdef");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "imported-piqdef";
              typeref = Some (`name "piqdef");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "resolved-import";
              typeref = Some (`name "import");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "included-piqi";
              typeref = Some (`name "piqi");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "original-piqi";
              typeref = Some (`name "piqi");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `variant
      V#{
        name = "piqdef";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{ name = None; typeref = Some (`name "record");  ocaml_name = None; code = None; };
            O#{ name = None; typeref = Some (`name "variant"); ocaml_name = None; code = None; };
            O#{ name = None; typeref = Some (`name "enum");    ocaml_name = None; code = None; };
            O#{ name = None; typeref = Some (`name "alias");   ocaml_name = None; code = None; };
            O#{ name = None; typeref = Some (`name "list");    ocaml_name = Some "list"; code = None; };
          ];
      };
    `list
      L#{
        name = "piqdef-list";
        ocaml_name = None;
        typeref = `name "piqdef";
        parent = None;
      };
    `variant
      V#{
        name = "piqtype";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{ name = None; typeref = Some (`name "piqdef"); ocaml_name = None; code = None; };
            (* built-in types *)
            O#{ name = Some "int";    typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "float";  typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "bool";   typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "string"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "binary"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "text";   typeref = None; ocaml_name = None; code = None; };

            O#{ name = Some "word";   typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "any";    typeref = None; ocaml_name = None; code = None; };
          ];
      };
    `variant
      V#{
        name = "type";
        ocaml_name = Some "typeref";
        parent = None;
        option = 
          [
            O#{ name = Some "name"; typeref = Some `word;             ocaml_name = None; code = None; };
            O#{ name = None; typeref = Some (`name "piqtype");        ocaml_name = None; code = None; };
          ];
      };
    `record
      R#{
        name = "record";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `required;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "field");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "wire-field";
              typeref = Some (`name "field");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `record
      R#{
        name = "field";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "type");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "mode";
              typeref = Some (`name "field-mode");
              mode = `optional;
              default =
                 Some Any#{ ast = Some (`name "required"); binobj = None; };
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "default";
              typeref = Some `any;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "code";
              typeref = Some (`name "int32");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `variant
      V#{
        name = "field-mode";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{ name = Some "required"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "optional"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "repeated"; typeref = None; ocaml_name = None; code = None; };
          ];
      };
    `record
      R#{
        name = "variant";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `required;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "option");
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `record
      R#{
        name = "option";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "type");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "code";
              typeref = Some (`name "int32");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `alias
      A#{
        name = "int32";
        ocaml_name = None;
        typeref = `int;
        wire_type = Some `zigzag_varint; ocaml_type = Some "int32";
        parent = None;
      };
    `alias
      A#{
        name = "enum";
        ocaml_name = None;
        typeref = `name "variant";
        wire_type = None; ocaml_type = None;
        parent = None;
      };
    `record
      R#{
        name = "alias";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `required;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "type");
              mode = `required;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "wire-type");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-type";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `record
      R#{
        name = "list";
        ocaml_name = Some "piqlist";
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `required;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "type");
              mode = `required;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `record
      R#{
        name = "include";
        ocaml_name = Some "includ";
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "module";
              typeref = Some `word;
              mode = `required;
              default = None;
              ocaml_name = Some "modname"; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "piqi");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `record
      R#{
        name = "import";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "module";
              typeref = Some `word;
              mode = `required;
              default = None;
              ocaml_name = Some "modname"; code = None;
            };
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              typeref = Some (`name "piqi");
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
              default = None;
              ocaml_name = None; code = None;
            };
          ]
      };
    `record
      R#{
        name = "extend";
        ocaml_name = None;
        parent = None;
        wire_field = [];
        field =
          [
            F#{
              name = Some "name";
              typeref = Some `word;
              mode = `repeated;
              default = None;
              ocaml_name = None; code = None;
            };
            F#{
              name = None;
              ocaml_name = Some "quote";
              typeref = Some `any;
              mode = `repeated;
              default = None;
              code = None;
            };
          ]
      };
    `enum
      E#{
        name = "ocaml-type";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{ name = Some "int"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "int32"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "int64"; typeref = None; ocaml_name = None; code = None; };
          ];
      };
    `enum
      E#{
        name = "wire-type";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{ name = Some "varint"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "zigzag-varint"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "fixed32"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "fixed64"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "signed-varint"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "signed-fixed32"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "signed-fixed64"; typeref = None; ocaml_name = None; code = None; };
            O#{ name = Some "block"; typeref = None; ocaml_name = None; code = None; };
          ];
      };
    `variant
      V#{
        name = "namespace";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{ name = None; typeref = Some (`name "piqi");    ocaml_name = None; code = None; };
            O#{ name = None; typeref = Some (`name "import");  ocaml_name = None; code = None; };
          ];
      };
  ]


let piqi =
  Piqi#{
    (* using piqi.org/piqtype instead of piqi.org/piqi to generate hashcodes
     * otherwise, serial wire codes would be generated *)
    modname = Some "piqi.org/piqtype";
    ocaml_module = Some "Piqtype";

    piqdef = piqdef_list;
    includ = [];
    import = [];
    extend = [];

    custom_field = [];

    extended_piqdef = [];
    resolved_piqdef = [];
    imported_piqdef = [];
    resolved_import = [];
    included_piqi = [];
    original_piqi = None;
  }

