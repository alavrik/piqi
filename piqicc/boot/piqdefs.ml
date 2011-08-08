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


module R = Record
module F = Field
module V = Variant
module O = Option
module E = Variant
module A = Alias
module L = Piqlist


(* default field *)
let f = F#{
  name = None;
  typeref = None;
  mode = `required;
  default = None;

  alt_name = None;
  ocaml_name = None;
  code = None;

  wire_packed = false;
  ocaml_array = false;
}


(* default option *)
let o = O#{
  name = None;
  typeref = None;

  alt_name = None;
  ocaml_name = None;
  code = None;
}


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
            F#{f with
              name = Some "module";
              typeref = Some `word;
              mode = `optional;
              ocaml_name = Some "modname";
            };
            F#{f with
              name = Some "ocaml-module";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = None;
              typeref = Some (`name "piqdef");
              mode = `repeated;
            };
            F#{f with
              name = None;
              typeref = Some (`name "include");
              mode = `repeated;
            };
            F#{f with
              name = None;
              typeref = Some (`name "import");
              mode = `repeated;
            };
            F#{f with
              name = None;
              typeref = Some (`name "extend");
              mode = `repeated;
            };
            F#{f with
              name = Some "custom-field";
              typeref = Some `word;
              mode = `repeated;
            };


            F#{f with
              name = Some "extended-piqdef";
              typeref = Some (`name "piqdef");
              mode = `repeated;
            };
            F#{f with
              name = Some "resolved-piqdef";
              typeref = Some (`name "piqdef");
              mode = `repeated;
            };
            F#{f with
              name = Some "imported-piqdef";
              typeref = Some (`name "piqdef");
              mode = `repeated;
            };
            F#{f with
              name = Some "resolved-import";
              typeref = Some (`name "import");
              mode = `repeated;
            };
            F#{f with
              name = Some "included-piqi";
              typeref = Some (`name "piqi");
              mode = `repeated;
            };
            F#{f with
              name = Some "original-piqi";
              typeref = Some (`name "piqi");
              mode = `optional;
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
            O#{o with typeref = Some (`name "record");  };
            O#{o with typeref = Some (`name "variant"); };
            O#{o with typeref = Some (`name "enum");    };
            O#{o with typeref = Some (`name "alias");   };
            O#{o with typeref = Some (`name "list"); ocaml_name = Some "list"; };
          ];
      };
    `list
      L#{
        name = "piqdef-list";
        ocaml_name = None;
        typeref = `name "piqdef";
        parent = None;
        wire_packed = false;
        ocaml_array = false;
      };
    `variant
      V#{
        name = "piqtype";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{o with name = None; typeref = Some (`name "piqdef"); };
            (* built-in types *)
            O#{o with name = Some "int";    typeref = None; };
            O#{o with name = Some "float";  typeref = None; };
            O#{o with name = Some "bool";   typeref = None; };
            O#{o with name = Some "string"; typeref = None; };
            O#{o with name = Some "binary"; typeref = None; };
            O#{o with name = Some "text";   typeref = None; };

            O#{o with name = Some "word";   typeref = None; };
            O#{o with name = Some "any";    typeref = None; };
          ];
      };
    `variant
      V#{
        name = "type";
        ocaml_name = Some "typeref";
        parent = None;
        option = 
          [
            O#{o with name = Some "name"; typeref = Some `word;      };
            O#{o with name = None; typeref = Some (`name "piqtype"); };
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `required;
            };
            F#{f with
              name = None;
              typeref = Some (`name "field");
              mode = `repeated;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
            };
            F#{f with
              name = Some "wire-field";
              typeref = Some (`name "field");
              mode = `repeated;
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `optional;
            };
            F#{f with
              name = None;
              typeref = Some (`name "type");
              mode = `optional;
            };
            F#{f with
              name = Some "mode";
              typeref = Some (`name "field-mode");
              mode = `optional;
              default =
                 Some Any#{ ast = Some (`name "required"); binobj = None; };
            };
            F#{f with
              name = Some "default";
              typeref = Some `any;
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-array";
              typeref = None;
              mode = `optional;
            };
            F#{f with
              name = Some "code";
              typeref = Some (`name "int32");
              mode = `optional;
            };
            F#{f with
              name = Some "wire-packed";
              typeref = None;
              mode = `optional;
            };
            F#{f with
              name = Some "alt-name";
              typeref = Some `word;
              mode = `optional;
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
            O#{o with name = Some "required"; typeref = None; };
            O#{o with name = Some "optional"; typeref = None; };
            O#{o with name = Some "repeated"; typeref = None; };
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `required;
            };
            F#{f with
              name = None;
              typeref = Some (`name "option");
              mode = `repeated;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `optional;
            };
            F#{f with
              name = None;
              typeref = Some (`name "type");
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = Some "code";
              typeref = Some (`name "int32");
              mode = `optional;
            };
            F#{f with
              name = Some "alt-name";
              typeref = Some `word;
              mode = `optional;
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `required;
            };
            F#{f with
              name = None;
              typeref = Some (`name "type");
              mode = `required;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = None;
              typeref = Some (`name "wire-type");
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-type";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `required;
            };
            F#{f with
              name = None;
              typeref = Some (`name "type");
              mode = `required;
            };
            F#{f with
              name = Some "wire-packed";
              typeref = None;
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-array";
              typeref = None;
              mode = `optional;
            };
            F#{f with
              name = Some "parent";
              typeref = Some (`name "namespace");
              mode = `optional;
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
            F#{f with
              name = Some "module";
              typeref = Some `word;
              mode = `required;
              ocaml_name = Some "modname";
            };
            F#{f with
              name = None;
              typeref = Some (`name "piqi");
              mode = `optional;
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
            F#{f with
              name = Some "module";
              typeref = Some `word;
              mode = `required;
              ocaml_name = Some "modname";
            };
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `optional;
            };
            F#{f with
              name = None;
              typeref = Some (`name "piqi");
              mode = `optional;
            };
            F#{f with
              name = Some "ocaml-name";
              typeref = Some `string;
              mode = `optional;
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
            F#{f with
              name = Some "name";
              typeref = Some `word;
              mode = `repeated;
            };
            F#{f with
              name = None;
              ocaml_name = Some "quote";
              typeref = Some `any;
              mode = `repeated;
            };
          ]
      };
    `enum
      E#{
        name = "wire-type";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{o with name = Some "varint"; };
            O#{o with name = Some "zigzag-varint"; };
            O#{o with name = Some "fixed32"; };
            O#{o with name = Some "fixed64"; };
            O#{o with name = Some "signed-varint"; };
            O#{o with name = Some "signed-fixed32"; };
            O#{o with name = Some "signed-fixed64"; };
            O#{o with name = Some "block"; };
          ];
      };
    `variant
      V#{
        name = "namespace";
        ocaml_name = None;
        parent = None;
        option = 
          [
            O#{o with name = None; typeref = Some (`name "piqi");    };
            O#{o with name = None; typeref = Some (`name "import");  };
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


(* create an empty boot Piqi module with no definitions at all, as the boot
 * piqdefs above to not require normal boot types definitions *)
let boot_piqi =
  Piqi#{
    piqi with
    piqdef = [];
  }

