module Piqirun = Piqi_piqirun

module rec Piq_piqi:
  sig
    type float64 = float
    type uint64 = int64
    type float = Piq_piqi.float64
    type binary = string
    type word = string
    type name = Piq_piqi.word
    type piq =
      [
        | `int of int64
        | `uint of Piq_piqi.uint64
        | `float of Piq_piqi.float
        | `bool of bool
        | `binary of Piq_piqi.binary
        | `string of string
        | `word of Piq_piqi.word
        | `text of string
        | `raw_string of Piq_piqi.binary
        | `name of Piq_piqi.name
        | `named of Piq_piqi.named
        | `typename of Piq_piqi.name
        | `typed of Piq_piqi.typed
        | `list of Piq_piqi.piq_list
        | `splice of Piq_piqi.splice
      ]
    type piq_node = Piq_node.t
    type loc = Loc.t
    type piq_list = Piq_piqi.piq_node list
    type named = Named.t
    type splice = Splice.t
    type typed = Typed.t
  end = Piq_piqi
and Piq_node:
  sig
    type t = {
      mutable piq: Piq_piqi.piq;
      mutable loc: Piq_piqi.loc option;
    }
  end = Piq_node
and Loc:
  sig
    type t = {
      mutable file: string;
      mutable line: int;
      mutable column: int;
    }
  end = Loc
and Named:
  sig
    type t = {
      mutable name: Piq_piqi.name;
      mutable value: Piq_piqi.piq_node;
    }
  end = Named
and Splice:
  sig
    type t = {
      mutable name: Piq_piqi.name;
      mutable item: Piq_piqi.piq_node list;
    }
  end = Splice
and Typed:
  sig
    type t = {
      mutable typename: Piq_piqi.name;
      mutable value: Piq_piqi.piq_node;
    }
  end = Typed


let rec parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x

and parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x

and parse_uint64 x = Piqirun.int64_of_varint x
and packed_parse_uint64 x = Piqirun.int64_of_packed_varint x

and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_binary x = Piqirun.string_of_block x

and parse_string x = Piqirun.string_of_block x

and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_piq x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 ->
        let res = parse_int64 x in
        `int res
    | 2 ->
        let res = parse_uint64 x in
        `uint res
    | 3 ->
        let res = parse_float x in
        `float res
    | 4 ->
        let res = parse_bool x in
        `bool res
    | 5 ->
        let res = parse_binary x in
        `binary res
    | 6 ->
        let res = parse_string x in
        `string res
    | 7 ->
        let res = parse_word x in
        `word res
    | 8 ->
        let res = parse_string x in
        `text res
    | 9 ->
        let res = parse_binary x in
        `raw_string res
    | 10 ->
        let res = parse_name x in
        `name res
    | 11 ->
        let res = parse_named x in
        `named res
    | 12 ->
        let res = parse_name x in
        `typename res
    | 13 ->
        let res = parse_typed x in
        `typed res
    | 14 ->
        let res = parse_piq_list x in
        `list res
    | 15 ->
        let res = parse_splice x in
        `splice res
    | _ -> Piqirun.error_variant x code

and parse_piq_node x =
  let x = Piqirun.parse_record x in
  let _piq, x = Piqirun.parse_required_field 1 parse_piq x in
  let _loc, x = Piqirun.parse_optional_field 2 parse_loc x in
  Piqirun.check_unparsed_fields x;
  {
    Piq_node.piq = _piq;
    Piq_node.loc = _loc;
  }

and parse_loc x =
  let x = Piqirun.parse_record x in
  let _file, x = Piqirun.parse_required_field 1 parse_string x in
  let _line, x = Piqirun.parse_required_field 2 parse_int x in
  let _column, x = Piqirun.parse_required_field 3 parse_int x in
  Piqirun.check_unparsed_fields x;
  {
    Loc.file = _file;
    Loc.line = _line;
    Loc.column = _column;
  }

and parse_word x = parse_string x

and parse_name x = parse_word x

and parse_piq_list x =
  Piqirun.parse_list (parse_piq_node) x


and parse_named x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_required_field 1 parse_name x in
  let _value, x = Piqirun.parse_required_field 2 parse_piq_node x in
  Piqirun.check_unparsed_fields x;
  {
    Named.name = _name;
    Named.value = _value;
  }

and parse_splice x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_required_field 1 parse_name x in
  let _item, x = Piqirun.parse_repeated_field 2 parse_piq_node x in
  Piqirun.check_unparsed_fields x;
  {
    Splice.name = _name;
    Splice.item = _item;
  }

and parse_typed x =
  let x = Piqirun.parse_record x in
  let _typename, x = Piqirun.parse_required_field 1 parse_name x in
  let _value, x = Piqirun.parse_required_field 2 parse_piq_node x in
  Piqirun.check_unparsed_fields x;
  {
    Typed.typename = _typename;
    Typed.value = _value;
  }


let rec gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x

and gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x

and gen__uint64 code x = Piqirun.int64_to_varint code x
and packed_gen__uint64 x = Piqirun.int64_to_packed_varint x

and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__binary code x = Piqirun.string_to_block code x

and gen__string code x = Piqirun.string_to_block code x

and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__piq code (x:Piq_piqi.piq) =
  Piqirun.gen_record code [(match x with
    | `int x -> gen__int64 1 x
    | `uint x -> gen__uint64 2 x
    | `float x -> gen__float 3 x
    | `bool x -> gen__bool 4 x
    | `binary x -> gen__binary 5 x
    | `string x -> gen__string 6 x
    | `word x -> gen__word 7 x
    | `text x -> gen__string 8 x
    | `raw_string x -> gen__binary 9 x
    | `name x -> gen__name 10 x
    | `named x -> gen__named 11 x
    | `typename x -> gen__name 12 x
    | `typed x -> gen__typed 13 x
    | `list x -> gen__piq_list 14 x
    | `splice x -> gen__splice 15 x
  )]

and gen__piq_node code x =
  let _piq = Piqirun.gen_required_field 1 gen__piq x.Piq_node.piq in
  let _loc = Piqirun.gen_optional_field 2 gen__loc x.Piq_node.loc in
  Piqirun.gen_record code (_piq :: _loc :: [])

and gen__loc code x =
  let _file = Piqirun.gen_required_field 1 gen__string x.Loc.file in
  let _line = Piqirun.gen_required_field 2 gen__int x.Loc.line in
  let _column = Piqirun.gen_required_field 3 gen__int x.Loc.column in
  Piqirun.gen_record code (_file :: _line :: _column :: [])

and gen__word code x = gen__string code x

and gen__name code x = gen__word code x

and gen__piq_list code x = (Piqirun.gen_list (gen__piq_node)) code x

and gen__named code x =
  let _name = Piqirun.gen_required_field 1 gen__name x.Named.name in
  let _value = Piqirun.gen_required_field 2 gen__piq_node x.Named.value in
  Piqirun.gen_record code (_name :: _value :: [])

and gen__splice code x =
  let _name = Piqirun.gen_required_field 1 gen__name x.Splice.name in
  let _item = Piqirun.gen_repeated_field 2 gen__piq_node x.Splice.item in
  Piqirun.gen_record code (_name :: _item :: [])

and gen__typed code x =
  let _typename = Piqirun.gen_required_field 1 gen__name x.Typed.typename in
  let _value = Piqirun.gen_required_field 2 gen__piq_node x.Typed.value in
  Piqirun.gen_record code (_typename :: _value :: [])


let gen_float64 x = gen__float64 (-1) x
let gen_int64 x = gen__int64 (-1) x
let gen_uint64 x = gen__uint64 (-1) x
let gen_float x = gen__float (-1) x
let gen_bool x = gen__bool (-1) x
let gen_binary x = gen__binary (-1) x
let gen_string x = gen__string (-1) x
let gen_int x = gen__int (-1) x
let gen_piq x = gen__piq (-1) x
let gen_piq_node x = gen__piq_node (-1) x
let gen_loc x = gen__loc (-1) x
let gen_word x = gen__word (-1) x
let gen_name x = gen__name (-1) x
let gen_piq_list x = gen__piq_list (-1) x
let gen_named x = gen__named (-1) x
let gen_splice x = gen__splice (-1) x
let gen_typed x = gen__typed (-1) x


let rec default_float64 () = 0.0
and default_int64 () = 0L
and default_uint64 () = 0L
and default_float () = default_float64 ()
and default_bool () = false
and default_binary () = ""
and default_string () = ""
and default_int () = 0
and default_piq () = `int (default_int64 ())
and default_piq_node () =
  {
    Piq_node.piq = default_piq ();
    Piq_node.loc = None;
  }
and default_loc () =
  {
    Loc.file = default_string ();
    Loc.line = default_int ();
    Loc.column = default_int ();
  }
and default_word () = default_string ()
and default_name () = default_word ()
and default_piq_list () = []
and default_named () =
  {
    Named.name = default_name ();
    Named.value = default_piq_node ();
  }
and default_splice () =
  {
    Splice.name = default_name ();
    Splice.item = [];
  }
and default_typed () =
  {
    Typed.typename = default_name ();
    Typed.value = default_piq_node ();
  }


include Piq_piqi
