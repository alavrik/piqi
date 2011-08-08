(* 
 * runtime support for OCaml custom types defined in example.piqi
 *)

type char = Char.t

let char_of_int: int -> char = Char.chr
let char_to_int: char -> int = Char.code


type nativeint = Nativeint.t

let nativeint_of_int: int -> nativeint = Nativeint.of_int
let nativeint_to_int: nativeint -> int = Nativeint.to_int


type bigint = Big_int.big_int

let bigint_of_string: string -> bigint = Big_int.big_int_of_string
let bigint_to_string: bigint -> string = Big_int.string_of_big_int

