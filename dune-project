(lang dune 2.8)
(name piqi)

; piqi schema and muili-format conversion library
(package (name piqilib))

; piqi executable
(package (name piqi) (depends piqilib))

; piqi runtime for OCaml
(package (name piqirun))

; piqic-ocaml executable
(package (name piqic_ocaml) (depends piqilib))
