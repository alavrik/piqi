optcomp - optional compilation with cpp-like directives
=======================================================

Optcomp is a syntax extension which handles `#if`, `#else`,
... directives in ocaml source files.

For example, to switch between two pieces of code according to the
ocaml compiler version, one can write:


    #if ocaml_version < (3, 10)
    let x = 1
    #else
    let x = 2
    #endif

### What the difference between cpp and optcomp ?

Optcomp is more OCaml-friendly than cpp:

* it does not interpret `//`, `/*`, and `*/` as comment delimiters
* it does not complains about missing `'`
* it is easier to integrate in the build process when using other
  camlp4 syntax extensions

By the way optcomp does not do macro expansion while cpp does.

### What the difference between pa_macro and optcomp ?

Optcomp does not require code that will be dropped to be valid caml
code. So for example this code will be rejected by
camlp4(<3.13)+pa_macro:

    IFDEF HAVE_GADTS THEN
    type 'a t =
      | Int : int t
      | String : string t
    ENDIF

But this one will be accepted by camlp4+optcomp:

    #if HAVE_GADTS
    type 'a t =
      | Int : int t
      | String : string t
    #endif

Installation
------------

To build and install optcomp:

    $ ./configure
    $ make
    $ make install

### Documentation _(optional)_

To build the documentation:

    $ make doc

It will then be installed by `make install`.

### Tests _(optionnal)_

To build and execute tests:

    $ ./configure --enable-tests
    $ make test

Usage
-----

### As a syntax extension

You can use optcomp as a regular syntax extension with camlp4. If you
have ocamlfind installed, you can use:

    $ ocamlfind ocamlc -syntax camlp4o -pakcage camlp4,optcomp file.ml

You can also embed `pa_optcomp.ml` in your project sources.

#### As a preprocessor

Optcomp can be used as a preprocessor, for that there is the two
executable optcomp-o and optcomp-r:

* `optcomp-o` is for when directives are written using original syntax
* `optcomp-r` is for when directives are written using revised syntax

To use them:

    $ ocamlc -pp optcomp-o <file.ml>
    $ ocamlc -pp optcomp-r <file.ml>

The preprocessor version is especially usefull for .mli because it
does not modify the layout of your code, which is important for
ocamldoc.

Hacking
-------

To add support to more expressions, you can modify the eval function
of pa_optcomp.ml. It takes a camlp4 expression ast and must return
something of type value.
