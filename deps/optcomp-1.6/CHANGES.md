1.6 (2014-05-13)
----------------

* fix for ocaml 4.02

1.5 (2013-11-26)
----------------

* build system tweaks

1.4 (2012-09-23)
----------------

* fix `META` generation

1.3 (2012-07-19)
----------------

* allow quotations in standalone mode
* add value conversion functions
* fix locations in standalone mode
* fix compatibility with latest camlp4
* use oasis 0.3

1.2 (2009-02-25)
----------------

* add a standalone version of optcomp

1.1 (2009-01-27)
----------------

* fix: else blocks didn't get dropped correctly
* fix: parsing stopped after an `#include`
* syntax modifications (more caml-like):
    * allow indentation
    * `#define x value` -> `#let x = value`
    * `#default x value` -> `#let_default x = value`
* new `#warning` directive
* adding of the `<:optcomp< expr >>` quotation
* generation of dependencies for compilation with a makefile

1.0 (2008-12-13)
----------------

* first release
