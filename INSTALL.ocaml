This file contains instructions for building Piqi to be used as a data
serialization system for OCaml.


INSTALLING USING OPAM
=====================

In order to install Piqi using [OPAM](http://opam.ocamlpro.com/), run the
following command:

        opam install piqi

This command will install the latest stable version of Piqi that includes `piqi`
and `piqic-ocaml` executables and runtime libraries for OCaml.

To install the latest development version of Piqi, follow the instructions at

        https://github.com/piqi/piqi-opam-repo


INSTALLING FROM SOURCE CODE
===========================

1. Follow general build and installation instructions from the INSTALL file.

        If you want Piqi OCaml libraries to be installed to a custom filesystem
        location, run ./configure with --ocaml-libdir=<DIR>

        This will override ocamlfind setting determined by OCAMLFIND_DESTDIR
        environment variable or by ocamlfind config. You can run
        "ocamlfind printconf destdir" to see the current setting.


2. Build Piqi runtime libraries for OCaml:

        make ocaml


3. Install Piqi runtime libraries and `piqic-ocaml`:

        make ocaml-install


To uninstall:

        make ocaml-uninstall

