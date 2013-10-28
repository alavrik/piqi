[![Build Status](https://travis-ci.org/alavrik/piqi.png)](https://travis-ci.org/alavrik/piqi)


Piqi is a set of languages and open-source tools for working with structured
data. It includes:

- A cross-language data serialization system compatible with [Google Protocol
  Buffers](http://code.google.com/p/protobuf/). It allows programs implemented
  in various languages to exchange and persist data in a portable manner.

- Piq -- a human-friendly typed data representation language. It is designed to
  be more convenient for representing, viewing and editing data than JSON, XML,
  CSV, S-expressions and other formats.

- Piqi -- a universal data definition language. It can be used as a schema
  language for JSON, XML, Protocol Buffers and Piq.

- Tools for validating, pretty-printing and converting data between Piq, JSON,
  XML and Protocol Buffers formats.

- Piqi-RPC -- an RPC-over-HTTP system for Erlang. It provides a simple way to
  expose Erlang services via JSON, XML and Google Protocol Buffers over HTTP.

As a data serialization system, Piqi implements native support for
[OCaml](http://piqi.org/doc/ocaml/) and [Erlang](http://piqi.org/doc/erlang/).
Connectivity with other programming languages is provided via Google Protocol
Buffers. Overall, Piqi provides a more natural mapping to functional programming
languages compared to various serialization systems that were originally
designed for imperative or object-oriented languages.

Piqi was inspired by Google Protocol Buffers and specially designed to be
largely compatible with it. Like Protocol Buffers, Piqi relies on type
definitions and supports data schema evolution. The main difference is that Piqi
has a richer data model, high-level modules and a powerful data representation
language (Piq).

Combination of the data representation (Piq) and the data definition (Piqi)
languages is similar to the concept of "valid XML" (i.e. XML conforming to some
XML Schema). However, unlike XML, Piq has a concise, clean syntax and a data
model similar to those of high-level programming languages.

Full project description and documentation can be found at
[http://piqi.org](http://piqi.org)


Installation
------------

See [INSTALL](INSTALL) for the installation instructions.


Bug tracker
-----------

If you found a bug or have any suggestions please submit a GitHub issue:
[https://github.com/alavrik/piqi/issues](https://github.com/alavrik/piqi/issues)


Contributing
------------

Participation and patches are very welcome! The best way to submit a
contribution is to open a pull request on GitHub against the `master` branch.


Mailing list
------------

For discussions about the usage, development, and future of Piqi there is the
Piqi Google Group:
[http://groups.google.com/group/piqi](http://groups.google.com/group/piqi)


License
-------

[Apache License Version 2.0](LICENSE)


Files
-----

    README                this file
    INSTALL               general installation instructions
    INSTALL.erlang        instructions for building Piqi for Erlang
    INSTALL.ocaml         instructions for building Piqi for OCaml
    INSTALL.windows       instructions for building Piqi on Windows
    setenv.sh             environment configuration for running tests
    setenv-mingw.sh       environment configuration for Windows/MinGW builds
    LICENSE               license
    NOTICE                copyright notice
    CHANGES               release history
    VERSION               current version
    configure             configure script
    Makefile              top-level Makefile
    make/                 makefiles and build scripts
    deps/                 third-party components (see NOTICE for details)
    piqi-camlp4/          Piqi syntax extensions for OCaml
    piqicc/               Piqi compiler compiler (used only during boostrap)
    piqic/                Piq interface compiler(s) for OCaml
    piqi-tools/           "piqi" command-line utility (piqi tools)
    piqi                  Piqi self-specification
    piqilib/              Piqi common library
    piqirun-ocaml/        Piqi runtime library for OCaml
    examples/             examples
    tests/                tests for various piqi functionality; see tests/README
    doc/                  Piqi documentation
    editors/              text editor plugins for .piq and .piqi files

