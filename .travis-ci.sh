#!/bin/bash

set -ex


if [ -n "${PACKAGE-}" ]
then
    # build using opam, run tests, build package, install package

    wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-opam.sh

    . .travis-opam.sh

else
    # install ocaml, install basic build dependencies, run standard build

    if [ "$TRAVIS_OS_NAME" = "osx" ]
    then
        brew update

        # build dependencies (Mac OS X)
        brew install ocaml opam

        # optional dependencies for running tests
        brew install protobuf

        echo OCaml version
        ocaml -version

        echo OPAM versions
        opam --version
        opam --git-version

        export OPAMYES=1

        opam init
        eval `opam config env`

        # install basic build dependencies using opam
        opam install ocamlfind camlp4

    elif [ "$OCAML_VERSION" = "system" ]
    then
        # build dependencies (Ubuntu)
        sudo apt-get install ocaml-nox camlp4-extra ocaml-findlib

        # optional dependencies for running tests and building docs
        #
        # NOTE: these need to be tested only once, no need to re-run these
        # tests for all OCaml versions
        sudo apt-get install libprotoc-dev protobuf-compiler pandoc

        echo "system OCaml version:"
        ocaml -version
    else
        # install specific ocaml version
        wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-ocaml.sh

        . .travis-ocaml.sh

        # install basic build dependencies using opam
        opam install ocamlfind camlp4
    fi


    ./configure
    make deps
    make

    make test

    make doc

    # checking for broken doc links -- the test is flaky, disabling for now
    #make -C doc test
fi
