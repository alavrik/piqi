#!/bin/bash

set -ex


case $TRAVIS_OS_NAME in
    osx)
        # build dependencies
        brew install opam

        # optional dependencies for running tests
        brew install protobuf

        echo OCaml version
        ocaml -version

        echo OPAM versions
        opam --version
        opam --git-version

        export OPAMYES=1
        rm -rf ~/.opam

        opam init
        eval `opam config env`
        opam install ocamlfind camlp4
        ;;

    *)  # linux

        if [ "$OCAML_VERSION" = "system" ]
        then
            # build dependencies
            sudo apt-get install ocaml-nox camlp4-extra ocaml-findlib

            # optional dependencies for running tests and building docs
            #
            # NOTE: these need to be tested only once, no need to re-run these
            # tests for all OCaml versions
            sudo apt-get install libprotoc-dev protobuf-compiler pandoc

            echo "system OCaml version:"
            ocaml -version

        elif [ -n "${PACKAGE-}" ]
        then
            # build using opam, run tests, build package, install package
            wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-opam.sh

            . .travis-opam.sh

        else
            # install specific ocaml version
            wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-ocaml.sh

            . .travis-ocaml.sh

            # install build dependencies using opam
            opam install ocamlfind camlp4
        fi
        ;;
esac


./configure
make deps
make

make test

make doc

# checking for broken doc links -- the test is flaky, disabling for now
#make -C doc test

