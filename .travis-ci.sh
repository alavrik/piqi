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

        # see https://github.com/Homebrew/homebrew-core/issues/26358
        brew upgrade python

        # build dependencies (Mac OS X)
        brew install opam

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

        # so that tests pass with the latest protobuf version
        export CXXFLAGS='-std=c++14'

    else
        # install specific ocaml version
        wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-ocaml.sh

        . .travis-ocaml.sh
    fi

    opam install --deps-only ./opam

    ./configure

    make

    make test

    make doc

    # checking for broken doc links -- the test is flaky, disabling for now
    #make -C doc test
fi
