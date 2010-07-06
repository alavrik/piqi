#
# don't change the ones below:
#

export PIQI_ROOT=`pwd`

# path to piqi and piqic (required for tests)
export PATH=$PIQI_ROOT/bin:$PATH

# path to .piqi files (required for tests)
export PIQI_DIR=$PIQI_ROOT


#
# these are configurable:
#

# "prefix" path for installing "piqi" and "piqic" executables
PIQI_PREFIX=$PIQI_ROOT/build
export PIQI_PREFIX

# installation and search path for OCaml libraries
OCAMLFIND_DESTDIR=$PIQI_PREFIX/lib/ocaml
OCAMLPATH=$OCAMLFIND_DESTDIR
export OCAMLFIND_DESTDIR OCAMLPATH

