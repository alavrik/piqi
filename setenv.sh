
# "prefix" path for installing "piqi" and "piqic" executables
export PIQI_PREFIX=/usr/local


# "prefix" path for installing Piqi OCaml libraries; if not set, the OCaml
# installation's library path (i.e. `ocamlc -where`) will be used
export PIQI_OCAML_PREFIX=


#
# don't change the settings below -- they are necessary for the build process
#

export PIQI_ROOT="`pwd`"

# directory for temporary files required for the build
export PIQI_BUILD="$PIQI_ROOT/build"

# path to piqi and piqic (required for tests)
export PATH="$PIQI_ROOT/bin:$PATH"

# path to .piqi files (required for tests)
export PIQI_DIR="$PIQI_ROOT"

