include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs


# export installation and search path for third-party OCaml dependencies
OCAMLFIND_DESTDIR := $(PIQI_BUILD)/lib/ocaml
ifeq ($(SYSTEM),$(filter $(SYSTEM),mingw mingw64))
OCAMLFIND_DESTDIR := $(shell cygpath -w $(OCAMLFIND_DESTDIR))
endif
export OCAMLFIND_DESTDIR

OCAMLPATH := $(PIQI_ROOT)/deps
ifeq ($(SYSTEM),$(filter $(SYSTEM),mingw mingw64))
OCAMLPATH := $(shell cygpath -w $(OCAMLPATH))
endif
export OCAMLPATH


ifndef SKIP-ulex
DIRS += ulex
endif

ifndef SKIP-xmlm
DIRS += xmlm
endif

ifndef SKIP-easy-format
DIRS += easy-format
endif

ifndef SKIP-base64
DIRS += base64
endif


install: dirs

uninstall: dirs

download: dirs
