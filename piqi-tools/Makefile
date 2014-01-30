include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqi


PACKS = piqilib.lib unix

SOURCES = \
	$(PIQI_ML_FILES) \
	piqi_main.ml \
	\
	piqi_json_pp.ml \
	piqi_pp_cmd.ml \
	piqi_convert_cmd.ml \
	piqi_check.ml \
	piqi_expand.ml \
	piqi_light_cmd.ml \
	piqi_getopt_cmd.ml \
	piqi_compile_cmd.ml \
	piqi_cc.ml \
	\
	piqi_to_proto.ml \
	piqi_of_proto.ml \
	\
	piqi_http.ml \
	piqi_rpc.ml \
	piqi_server.ml \
	piqi_call.ml \
	\
	piqi_run.ml


# make sure piqi gets re-linked when the piqilib library changes
RESULTDEPS := $(wildcard ../piqilib/piqi.cm*a)

# allow ocamldep to pick up ../piqilib/*.cm[ox] as dependencies for ./*.cm[ox]
DINCDIRS := ../piqilib

# campl4 flags
PPFLAGS := -I ../piqilib pa_labelscope.cmo pa_openin.cmo


PIQI_FILES = descriptor.proto.piqi piqi_tools.piqi piqi_rpc.piqi
PIQI_ML_FILES = descriptor_piqi.ml piqi_tools_piqi.ml piqi_rpc_piqi.ml


PIQI_OCAML_DIR ?= ..
PIQIC = $(PIQI_OCAML_DIR)/piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = --pp --runtime Piqi_piqirun


all: piqilib nc


debug: piqilib dc top


# NOTE: we have to unset these variables to prevent piqilib's OCamlMakefile from
# inheriting them
piqilib:
	unset RESULTDEPS DINCDIRS PPFLAGS REAL_OCAMLFIND && $(MAKE) -C ../piqilib $(MAKECMDGOALS)


# rebuilding *_piqi.ml from .piqi -- called manually as "make piqi" when there
# were changes in the .piqi files or to take advantage of new versions of
# piqic-ocaml and the piqirun.pb runtime library
piqi:
	$(PIQIC) $(PIQIC_FLAGS) descriptor.proto.piqi
	$(PIQIC) $(PIQIC_FLAGS) piqi_rpc.piqi
	$(PIQIC) $(PIQIC_FLAGS) --embed-piqi piqi_tools.piqi


# re-creating descriptor.proto.piqi from a (newer) descriptor.proto
# -- called as manually "make descriptor" when when there are changes in the
# upstream descriptor.proto; should be followed by "make piqi" to generate
# descriptor_piqi.ml
descriptor: descriptor.proto
	./piqi of-proto --normalize $<
	$(MAKE) piqi


.PHONY: descriptor piqi piqilib


include $(OCAMLMAKEFILE)

ifdef WIN32
# otherwise breaks on Win32 because of slashes
unexport DINCDIRS
endif
