include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqi


PACKS = piqi.lib unix

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


# make sure piqi gets rebuilt when we change the library
RESULTDEPS = ../piqilib/piqi.cmxa $(wildcard ../piqilib/piqi.cma)


PIQI_FILES = descriptor.proto.piqi piqi_tools.piqi piqi_rpc.piqi
PIQI_ML_FILES = descriptor_piqi.ml piqi_tools_piqi.ml piqi_rpc_piqi.ml


PIQIC = ../piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = -I $(PIQI_ROOT) --pp --runtime Piqi_piqirun


all: nc


debug: dc top


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


.PHONY: descriptor piqi


include $(OCAMLMAKEFILE)
