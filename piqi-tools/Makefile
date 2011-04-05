include $(PIQI_ROOT)/make/Makefile.ocaml
include $(PIQI_ROOT)/piqilib/Makefile.piqilib


RESULT = piqi

SOURCES += \
	piqi_json_pp.ml \
	\
	piqi_pp_cmd.ml \
	piqi_convert.ml \
	piqi_check.ml \
	piqi_expand.ml \
	piqi_light_cmd.ml \
	piqi_getopt_cmd.ml \
	\
	$(PIQI_ML_SOURCES) \
	\
	piqi_to_proto.ml \
	piqi_of_proto.ml \
	\
	piqi_server.ml \
	\
	piqi_http.ml \
	piqi_call.ml \
	\
	piqi_run.ml \


PIQI_MODULES = descriptor.piqi piqi-tools.piqi piqi-rpc.piqi

PIQI_ML_SOURCES = descriptor_piqi.ml piqi_tools_piqi.ml piqi_rpc_piqi.ml


PRE_TARGETS += \
	$(PIQI_SOURCES) $(PIQI_PIQI) $(PIQI_ML_SOURCES)
	

PACKS += unix


PIQIC = ../piqic/piqic
# TODO: we don't need to embed piqi in descriptor.ml
PIQIC_FLAGS = -I $(PIQI_ROOT) --embed-piqi


#OCAMLBLDFLAGS = -custom


all: nc


debug: dc top


$(PIQI_ML_SOURCES): $(PIQI_MODULES)
	for i in $^; \
	do \
		$(PIQIC) ocaml $(PIQIC_FLAGS) --pp $$i; \
	done


clean::


include $(OCAMLMAKEFILE)
