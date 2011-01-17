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
	piqi_to_proto.ml \
	\
	descriptor.ml \
	piqi_of_proto.ml \
	\
	piqi_run.ml \



PIQI_MODULES = descriptor.piqi
PIQI_ML_SOURCES = $(PIQI_MODULES:%.piqi=%.ml)


PRE_TARGETS += $(PIQI_SOURCES) $(PIQI_PIQI) $(PIQI_ML_SOURCES)


PIQIC = ../piqic/piqic
PIQIC_FLAGS = -I $(PIQI_ROOT)


#OCAMLBLDFLAGS = -custom


all: nc


debug: dc top


$(PIQI_ML_SOURCES): %.ml: %.piqi
	$(PIQIC) ocaml $(PIQIC_FLAGS) --pp -o $@ $<


clean::


include $(OCAMLMAKEFILE)
