include $(PIQI_ROOT)/make/Makefile.ocaml
include Makefile.piqi_common


RESULT = piqi

SOURCES += \
	piqi_ext.ml \
	piqi_func.ml \
	\
	piqi_json_common.ml \
	piqi_json_parser.mll \
	piqi_json_gen.ml \
	piqi_json.ml \
	\
	base64.ml \
	piqobj_to_json.ml \
	piqobj_of_json.ml \
	\
	piqi_pp.ml \
	piq.ml \
	\
	piqi_convert.ml \
	piqi_check.ml \
	piqi_expand.ml \
	piqi_light.ml \
	\
	piqi_to_proto.ml \
	\
	descriptor.ml \
	piqi_of_proto.ml \
	\
	piqi_run.ml \



PIQI_MODULES = descriptor.piqi
PIQI_ML_SOURCES = $(PIQI_MODULES:%.piqi=%.ml)


PRE_TARGETS += $(PIQI_ML_SOURCES)


PIQIC = ../piqic/piqic
PIQIC_FLAGS = -I $(PIQI_ROOT)

PIQICC = ../piqicc/piqicc
PIQICC_FLAGS = $(PIQIC_FLAGS)


#OCAMLBLDFLAGS = -custom


all: nc


debug: dc top


piqtype.ml: piqi-impl.piqi
	# Note, using standard piqi.org/piqi-boot as boot module for piqicc
	$(PIQICC) $(PIQICC_FLAGS) -o $@.tmp.ml \
		--boot piqi-boot.piqi --piqi piqi.piqi --impl piqi-impl.piqi
	$(CAMLP4)o -o $@ $@.tmp.ml


$(PIQI_ML_SOURCES): %.ml: %.piqi
	$(PIQIC) ocaml $(PIQIC_FLAGS) -o $@.tmp.ml $<
	$(CAMLP4)o -o $@ $@.tmp.ml


clean::
	rm -f *.tmp.ml


include $(OCAMLMAKEFILE)
