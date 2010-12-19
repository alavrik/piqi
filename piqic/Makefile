include $(PIQI_ROOT)/make/Makefile.ocaml
include $(PIQI_ROOT)/piqi/Makefile.piqi_common


RESULT = piqic


SOURCES += \
	piqic_common.ml \
	\
	piqic_ocaml_types.ml \
	piqic_ocaml_out.ml \
	piqic_ocaml_in.ml \
	piqic_ocaml.ml \
	\
	piqic_erlang_types.ml \
	piqic_erlang_out.ml \
	piqic_erlang_in.ml \
	piqic_erlang.ml \
	\
	piqic_run.ml \


PRE_TARGETS += $(PIQI_SOURCES) 


PIQICC = ../piqicc/piqicc
PIQIC_FLAGS = -I $(PIQI_ROOT)
PIQICC_FLAGS = $(PIQIC_FLAGS)


all: debug #nc

debug: dc top


piqtype.ml: $(wildcard *.piqi)
	$(PIQICC) $(PIQICC_FLAGS) -o $@.tmp.ml \
		--boot piqic-boot.piqi --piqi piqic.piqi --impl piqic-impl.piqi
	$(CAMLP4)o -o $@ $@.tmp.ml


clean::
	rm -f *.tmp.ml


include $(OCAMLMAKEFILE)
