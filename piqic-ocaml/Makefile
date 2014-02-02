include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqic-ocaml


SOURCES = \
	piqic_piqi.ml \
	piqic_common.ml \
	\
	piqic_ocaml_types.ml \
	piqic_ocaml_out.ml \
	piqic_ocaml_in.ml \
	piqic_ocaml_defaults.ml \
	piqic_ocaml_ext.ml \
	\
	piqic_ocaml_version.ml \
	piqic_ocaml.ml \


PACKS = piqilib piqirun.pb


PRE_TARGETS = piqic_ocaml_version.ml


all: nc

debug: dc top


clean::
	rm -f piqi.piqi


# recompile piqi self-spec into piqic_piqi.ml
PIQI ?= ../piqi-tools/piqi
PIQIC = ./$(RESULT)
PIQIC_FLAGS = -e piqi-ocaml --embed-piqi --pp #--trace

piqi:
	$(PIQI) cc -o piqi.piqi
	$(PIQIC) $(PIQIC_FLAGS) piqi.piqi


piqic_ocaml_version.ml: ../VERSION
	echo "let version = \"`head -1 $<`\"" >$@


include $(OCAMLMAKEFILE)
