include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = a.out


SOURCES = \
	piqirun_custom.ml \
	$(PIQI_ML_FILES) \


PACKS = piqi.lib num


PIQI_FILES = example.piqi
PIQI_ML_FILES = example_piqi.ml


PRE_TARGETS = $(PIQI_ML_FILES)


PIQIC = piqic
PIQIC_FLAGS = --pp --gen-defaults #--leave-tmp-files


all: native-code #byte-code debug-code


$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) ocaml $(PIQIC_FLAGS) $$i ; \
	done


clean::
	rm -f *.tmp.ml


include $(OCAMLMAKEFILE)
