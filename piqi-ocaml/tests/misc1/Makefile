OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = t
SOURCES = id_piqi.ml id_piqi_ext.ml example_piqi.ml example_piqi_ext.ml test.ml


PRE_TARGETS = id_piqi.ml id_piqi_ext.ml example_piqi.ml example_piqi_ext.ml


export OCAMLPATH := ../..:$(OCAMLPATH)
PACKS = piqirun.ext


PIQI ?= piqi
PIQIC = ../../piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = --ext


all: native-code test #byte-code


test:
	./$(RESULT)


%_piqi.ml: %.proto $(realpath $(PIQI)) $(PIQIC)
ifneq ($(shell which protoc), )
	$(PIQI) of-proto $<
endif
	$(PIQIC) $(PIQIC_FLAGS) $<.piqi


clean::
	rm -f *.tmp.ml


include $(OCAMLMAKEFILE)
