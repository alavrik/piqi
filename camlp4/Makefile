OCAMLMAKEFILE = $(PIQI_ROOT)/make/OCamlMakefile


RESULT = piqi


INCDIRS = +camlp4
PRE_TARGETS = \
	META \
	pa_labelscope.ml pa_openin.ml \
	pa_labelscope.cmo pa_labelscope.cmi \
	pa_openin.cmo pa_openin.cmi


LIBINSTALL_FILES = $(PRE_TARGETS)


OCAML_VERSION = $(shell ocamlfind ocamlc -version | sed -e 's/^\([0-9]*\.[0-9]*\)\..*/\1/')


all: supported_ocaml_version $(PRE_TARGETS)


supported_ocaml_version: $(OCAML_VERSION)


install: libinstall
uninstall: libuninstall


META: META.templ ../VERSION
	@echo "Building CamlP4 extensions for OCaml $(OCAML_VERSION)"
	echo "version = \"`cat ../VERSION`\"" > $@; cat $< >> $@


%.ml: $(OCAML_VERSION)/%.ml
	ln -s $< $@


include $(OCAMLMAKEFILE)
