OCAMLMAKEFILE = $(PIQI_ROOT)/make/OCamlMakefile


RESULT = piqi


INCDIRS = +camlp4
PRE_TARGETS = \
	META \
	pa_labelscope.cmo pa_labelscope.cmi \
	pa_openin.cmo pa_openin.cmi


LIBINSTALL_FILES = $(PRE_TARGETS)


all: $(PRE_TARGETS)


install: libinstall
uninstall: libuninstall


META: META.templ ../VERSION
	echo "version = \"`cat ../VERSION`\"" > $@; cat $< >> $@


include $(OCAMLMAKEFILE)
