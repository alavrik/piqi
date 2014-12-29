OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = bytes

SOURCES = bytes.ml

LIBINSTALL_FILES = bytes.cmi bytes.cma bytes.cmxa bytes.a


all: bcl ncl


install: libinstall


uninstall: libuninstall


download:
	$(MAKE) -f Makefile.download


clean::
	$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
