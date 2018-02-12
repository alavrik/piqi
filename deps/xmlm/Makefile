OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = xmlm

SOURCES = xmlm.ml xmlm.mli

LIBINSTALL_FILES = xmlm.mli xmlm.cmi xmlm.cmx xmlm.cma xmlm.cmxa xmlm.a


all: bcl ncl


install: libinstall


uninstall: libuninstall


download:
	$(MAKE) -f Makefile.download


clean::
	$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
