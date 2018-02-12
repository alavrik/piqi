OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = base64

SOURCES = b64.ml

PACKS = bytes

LIBINSTALL_FILES = b64.mli b64.cmi b64.cmx base64.cma base64.cmxa base64.a


all: bcl ncl


install: libinstall


uninstall: libuninstall


download:
	$(MAKE) -f Makefile.download


clean::
	$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
