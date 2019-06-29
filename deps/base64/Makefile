OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = base64

SOURCES = base64.ml base64_rfc2045.ml

LIBINSTALL_FILES = base64.mli base64.cmi base64_rfc2045.mli base64_rfc2045.cmi base64.cma base64.cmxa base64.a


all: bcl ncl


install: libinstall


uninstall: libuninstall


download:
	$(MAKE) -f Makefile.download


clean::
	$(MAKE) -f Makefile.download clean


include $(OCAMLMAKEFILE)
