OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = sedlexing

SOURCES = sedlexing.ml

OCAMLFIND_PACKAGE = sedlex

unexport PACKS


MODS = sedlexing sedlex
LIBINSTALL_FILES = \
	$(wildcard $(MODS:=.mli) $(MODS:=.cmi) $(MODS:=.cmx) \
	  ppx_sedlex$(EXE) ppx_sedlex.opt$(EXE) \
          sedlexing.cma sedlexing.cmxa sedlexing.$(EXT_LIB) \
          sedlex.cma sedlex.cmxa sedlex.$(EXT_LIB) \
	 )


OCAMLFLAGS = -w +A-4-9 -safe-string


all: bcl ncl syntax


install:
	$(MAKE) libinstall


uninstall:
	$(MAKE) libuninstall


syntax:
	$(MAKE) -f Makefile.syntax
	$(MAKE) -f Makefile.ppx


clean::
	$(MAKE) -f Makefile.syntax clean
	$(MAKE) -f Makefile.ppx clean


include $(OCAMLMAKEFILE)
