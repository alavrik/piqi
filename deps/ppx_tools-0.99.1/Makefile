OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = ppx_tools

SOURCES = ast_convenience.ml ast_mapper_class.ml

OCAMLFIND_PACKAGE = ppx_tools

unexport PACKS


MODS = ast_mapper_class ast_convenience
LIBINSTALL_FILES = \
	$(wildcard $(MODS:=.mli) $(MODS:=.cmi) $(MODS:=.cmx) \
	  ppx_metaquot$(EXE) \
	  ppx_tools.cma ppx_tools.cmxa ppx_tools.$(EXT_LIB) \
         )


OCAMLFLAGS = -w +A-4-17-44-45 -I +compiler-libs -safe-string


all: bcl ncl ppx


install:
	$(MAKE) libinstall


uninstall:
	$(MAKE) libuninstall


ppx:
	$(MAKE) -f Makefile.ppx


clean::
	$(MAKE) -f Makefile.ppx clean


include $(OCAMLMAKEFILE)
