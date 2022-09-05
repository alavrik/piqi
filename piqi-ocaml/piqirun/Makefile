OCAMLMAKEFILE := ../make/OCamlMakefile


RESULT = piqirun


# piqirun_ext.ml depends on it
PACKS = piqilib stdlib-shims


SOURCES = \
	piqirun.ml \
	piqirun_ext.ml piqirun_ext.mli

LIBINSTALL_FILES = \
	piqirun.cmi piqirun.cmo piqirun.cmx piqirun.o \
	piqirun_ext.mli piqirun_ext.cmi piqirun_ext.cmo piqirun_ext.cmx piqirun_ext.o \
	piqirun.ml # TODO: piqirun.mli


PRE_TARGETS = META


all: bcl ncl


debug: dcl top


# NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall libinstall


uninstall: libuninstall


META: ../VERSION META.in
	echo "version = \"`head -1 $<`\"" >$@
	cat META.in >>$@


test: bcl
	ocaml piqirun.cmo test.ml


# these commands are useful for debugging:
#ocamlc -c -g piqirun.cmo test.ml 
#ocaml -init test.ocaml piqirun.cmo test.cmo 
#
#ocamlc -g piqirun.cmo test.ml 
#OCAMLRUNPARAM=b ./a.out


clean::
	rm -f test.cm? a.out


include $(OCAMLMAKEFILE)
