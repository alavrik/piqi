include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqirun


PACKS = piqi.lib  # piqirun_ext.ml depends on it


SOURCES = \
	piqirun.ml \
	piqirun_ext.ml piqirun_ext.mli


LIBINSTALL_FILES = \
	piqirun.cmi piqirun.cmo piqirun.cmx piqirun.o \
	piqirun_ext.mli piqirun_ext.cmi piqirun_ext.cmo piqirun_ext.cmx piqirun_ext.o \
	# TODO: piqirun.mli


STATIC = 1  # build a static library (don't build dlls)

# LIBS =
# LIBDIRS = 
# INCDIRS =
# RESULTDEPS =

# PACKS = 
# ANNOTATE = yes
# INCDIRS = +camlp4
# USE_CAMLP4 = yes


all: dcl ncl test #top


post_target:: install


# NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall META libinstall


uninstall: libuninstall


META: ../VERSION META.tmpl
	echo "version = \"`head -1 $<`\"" >$@
	cat META.tmpl >>$@


test:
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
