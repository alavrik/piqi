include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqirun


SOURCES = piqirun.ml


LIBINSTALL_FILES = \
        $(RESULT).cma $(RESULT).cmxa \
	piqirun.cmi


# LIBS =
# LIBDIRS = 
# INCDIRS =
# RESULTDEPS =

# PACKS = 
# ANNOTATE = yes
# INCDIRS = +camlp4
# USE_CAMLP4 = yes

PRE_TARGETS = META


all: dcl ncl test #top


install: libinstall
uninstall: libuninstall


META: META.templ ../VERSION
	echo "version = \"`cat ../VERSION`\"" > $@; cat $< >> $@


test:
	ocaml piqirun.cma test.ml


# these commands are useful for debugging:
#ocamlc -c -g piqirun.cma test.ml 
#ocaml -init test.ocaml piqirun.cma test.cmo 
#
#ocamlc -g piqirun.cma test.ml 
#OCAMLRUNPARAM=b ./a.out


clean::
	rm -f test.cm? a.out


include $(OCAMLMAKEFILE)
