include Makefile.piqicc_common


RESULT = piqi_boot

SOURCES += boot.ml


piqtype.ml: boot/piqtype.ml.m4 boot/piqdefs.ml boot/piqast.ml.m4
	echo "(*pp camlp4o -I $$PIQI_ROOT/camlp4 pa_labelscope.cmo pa_openin.cmo *)" >$@
	m4 boot/piqast.ml.m4 >>$@
	m4 boot/piqtype.ml.m4 >>$@
	cat boot/piqdefs.ml >>$@



include $(OCAMLMAKEFILE)
