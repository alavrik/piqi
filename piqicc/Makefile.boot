include Makefile.piqicc_common


RESULT = piqi_boot

SOURCES += boot.ml


piqi_piqi.ml: boot/piqi_piqi.ml.m4 boot/piqdefs.ml boot/piqast.ml.m4
	echo '(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)' >$@
	m4 boot/piqast.ml.m4 >>$@
	m4 boot/piqi_piqi.ml.m4 >>$@
	cat boot/piqdefs.ml >>$@



include $(OCAMLMAKEFILE)
