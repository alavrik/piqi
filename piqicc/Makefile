include Makefile.piqicc_common


RESULT = piqicc


SOURCES += piqicc_run.ml


PIQIC_FLAGS = -I $(PIQI_ROOT)
PIQICC_FLAGS = $(PIQIC_FLAGS) --boot piqicc-boot.piqi



.PHONY: boot boot2 reboot
boot: piqi_boot
boot2: piqi_boot2
reboot: piqi_reboot



piqtype.ml: piqi_reboot piqicc.piqi piqicc-boot.piqi
	./piqi_reboot $(PIQICC_FLAGS) -o piqtype.tmp.ml piqicc.piqi
	#./piqi_reboot >piqtype.tmp.ml
	rm -f *.cmo *.cmi
	$(CAMLP4)o -o piqtype.3.ml piqtype.tmp.ml
	ln -sf piqtype.3.ml $@


piqi_reboot: piqi_boot2
	rm -f piqtype.ml
	$(MAKE) -f Makefile.reboot

piqi_boot2: piqi_boot
	rm -f piqtype.ml
	$(MAKE) -f Makefile.boot2

piqi_boot: boot/piqtype.ml.m4 boot/piqdefs.ml boot/piqast.ml.m4
	rm -f piqtype.ml
	$(MAKE) -f Makefile.boot


clean::
	$(MAKE) -f Makefile.reboot clean
	$(MAKE) -f Makefile.boot2 clean
	$(MAKE) -f Makefile.boot clean
	rm -f piqtype.tmp.ml piqtype.3.ml


include $(OCAMLMAKEFILE)

