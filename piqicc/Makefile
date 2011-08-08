include Makefile.piqicc_common


RESULT = piqicc


SOURCES += piqicc.ml piqicc_run.ml


PIQIC_FLAGS = -I $(PIQI_ROOT)
PIQICC_FLAGS = $(PIQIC_FLAGS)



.PHONY: boot boot2 reboot
boot: piqi_boot
boot2: piqi_boot2
reboot: piqi_reboot



piqi_piqi.ml: piqi_reboot $(wildcard *.piqi)
	./piqi_reboot $(PIQICC_FLAGS) -o piqi_piqi.tmp.ml \
		--boot piqicc-boot.piqi --piqi piqicc.piqi --impl piqicc-impl.piqi
	rm -f *.cmo *.cmi
	$(CAMLP4_O) -o piqi_piqi.3.ml piqi_piqi.tmp.ml
	ln -sf piqi_piqi.3.ml $@


piqi_reboot: piqi_boot2
	rm -f piqi_piqi.ml
	$(MAKE) -f Makefile.reboot

piqi_boot2: piqi_boot
	rm -f piqi_piqi.ml
	$(MAKE) -f Makefile.boot2

piqi_boot: boot/piqi_piqi.ml.m4 boot/piqdefs.ml boot/piqast.ml.m4
	rm -f piqi_piqi.ml
	$(MAKE) -f Makefile.boot


clean::
	$(MAKE) -f Makefile.reboot clean
	$(MAKE) -f Makefile.boot2 clean
	$(MAKE) -f Makefile.boot clean
	rm -f piqi_piqi.tmp.ml piqi_piqi.3.ml


include $(OCAMLMAKEFILE)

