include $(PIQI_ROOT)/make/Makefile.dirs


DIRS = \
	camlp4 \
	piqirun-ocaml \
	piqicc piqic piqi \


install:
	-install -d $(PIQI_PREFIX)/bin
	install piqi/piqi $(PIQI_PREFIX)/bin
	install piqic/piqic $(PIQI_PREFIX)/bin


libinstall:
	$(MAKE) -C piqirun-ocaml libinstall
	$(MAKE) -C camlp4 libinstall


libuninstall:
	$(MAKE) -C piqirun-ocaml libuninstall
	$(MAKE) -C camlp4 libuninstall


erlang:
	$(MAKE) -C piqirun-erlang


erlang-clean:
	$(MAKE) -C piqirun-erlang clean


clean:: erlang-clean
	$(MAKE) -C deps clean
	$(MAKE) -C tests clean

