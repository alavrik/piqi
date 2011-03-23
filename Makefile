include $(PIQI_ROOT)/make/Makefile.dirs


DIRS = \
	camlp4 \
	piqirun-ocaml \
	piqicc piqic piqi-tools \


.PHONY: deps install ocaml-install ocaml-uninstall erlang erlang-clean


deps:
	$(MAKE) -C deps


install:
	-install -d $(PIQI_PREFIX)/bin
	install piqi-tools/piqi $(PIQI_PREFIX)/bin
	install piqic/piqic $(PIQI_PREFIX)/bin


ocaml-install:
	$(MAKE) -C piqirun-ocaml libinstall
	$(MAKE) -C camlp4 libinstall


ocaml-uninstall:
	$(MAKE) -C piqirun-ocaml libuninstall
	$(MAKE) -C camlp4 libuninstall


erlang:
	$(MAKE) -C piqi-erlang


erlang-clean:
	$(MAKE) -C piqi-erlang clean


clean:: erlang-clean
	$(MAKE) -C deps clean
	$(MAKE) -C tests clean

