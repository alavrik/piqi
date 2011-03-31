include $(PIQI_ROOT)/make/Makefile.dirs


DIRS = \
	camlp4 \
	piqirun-ocaml \
	piqicc piqic piqi-tools \


.PHONY: deps install ocaml-install ocaml-uninstall erlang erlang-clean distclean


# export installation and search path for OCaml dependencies
ifeq ($(MAKECMDGOALS),deps)
OCAMLFIND_DESTDIR = $(PIQI_BUILD)/lib/ocaml
OCAMLPATH = $(OCAMLFIND_DESTDIR)
export OCAMLFIND_DESTDIR OCAMLPATH
endif


# export installation path for Piqi OCaml libraries
ifneq ($(findstring ocaml-,$(MAKECMDGOALS)),)
ifneq ($(PIQI_OCAML_PREFIX),)
OCAMLFIND_DESTDIR = $(PIQI_OCAML_PREFIX)
export OCAMLFIND_DESTDIR
endif
endif


deps:
	$(MAKE) -C deps
	mkdir -p $(OCAMLFIND_DESTDIR)
	$(MAKE) -C deps install


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
	$(MAKE) -C piqi-rpc clean


clean:: erlang-clean
	$(MAKE) -C deps clean
	$(MAKE) -C tests clean


distclean:
	$(MAKE) clean
	rm -rf $(PIQI_BUILD)

