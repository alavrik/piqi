include $(PIQI_ROOT)/make/Makefile.dirs


DIRS = \
	piqi-camlp4 \
	piqirun-ocaml \
	piqicc piqic piqilib piqi-tools \


.PHONY: deps install ocaml ocaml-install ocaml-uninstall erlang erlang-clean distclean


# export installation and search path for OCaml dependencies
ifeq ($(MAKECMDGOALS),deps)
OCAMLFIND_DESTDIR = $(PIQI_BUILD)/lib/ocaml
OCAMLPATH = $(OCAMLFIND_DESTDIR)
export OCAMLFIND_DESTDIR OCAMLPATH
endif


# export installation path for Piqi OCaml libraries
ifneq ($(findstring ocaml-,$(MAKECMDGOALS)),)
ifneq ($(PIQI_OCAML_PREFIX),)
OCAMLFIND_INSTFLAGS = -destdir $(PIQI_OCAML_PREFIX)
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


ocaml:
	$(MAKE) -C piqilib bcl install


ocaml-install: ocaml-uninstall
	test -d $(PIQI_OCAML_PREFIX) || mkdir -p $(PIQI_OCAML_PREFIX)
	ocamlfind install $(OCAMLFIND_INSTFLAGS) piqi `ls $(PIQI_BUILD)/lib/ocaml/piqi/*`


ocaml-uninstall:
	ocamlfind remove $(OCAMLFIND_INSTFLAGS) piqi


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

