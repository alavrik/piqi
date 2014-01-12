ifeq ($(MAKECMDGOALS),distclean)
-include Makefile.config
-include $(PIQI_ROOT)/make/Makefile.dirs
else
include Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs
endif


DIRS = \
	piqilib \
	piqi-tools \
	piqirun-ocaml piqic-ocaml \


.PHONY: deps build-dir install distclean \
	ocaml ocaml-install ocaml-uninstall \
	doc


# export installation and search path for OCaml dependencies
ifeq ($(MAKECMDGOALS),deps)
OCAMLFIND_DESTDIR = $(PIQI_BUILD)/lib/ocaml
OCAMLPATH = $(OCAMLFIND_DESTDIR)
export OCAMLFIND_DESTDIR OCAMLPATH
endif


# export installation path for third-party deps and Piqi OCaml libraries
ifneq ($(findstring ocaml-,$(MAKECMDGOALS)),)
ifneq ($(PIQI_OCAML_DESTDIR),)
OCAMLFIND_DESTDIR = $(PIQI_OCAML_DESTDIR)
OCAMLPATH = $(OCAMLFIND_DESTDIR)
export OCAMLFIND_DESTDIR OCAMLPATH
endif
endif


pre_target:: build-dir


deps: build-dir
	$(MAKE) -C deps
	$(MAKE) -C deps install


build-dir:
	mkdir -p build/lib/ocaml
	cd build/lib/ocaml && test -L piqi || ln -sf ../../../piqilib piqi
	cd build/lib/ocaml && test -L piqirun || ln -sf ../../../piqirun-ocaml piqirun


doc:
	$(MAKE) -C doc


install:
	-install -d $(DESTDIR)$(PIQI_PREFIX)/bin
	install piqi-tools/piqi $(DESTDIR)$(PIQI_PREFIX)/bin
	-install -d $(DESTDIR)$(PIQI_PREFIX)/share/man/man1
	install -m 644 doc/piqi.1 $(DESTDIR)$(PIQI_PREFIX)/share/man/man1


ocaml:
	$(MAKE) -C piqilib bcl install


ocaml-install: ocaml-uninstall
	test -d $(PIQI_OCAML_DESTDIR) || mkdir -p $(PIQI_OCAML_DESTDIR)
	$(MAKE) -C deps install
	ocamlfind install piqi `ls $(PIQI_BUILD)/lib/ocaml/piqi/*`
	ocamlfind install piqirun `ls $(PIQI_BUILD)/lib/ocaml/piqirun/*`
	-install -d $(DESTDIR)$(PIQI_PREFIX)/bin
	install piqic-ocaml/piqic-ocaml $(DESTDIR)$(PIQI_PREFIX)/bin


ocaml-uninstall:
	$(MAKE) -C deps uninstall
	ocamlfind remove piqi
	ocamlfind remove piqirun
	rm -f $(DESTDIR)$(PIQI_PREFIX)/bin/piqic-ocaml


clean::
	$(MAKE) -C deps clean
	$(MAKE) -C tests clean


distclean:
	if [ -f Makefile.config ]; then \
		$(MAKE) clean; \
		rm -rf $(PIQI_BUILD); \
		rm Makefile.config; \
	fi

