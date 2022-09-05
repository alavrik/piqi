include make/Makefile.dirs


DESTDIR ?= /usr/local


DIRS = piqirun piqic-ocaml


.PHONY: install uninstall clean distclean test


install:
	$(MAKE) -C piqirun install
	-install -d $(DESTDIR)/bin
	install piqic-ocaml/piqic-ocaml $(DESTDIR)/bin


uninstall:
	$(MAKE) -C piqirun uninstall
	rm -f $(DESTDIR)/bin/piqic-ocaml


test:
	$(MAKE) -C tests


distclean:
	$(MAKE) clean
	$(MAKE) -C tests clean

