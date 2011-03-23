
DIRS = ulex-1.1 easy-format-1.0.0 xmlm-1.0.2


.PHONY: all build-install clean install uninstall install-dir


build-install: $(DIRS) install-dir
	set -e; \
	for i in $(DIRS); do \
		$(MAKE) -C $$i; \
		$(MAKE) -C $$i install; \
	done


install: install-dir all
uninstall: install-dir all
clean: all


all: $(DIRS)
	set -e; \
	for i in $(DIRS); do \
		$(MAKE) -C $$i $(MAKECMDGOALS); \
	done


install-dir: $(OCAMLFIND_DESTDIR)


$(OCAMLFIND_DESTDIR):
	mkdir -p $@

