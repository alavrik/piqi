OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = bytes

SOURCES =

LIBINSTALL_FILES = META


all:


install: libinstall


uninstall: libuninstall


# make META available under bytes directory to satisfy build dependency for
# base64
download:
	cp META ../bytes


clean::
	rm -f ../bytes/META


include $(OCAMLMAKEFILE)
