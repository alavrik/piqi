include $(PIQI_ROOT)/make/Makefile.ocaml
include Makefile.piqilib


RESULT = piqilib
PIQI_LIB_NAME = lib


STATIC = 1 # force creation of static library (don't create dlls)


all: bcl ncl


post_target:: install


#NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall libinstall
	$(PIQI_ROOT)/make/piqi-fix-libinstall $(RESULT) $(PIQI_LIB_NAME)

uninstall: libuninstall


include $(OCAMLMAKEFILE)
