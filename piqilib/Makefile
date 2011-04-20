include $(PIQI_ROOT)/make/Makefile.ocaml
include $(PIQI_ROOT)/piqilib/Makefile.piqi_extended


RESULT = piqilib
PIQI_LIB_NAME = lib
LIBINSTALL_FILES = \
	$(RESULT).cma $(RESULT).cmxa \
	$(RESULT).$(EXT_LIB) $(RES_CLIB) \
	$(shell ls *.cmi) \
	# *.mli -- TODO


PACKS += xmlm

STATIC = 1 # force creation of static library (don't create dlls)

PIQI_SOURCES_LIB = \
	piqi_json_common.ml \
	piqi_json_parser.mll \
	piqi_json_gen.ml \
	piqi_json.ml \
	\
	piqi_base64.ml \
	piqobj_to_json.ml \
	piqobj_of_json.ml \
	\
	piqi_xml.ml \
	piqobj_to_xml.ml \
	piqobj_of_xml.ml \
	\
	piq.ml \
	piqi_convert.ml \
	\
	piqi_light.ml \
	piqi_getopt.ml \


PIQI_SOURCES_TAIL += $(PIQI_SOURCES_LIB)


PIQI_PIQI = piqi-boot.piqi piqi.piqi piqi-impl.piqi


PIQICC = ../piqicc/piqicc
PIQICC_FLAGS = -I $(PIQI_ROOT)


all: bcl ncl


debug: dcl


post_target:: install


piqi_piqi.ml: $(PIQI_PIQI)
	# Note, using standard piqi.org/piqi-boot as boot module for piqicc
	$(PIQICC) $(PIQICC_FLAGS) -o $@.tmp.ml \
		--boot piqi-boot.piqi --piqi piqi.piqi --impl piqi-impl.piqi
	$(CAMLP4)o -o $@ $@.tmp.ml


#NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall libinstall
	$(PIQI_ROOT)/make/piqi-fix-libinstall $(RESULT) $(PIQI_LIB_NAME)

uninstall: libuninstall


clean::
	rm -f piqi_piqi.ml.tmp.ml


include $(OCAMLMAKEFILE)
