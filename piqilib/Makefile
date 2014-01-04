include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqilib
PIQI_LIB_NAME = lib
LIBINSTALL_FILES = \
	$(RESULT).cmxa \
	$(RESULT).$(EXT_LIB) $(RES_CLIB) \
	$(shell ls *.cmi 2>/dev/null) \
	piqirun_ext.mli

# we're not building byte-code library by default, only when requested
# explicitly, e.g. from top-level Makefile
ifneq ($(findstring bcl,$(MAKECMDGOALS)),)
LIBINSTALL_FILES += $(RESULT).cma
endif

ifneq ($(findstring debug,$(MAKECMDGOALS)),)
LIBINSTALL_FILES += $(RESULT).cma
endif


STATIC = 1 # force creation of static library (don't create dlls)

PACKS = piqirun ulex easy-format xmlm

SOURCES = \
	piqi_version.ml \
	piqi_piqi.ml \
	\
	piqloc.ml \
	piqi_util.ml \
	piq_ast.ml \
	\
	piqi_impl_piqi.ml \
	piqi_boot.ml \
	\
	piqi_c.ml piqi_c.mli \
	piqi_c_impl.c \
	\
	piqi_config.ml \
	piqi_iolist.ml \
	piqi_graph.ml \
	piqi_name.ml \
	piqi_common.ml \
	piqi_file.ml \
	piqi_main.ml \
	piqi_protobuf.ml \
	piqi_db.ml \
	\
	piq_lexer.ml \
	piq_parser.ml \
	piq_gen.ml \
	\
	piqi_json_type.mli \
	piqi_xml_type.mli \
	\
	piqi_objstore.ml \
	piqobj.ml \
	piqobj_common.ml \
	piqobj_to_protobuf.ml \
	piqobj_of_protobuf.ml \
	piqobj_to_piq.ml \
	piqobj_of_piq.ml \
	\
	piqi.ml \
	\
	piqi_pp.ml \
	\
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
	piqi_compile.ml \
	\
	piqirun_ext.mli \
	piqirun_ext.ml \
	\
	piqi_light.ml \
	piqi_getopt.ml \


PRE_TARGETS = piqi_version.ml


all: ncl


debug: dcl install


post_target:: install


#NOTE: when installing, uninstall first to avoid "already installed" error
install: uninstall libinstall
	$(PIQI_ROOT)/make/piqi-fix-libinstall $(RESULT) $(PIQI_LIB_NAME)


uninstall: libuninstall


piqi_version.ml: $(PIQI_ROOT)/VERSION
	echo "let version = \"`head -1 $<`\"" >$@


# re-generate piqi_piqi.ml piqi_impl_piqi.ml and piqi_boot.ml
#
# NOTE: we need to the bytecode version of the library (bcl) for reboot toplevel
# -- see Makefile.reboot for details; also, we need to unset REAL_OCAMLFIND,
# because OCamlMakefile is not re-entrant otherwise
reboot:
	$(MAKE) bcl install
	unset REAL_OCAMLFIND && $(MAKE) -C boot


include $(OCAMLMAKEFILE)
