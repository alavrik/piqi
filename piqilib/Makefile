include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqi

LIBINSTALL_FILES += \
	$(CAMLP4_TARGETS) \
	$(RESULT).cma $(RESULT).cmxa \
	$(RESULT).$(EXT_LIB) $(RES_CLIB) \
	$(shell ls *.cmi 2>/dev/null) \


STATIC = 1 # force creation of static library (don't create dlls)

PACKS = ulex easy-format xmlm
INCDIRS = +camlp4  # for building camlp4 extensions


CAMLP4_SOURCES = pa_labelscope.ml pa_openin.ml
CAMLP4_TARGETS = pa_labelscope.cmo pa_openin.cmo


SOURCES = \
	piqi_version.ml \
	\
	piqi_piqirun.ml \
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
	piqi_light.ml \
	piqi_getopt.ml \


PRE_TARGETS = piqi_version.ml META $(CAMLP4_SOURCES) $(CAMLP4_TARGETS)


PIQI = ../piqi-tools/piqi
PIQIC = ../piqic-ocaml/piqic-ocaml

PIQI_FLAGS = -I $(PIQI_ROOT)
PIQIC_FLAGS = $(PIQI_FLAGS) --runtime Piqi_piqirun


all: ncl


debug: dcl top


# NOTE: when installing, uninstall first to avoid "already installed" error
# also, make sure we have the byte-code version of the library built as well
install: bcl uninstall libinstall


uninstall: libuninstall


piqi_version.ml: ../VERSION
	echo "let version = \"`head -1 $<`\"" >$@


META: ../VERSION META.tmpl
	echo "version = \"`head -1 $<`\"" >$@
	cat META.tmpl >>$@


$(CAMLP4_SOURCES):
	$(MAKE) -f Makefile.camlp4


# re-generate piqi_piqi.ml piqi_impl_piqi.ml and piqi_boot.ml from .piqi and
# also copying the latest version of piqilib.ml  -- called manually as "make
# reboot" when there are new versions of piqic-ocaml or the piqirun.pb runtime
# library
#
# NOTE: expand_piqi_boot is optional and can be safely turned off if it is
# giving troubles
reboot: reboot_piqi piqi_boot expand_piqi_boot


reboot_piqi:
	cp ../piqirun-ocaml/piqirun.ml piqi_piqirun.ml
	$(PIQIC) $(PIQIC_FLAGS) --cc --pp piqi-impl.piqi
	$(PIQIC) $(PIQIC_FLAGS) --pp piqi.piqi


piqi_boot:
	$(PIQI) convert -t pb piqi-lang.piqi
	$(PIQI) convert -t pb piqi.piqi
	ocaml gen_piqi_boot.ml > piqi_boot.ml
	rm piqi-lang.piqi.pb piqi.piqi.pb


# turn piqi specs serialized as Protobuf into OCaml data structures (optional)
expand_piqi_boot:
	$(MAKE) -f Makefile.expand_piqi_boot top
	echo "#print_length 1000000;; #print_depth 1000000;;" > expand_piqi_boot.ocaml
	(set -e; \
	 echo "let piqi_spec ="; \
	 echo "Piqi_boot.piqi_spec;;" | ./expand_piqi_boot.top -noprompt -init expand_piqi_boot.ocaml | sed -e '1,3d'; \
	 echo "let piqi_lang ="; \
	 echo "Piqi_boot.piqi_lang;;" | ./expand_piqi_boot.top -noprompt -init expand_piqi_boot.ocaml | sed -e '1,3d'; \
	) > piqi_boot_expanded.ml
	mv piqi_boot_expanded.ml piqi_boot.ml
	rm -f expand_piqi_boot.ocaml
	$(MAKE) -f Makefile.expand_piqi_boot clean


.PHONY: reboot reboot_piqi piqi_boot expand_piqi_boot


include $(OCAMLMAKEFILE)
