PIQI = $(PIQI_ROOT)/piqi-tools/piqi
PIQIC = $(PIQI_ROOT)/piqic-ocaml/piqic-ocaml

PIQI_FLAGS = -I $(PIQI_ROOT)
PIQIC_FLAGS = $(PIQI_FLAGS)


all: piqi_piqi piqi_impl_piqi gen_piqi_boot expand_piqi_boot
	mv piqi_piqi.ml piqi_impl_piqi.ml piqi_boot.ml ..


piqi_impl_piqi:
	$(PIQIC) $(PIQIC_FLAGS) --cc --pp piqi-impl.piqi


piqi_piqi:
	$(PIQIC) $(PIQIC_FLAGS) --pp piqi.piqi


gen_piqi_boot:
	$(PIQI) convert -t pb piqi-lang.piqi
	$(PIQI) convert -t pb piqi.piqi
	ocaml gen_piqi_boot.ml > piqi_boot.ml
	rm piqi-lang.piqi.pb piqi.piqi.pb


# turn Protobuf specs into OCaml data structures (optional)
expand_piqi_boot:
	$(MAKE) top
	echo "#print_length 1000000;; #print_depth 1000000;;" > reboot_init.ocaml
	(set -e; \
	 echo "let piqi_spec ="; \
	 echo "Piqi_boot.piqi_spec;;" | ./reboot.top -noprompt -init reboot_init.ocaml | sed -e '1,3d'; \
	 echo "let piqi_lang ="; \
	 echo "Piqi_boot.piqi_lang;;" | ./reboot.top -noprompt -init reboot_init.ocaml | sed -e '1,3d'; \
	) > piqi_boot_expanded.ml
	mv piqi_boot_expanded.ml piqi_boot.ml
	$(MAKE) clean
	rm -f reboot_init.ocaml


include $(PIQI_ROOT)/Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = reboot
SOURCES = piqi_impl_piqi.ml piqi_boot.ml
PACKS = piqi.lib


include $(OCAMLMAKEFILE)

# ex:ft=make
