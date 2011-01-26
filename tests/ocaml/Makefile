all: prep test


prep:
	ln -sf ../../piqi.org/piqi.piqi .
	ln -sf ../../piqi.org/piqtype.piqi .
	ln -sf ../../piqilib/piqobj.piqi .
	piqi expand piqi.piqi > piqi-expanded.piqi
	piqi convert --add-defaults -t pb -o piqi.piqi.pb piqi-expanded.piqi


PIQIC_FLAGS = --pp --gen-defaults


test:
	piqic ocaml $(PIQIC_FLAGS) piqi.piqi
	piqic ocaml $(PIQIC_FLAGS) piqtype.piqi
	piqic ocaml $(PIQIC_FLAGS) piqobj.piqi
	$(MAKE) -f Makefile.ocaml
	./otest
	cmp *.pb


clean:
	rm -f *.piqi piqi.* piqtype.* piqobj.* t.*
	$(MAKE) -f Makefile.ocaml clean

