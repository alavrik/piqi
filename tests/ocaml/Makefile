all: prep test


prep:
	ln -sf ../../piqi.org/piqi.piqi .
	ln -sf ../../piqi.org/piqtype.piqi .
	ln -sf ../../piqi/piqobj.piqi .
	piqi expand piqi.piqi > piqi-expanded.piqi
	piqi convert --add-defaults -t pb -o piqi.piqi.pb piqi-expanded.piqi


test:
	piqic ocaml --pp piqi.piqi
	piqic ocaml --pp piqtype.piqi
	piqic ocaml --pp piqobj.piqi
	$(MAKE) -f Makefile.ocaml
	./otest
	cmp *.pb


clean:
	rm -f *.piqi piqi.* piqtype.* piqobj.* t.*
	$(MAKE) -f Makefile.ocaml clean

