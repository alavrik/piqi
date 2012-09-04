all: prep test


prep:
	ln -sf ../../piqi.org/piqi.piqi .
	ln -sf ../../piqic/piqi.ocaml-types.piqi piqi.ocaml.piqi
	ln -sf ../piqi_misc/piqobj.piqi .
	piqi convert --add-defaults -t pb piqi.piqi


test:
	$(MAKE) -f Makefile.ocaml
	./otest
	cmp *.pb


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f *.piqi piqi.* t.*

