.PHONY: test

all: prep test


prep:
	ln -sf ../../piqi.org/piqi.piqi .
	ln -sf ../../piqi.org/piqtype.piqi .
	ln -sf ../../piqilib/piqobj.piqi .
	piqi expand piqi.piqi > piqi-expanded.piqi
	piqi convert --add-defaults -t pb -o piqi.piqi.pb piqi-expanded.piqi


test:
	piqic erlang piqi.piqi
	piqic erlang piqtype.piqi
	piqic erlang piqobj.piqi
	$(MAKE) -f Makefile.erlang
	./test
	cmp *.pb


clean:
	rm -f *.piqi piqi.* piqtype.* piqobj.* t.*
	$(MAKE) -f Makefile.erlang clean

