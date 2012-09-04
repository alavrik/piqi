.PHONY: test

all: prep test


prep:
	ln -sf ../../piqi.org/piqi.piqi .
	ln -sf ../../piqic/piqi.erlang-types.piqi piqi.erlang.piqi
	ln -sf ../piqi_misc/piqobj.piqi .
	piqi convert --add-defaults -t pb piqi.piqi


PIQIC_FLAGS = --gen-defaults --embed-piqi


test:
	piqic erlang $(PIQIC_FLAGS) piqi.piqi
	piqic erlang $(PIQIC_FLAGS) piqobj.piqi
	$(MAKE) -f Makefile.erlang
	./test
	cmp *.pb


clean:
	rm -f *.piqi piqi.* piqi_piqi.* piqobj_piqi.* t.*
	$(MAKE) -f Makefile.erlang clean

