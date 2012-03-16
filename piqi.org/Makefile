

PIQI_FILES = \
	piqast.piqi \
	piqtype.piqi \
	\
	piqi.proto.piqi \
	piqi.wire.piqi \
	piqi.json.piqi \
	piqi.getopt.piqi \
	\
	piqi.piqi \
	piqi-lang.piqi \
	\
	piqobj.piqi


BOOT_PIQI_FILES = piqi-boot.piqi piqtype-core.piqi


test: test_boot test_piqi test_piqi_piqi test_piqtype_core


test_boot:
	set -e; \
	ln -sf ../piqilib/piqobj.piqi piqobj.piqi; \
	for i in $(BOOT_PIQI_FILES); \
	do \
		piqi check --noboot $$i; \
	done


test_piqi:
	set -e; \
	for i in $(PIQI_FILES:%.piqi=%); \
	do \
		../examples/test_piqi $$i >/dev/null; \
	done


test_piqi_piqi:
	piqi to-proto piqi.piqi
	piqi to-proto piqtype.piqi
	piqi expand piqi.piqi > p.piqi
	piqi check p.piqi
	piqi expand --includes-only piqi.piqi > pp.piqi
	piqi check pp.piqi
	piqi convert -t pb p.piqi
	piqi convert -t pb piqi.piqi
	cmp p.piqi.pb piqi.piqi.pb
	test ! `which protoc` || protoc -I.. -I. --decode=piqi_org.piqi.piqi piqi.piqi.proto < p.piqi.pb > p.piqi.text
	piqi convert -t piq-json p.piqi
	piqi convert p.piqi.json >/dev/null
	piqi convert -t xml p.piqi
	piqi convert --piqtype piqi p.piqi.xml >/dev/null


test_piqtype_core:
	cat piqtype-core.piqi | sed -e 's!\.module.*!:piqtype piqi.org/piqtype-core/piqdef!' >piqtype-core.piq
	piqi check --noboot piqtype-core.piq


clean:
	rm -f p.piqi pp.piqi p.piqi.text *.proto *.wire *.pb *.json *.xml piqtype-core.piq piqobj.piqi

