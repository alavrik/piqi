

PIQI_FILES = \
	piqi.piq.piqi \
	piqi.protobuf.piqi \
	piqi.json.piqi \
	piqi.getopt.piqi \
	\
	piqi.piqi \
	piqi-lang.piqi


test: test_piqi test_piqi_piqi


test_piqi:
	set -e; \
	for i in $(PIQI_FILES:%.piqi=%); \
	do \
		../examples/test_piqi $$i --no-warnings >/dev/null; \
	done


test_piqi_piqi:
	piqi to-proto piqi.piqi
	piqi expand --add-module-name piqi.piqi > p.piqi
	piqi check p.piqi
	piqi expand --add-module-name --includes-only piqi.piqi > pp.piqi
	piqi check pp.piqi
	piqi convert -t pb p.piqi
	piqi convert -t pb piqi.piqi
	cmp p.piqi.pb piqi.piqi.pb
	test ! `which protoc` || protoc -I.. -I. --decode=piqi_org.piqi.piqi piqi.piqi.proto < p.piqi.pb > p.piqi.text
	piqi convert -t piq-json p.piqi
	piqi convert p.piqi.json >/dev/null
	piqi convert -t xml p.piqi
	piqi convert --piqtype piqi p.piqi.xml >/dev/null


clean:
	rm -f p.piqi pp.piqi p.piqi.text *.proto *.wire *.pb *.json *.xml

