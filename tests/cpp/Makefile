all: prep test


prep:
	ln -sf ../../piqi.org/piqi.piqi .
	ln -sf ../../piqi.org/piqtype.piqi .
	piqi expand piqi.piqi > piqi-expanded.piqi
	piqi convert -t pb -o piqi.piqi.pb piqi-expanded.piqi


test:
	piqi to-proto piqi.piqi
	protoc --cpp_out=. piqi.piqi.proto
	$(MAKE) -f Makefile.cpp
	#protoc --decode=piqi_org.piqi.piqi piqi.piqi.proto < piqi.piqi.pb > t.ptext
	./ctest > t.ptext1
	#cmp t.ptext*


clean:
	rm -f *.piqi piqi.* piqtype.* t.*
	$(MAKE) -f Makefile.cpp clean

