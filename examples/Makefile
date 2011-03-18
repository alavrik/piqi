all: test_builtin test_complex

.PHONY: test_builtin test_complex

test_builtin:
	set -e; \
	for i in comment bool int float string piq-text piq-word empty; \
	do \
		./test_piq $$i; \
	done


test_complex:
	./test_piqi empty
	./test_piqi_proto empty

	./test_piq complex
	./test_piqi complex
	./test_piqi_proto complex

	./test_piq person
	./test_piqi person
	./test_piqi_proto person

	./test_piqi def
	./test_piqi_proto def
	./test_piq list

	./test_piq flag

	./test_piqi record-variant-list
	./test_piqi_proto record-variant-list
	./test_piq record-variant-list

	ln -sf ../piqilib/piqobj.piqi .
	./test_piqi piqobj

	./test_piqi function
	./test_piq function
	./test_piqi_proto function

	./test_getopt

	test ! `which protoc` || (piqi of-proto --normalize addressbook.proto; \
	./test_piqi addressbook.proto; \
	./test_piq addressbook;)


clean:
	rm -f *.pb *.wire *.json *.xml *.piqi.proto* addressbook.proto.piqi piqobj.piqi
