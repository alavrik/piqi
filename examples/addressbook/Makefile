.PHONY: all ocaml test_ocaml clean


all: ocaml test_ocaml

ocaml:
	GOAL=add_person $(MAKE) -f Makefile.ocaml
	GOAL=list_people $(MAKE) -f Makefile.ocaml


clean:
	GOAL=add_person $(MAKE) -f Makefile.ocaml clean
	GOAL=list_people $(MAKE) -f Makefile.ocaml clean
	rm -f addressbook.piq.pb addressbook.pb.piq l1 l2


test_ocaml:
	./test_ocaml

