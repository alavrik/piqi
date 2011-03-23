.PHONY: all ocaml test clean


all: ocaml test


ocaml:
	GOAL=add_person $(MAKE) -f Makefile.ocaml
	GOAL=list_people $(MAKE) -f Makefile.ocaml


test:
	./test


clean:
	GOAL=add_person $(MAKE) -f Makefile.ocaml clean
	GOAL=list_people $(MAKE) -f Makefile.ocaml clean
	rm -f addressbook.piq.pb addressbook.pb.piq l1 l2

