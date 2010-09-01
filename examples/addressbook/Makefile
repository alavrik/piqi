.PHONY: all ocaml test-ocaml clean erlang test-erlang


all: ocaml test-ocaml


ocaml:
	GOAL=add_person $(MAKE) -f Makefile.ocaml
	GOAL=list_people $(MAKE) -f Makefile.ocaml


erlang:
	$(MAKE) -f Makefile.erlang


clean:
	GOAL=add_person $(MAKE) -f Makefile.ocaml clean
	GOAL=list_people $(MAKE) -f Makefile.ocaml clean
	$(MAKE) -f Makefile.erlang clean
	rm -f addressbook.piq.pb addressbook.pb.piq l1 l2


test-ocaml:
	./test ocaml


test-erlang: erlang
	./test erlang

