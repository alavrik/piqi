include $(PIQI_ROOT)/make/Makefile.dirs


DIRS = \
       boot_proto2piqi \
       piqi_proto \
       ocaml_piqi \
       cpp \
       examples \
       ocaml_addressbook \
       piqi.org \
       piqi_server \


.PHONY: erlang erlang-clean

erlang:
	$(MAKE) -C erlang_addressbook
	$(MAKE) -C erlang_piqi


erlang-clean:
	$(MAKE) -C erlang_addressbook clean
	$(MAKE) -C erlang_piqi clean


clean:: erlang-clean

