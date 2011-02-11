include $(PIQI_ROOT)/make/Makefile.dirs


DIRS = \
       boot_proto2piqi \
       piqi_proto \
       ocaml \
       cpp \
       examples \
       addressbook \
       piqi.org \
       piqi_server \


.PHONY: erlang erlang-clean

erlang:
	$(MAKE) -C addressbook test-erlang
	$(MAKE) -C erlang


erlang-clean:
	$(MAKE) -C erlang clean


clean:: erlang-clean

