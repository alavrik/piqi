include $(PIQI_ROOT)/make/Makefile.dirs


# Are Protocol Buffers tools installed? Assuming here that the C++ .so libraries
# are installed as well.
ifneq ($(shell which protoc), )
DIRS_DEPENDING_ON_PROTO = \
       piqi_proto \
       boot_proto2piqi \
       cpp
endif


DIRS = \
       examples \
       piqi.org \
       piqi_server \
       ocaml_piqi \
       ocaml_addressbook \
       $(DIRS_DEPENDING_ON_PROTO)


.PHONY: erlang erlang-clean

erlang:
	$(MAKE) -C erlang_addressbook
	$(MAKE) -C erlang_piqi


erlang-clean:
	$(MAKE) -C erlang_addressbook clean
	$(MAKE) -C erlang_piqi clean


clean:: erlang-clean

