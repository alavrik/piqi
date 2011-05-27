include $(PIQI_ROOT)/make/Makefile.dirs


# Are Protocol Buffers tools installed? Assuming here that the C++ .so libraries
# are installed as well.
ifneq ($(shell which protoc), )
DIRS_DEPENDING_ON_PROTO = \
       piqi_proto \
       piqi_proto1 \
       packed \
       boot_proto2piqi \
       cpp
endif


DIRS = \
       examples \
       piqi.org \
       piqi_server \
       ocaml_addressbook \
       ocaml_piqi \
       ocaml_perf \
       ocaml_packed \
       $(DIRS_DEPENDING_ON_PROTO)


.PHONY: erlang erlang-clean

erlang:
	$(MAKE) -C erlang_addressbook
	$(MAKE) -C erlang_piqi
	$(MAKE) -C erlang_perf
	$(MAKE) -C erlang_packed


erlang-clean:
	$(MAKE) -C erlang_addressbook clean
	$(MAKE) -C erlang_piqi clean
	$(MAKE) -C erlang_perf clean
	$(MAKE) -C erlang_packed clean


clean:: erlang-clean

