include $(PIQI_ROOT)/make/Makefile.dirs


# Are Protocol Buffers tools installed? Assuming here that the C++ .so libraries
# are installed as well.
ifneq ($(shell which protoc), )
DIRS_DEPENDING_ON_PROTO = \
       piqi_proto \
       piqi_proto1 \
       piqi_packed \
       boot_proto2piqi \
       cpp
endif


DIRS_ERLANG = \
	erlang_addressbook \
	erlang_piqi \
	erlang_perf \
	erlang_packed \
	erlang_misc \


DIRS_OCAML = \
       ocaml_addressbook \
       ocaml_piqi \
       ocaml_perf \
       ocaml_packed \
       ocaml_array \
       ocaml_misc \
       ocaml_custom_types \


DIRS = \
       examples \
       piqi.org \
       piqi_server \
       $(DIRS_OCAML) \
       $(DIRS_DEPENDING_ON_PROTO)


.PHONY: ocaml ocaml-clean erlang erlang-clean


ocaml:
	set -e; \
	for i in $(DIRS_OCAML); do \
		$(MAKE) -C $$i; \
	done


ocaml-clean:
	for i in $(DIRS_OCAML); do \
		$(MAKE) -C $$i clean; \
	done


erlang:
	set -e; \
	for i in $(DIRS_ERLANG); do \
		$(MAKE) -C $$i; \
	done


erlang-clean:
	for i in $(DIRS_ERLANG); do \
		$(MAKE) -C $$i clean; \
	done


clean:: erlang-clean

