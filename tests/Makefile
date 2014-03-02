include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs


# Are Protocol Buffers tools installed? Assuming here that the C++ .so libraries
# are installed as well.
ifneq ($(shell which protoc), )
DIRS_PROTO = \
       piqi_proto \
       piqi_proto1 \
       piqi_packed \
       cpp \

endif


DIRS = \
       examples \
       self_spec \
       extensions \
       piqi_misc \
       infinite_types \
       piqi_server \
       piqi_compile \
       piqi_pp \
       $(DIRS_PROTO)


.PHONY: proto proto-clean


proto:
	set -e; \
	for i in $(DIRS_PROTO); do \
		$(MAKE) -C $$i; \
	done


proto-clean:
	for i in $(DIRS_PROTO); do \
		$(MAKE) -C $$i clean; \
	done


clean::

