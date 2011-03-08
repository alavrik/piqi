
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	test.erl \


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS =
EBIN_DIR = .


PIQIC = ../piqic-erlang-ext
PIQIC_FLAGS =


PIQI_FILES = addressbook.proto.piqi

PIQI_ERL_FILES = addressbook_piqi.erl addressbook_piqi_ext.erl
PIQI_HRL_FILES = addressbook_piqi.hrl


PRE_TARGET = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


.PHONY: test


all: ebin test


test:
	piqi convert -t pb addressbook.piq
	./test addressbook.piq.pb


$(PIQI_ERL_FILES) $(PIQI_HRL_FILES): $(PIQI_FILES)
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done


clean::
	rm addressbook.piq.pb


include $(PIQI_ROOT)/make/Makefile.erlang

