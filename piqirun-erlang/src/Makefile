
ERL_SOURCES = \
	piqirun.erl \
	piqirun_tests.erl


ERL_TESTS = piqirun


ERLC_FLAGS =
#EBIN_DIR = .


VSN = $(shell cat $(PIQI_ROOT)/VERSION)
ERL_APP_FILE = piqirun.app


all:  ebin


test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


$(ERL_APP_FILE): $(ERL_APP_FILE).templ
	sed -e 's;%VSN%;$(VSN);' $< > $@


clean::
	rm -f $(ERL_APP_FILE)


include $(PIQI_ROOT)/make/Makefile.erlang

