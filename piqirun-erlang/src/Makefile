
ERL_SOURCES = \
	piqirun.erl \
	piqirun_tests.erl


ERL_TESTS = piqirun


ERLC_FLAGS =
#EBIN_DIR = .


all: ebin


test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


include $(PIQI_ROOT)/make/Makefile.erlang

