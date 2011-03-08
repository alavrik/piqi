
ERL_SOURCES = \
	piqirun.erl \
	\
	$(PIQI_ERL_FILES) \
	piqi_tools.erl \
	\
	piqirun_tests.erl


ERL_TESTS = piqirun


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS =
#EBIN_DIR = .


PIQIC = piqic
PIQIC_FLAGS =


PIQI_FILES = piqi-rpc.piqi piqi-tools.piqi

PIQI_ERL_FILES = piqi_rpc_piqi.erl piqi_tools_piqi.erl
PIQI_HRL_FILES = piqi_rpc_piqi.hrl piqi_tools_piqi.hrl


PRE_TARGET = $(PIQI_FILES)


VSN = $(shell cat $(PIQI_ROOT)/VERSION)
ERL_APP_FILE = piqi.app


all:  ebin


test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


$(ERL_APP_FILE): $(ERL_APP_FILE).templ
	sed -e 's;%VSN%;$(VSN);' $< > $@


$(PIQI_FILES): %: $(PIQI_ROOT)/piqi-tools/%
	ln -s $< $@


$(PIQI_ERL_FILES) $(PIQI_HRL_FILES): $(PIQI_FILES)
	for i in $^; do \
		$(PIQIC) erlang $(PIQIC_FLAGS) $$i; \
	done


clean::
	rm -f $(ERL_APP_FILE) $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


include $(PIQI_ROOT)/make/Makefile.erlang

