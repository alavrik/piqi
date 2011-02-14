
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	piqi_tools.erl \


#ERL_TESTS = piqirun


ERLC_FLAGS = -pa $(PIQI_ROOT)/piqirun-erlang -I $(PIQI_ROOT)/piqirun-erlang # XXX
#EBIN_DIR = .


PIQIC = piqic

PIQI_FILES = piqi-rpc.piqi piqi-tools.piqi

PIQI_ERL_FILES = piqi_rpc_piqi.erl piqi_tools_piqi.erl
PIQI_HRL_FILES = piqi_rpc_piqi.hrl piqi_tools_piqi.hrl


PRE_TARGET = $(PIQI_FILES)


VSN = $(shell cat $(PIQI_ROOT)/VERSION)
#ERL_APP_FILE = piqirun.app


all:  ebin


test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


$(ERL_APP_FILE): $(ERL_APP_FILE).templ
	sed -e 's;%VSN%;$(VSN);' $< > $@



$(PIQI_FILES): %: $(PIQI_ROOT)/piqi-tools/%
	ln -s $< $@


$(PIQI_ERL_FILES): $(PIQI_FILES)
	for i in $^; do \
		$(PIQIC) erlang $$i; \
	done


clean::
	rm -f $(ERL_APP_FILE) $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


include $(PIQI_ROOT)/make/Makefile.erlang

