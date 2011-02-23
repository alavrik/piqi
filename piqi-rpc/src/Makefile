
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	piqi_tools.erl \
	piqi_rpc_monitor.erl \
	piqi_rpc.erl \
	piqi_http_rpc.erl \
	piqic_erlang_rpc.erl \


#ERL_TESTS = piqirun


ERLC_FLAGS = -pa $(PIQI_ROOT)/piqirun-erlang -I $(PIQI_ROOT)/piqirun-erlang # XXX
#EBIN_DIR = .


PIQIC = piqic
PIQIC_FLAGS =


PIQI_FILES = piqi-rpc.piqi piqi-tools.piqi

PIQI_ERL_FILES = piqi_rpc_piqi.erl piqi_tools_piqi.erl piqi.erl
PIQI_HRL_FILES = piqi_rpc_piqi.hrl piqi_tools_piqi.hrl piqi.hrl


PRE_TARGET = $(PIQI_FILES)


PIQIC_PLUGIN = piqic-erlang-rpc


VSN = $(shell cat $(PIQI_ROOT)/VERSION)
#ERL_APP_FILE = piqirun.app


all: ebin $(PIQIC_PLUGIN)
	$(MAKE) -f Makefile.test


test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


$(ERL_APP_FILE): $(ERL_APP_FILE).templ
	sed -e 's;%VSN%;$(VSN);' $< > $@



$(PIQI_FILES): %: $(PIQI_ROOT)/piqi-tools/%
	ln -s $< $@


$(PIQI_ERL_FILES): $(PIQI_FILES) piqi.piqi
	for i in $^; do \
		$(PIQIC) erlang $(PIQIC_FLAGS) $$i; \
	done


PIQIC_PLUGIN_BEAMS = \
    $(PIQI_ROOT)/piqirun-erlang/ebin/piqirun.beam \
    ../ebin/piqi.beam \
    ../ebin/piqic_erlang_rpc.beam

$(PIQIC_PLUGIN): make_escript $(PIQIC_PLUGIN_BEAMS)
	./make_escript $@ $(PIQIC_PLUGIN_BEAMS)
	chmod +x $@


clean::
	rm -f $(ERL_APP_FILE) $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)
	rm -f $(PIQIC_PLUGIN)
	$(MAKE) -f Makefile.test clean


include $(PIQI_ROOT)/make/Makefile.erlang

