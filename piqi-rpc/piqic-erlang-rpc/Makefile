
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	piqic_erlang_rpc.erl


ERLC_FLAGS = #-pa $(PIQI_ROOT)/piqirun-erlang #-I $(PIQI_ROOT)/piqirun-erlang
ERL_APP_PATH = $(PIQI_ROOT)/piqirun-erlang
#alternative method:
#ERL_LIBS = $(PIQI_ROOT)/piqirun-erlang
#export ERL_LIBS

EBIN_DIR = .


PIQIC = piqic
PIQIC_FLAGS =

PIQI_FILES = piqi.piqi
PIQI_ERL_FILES = piqi_piqi.erl
PIQI_HRL_FILES = piqi_piqi.hrl


PRE_TARGET =


PIQIC_PLUGIN = piqic-erlang-rpc


all: ebin $(PIQIC_PLUGIN)


$(PIQI_FILES): %: $(PIQI_ROOT)/piqi-tools/%
	ln -s $< $@


$(PIQI_ERL_FILES) $(PIQI_HRL_FILES): $(PIQI_FILES)
	for i in $^; do \
		$(PIQIC) erlang $(PIQIC_FLAGS) $$i; \
	done


PIQIC_PLUGIN_BEAMS = \
    $(PIQI_ROOT)/piqirun-erlang/ebin/piqirun.beam \
    piqi_piqi.beam \
    piqic_erlang_rpc.beam


$(PIQIC_PLUGIN): make_escript $(PIQIC_PLUGIN_BEAMS)
	./make_escript $@ $(PIQIC_PLUGIN_BEAMS)
	chmod +x $@


clean::
	rm -f $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)
	rm -f $(PIQIC_PLUGIN)


include $(PIQI_ROOT)/make/Makefile.erlang

