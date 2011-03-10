
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	piqic_erlang_ext.erl


ERLC_FLAGS = #-pa $(PIQI_ROOT)/piqi-erlang #-I $(PIQI_ROOT)/piqi-erlang
ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
#alternative method:
#ERL_LIBS = $(PIQI_ROOT)/piqi-erlang
#export ERL_LIBS

EBIN_DIR = .


PIQIC = piqic
PIQIC_FLAGS =

PIQI_FILES = piqi.piqi
PIQI_ERL_FILES = piqi_piqi.erl
PIQI_HRL_FILES = piqi_piqi.hrl


PRE_TARGET =


PIQIC_PLUGIN = piqic-erlang-ext


all: ebin $(PIQIC_PLUGIN)


$(PIQI_ERL_FILES) $(PIQI_HRL_FILES): $(PIQI_FILES)
	for i in $^; do \
		$(PIQIC) erlang $(PIQIC_FLAGS) $$i; \
	done


PIQIC_PLUGIN_BEAMS = \
    $(PIQI_ROOT)/piqi-erlang/ebin/piqirun.beam \
    piqi_piqi.beam \
    piqic_erlang_ext.beam


$(PIQIC_PLUGIN): $(PIQIC_PLUGIN_BEAMS)
	$(PIQI_ROOT)/make/make_escript $@ $(PIQIC_PLUGIN_BEAMS)
	chmod +x $@


clean::
	rm -f $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)
	rm -f $(PIQIC_PLUGIN)


include $(PIQI_ROOT)/make/Makefile.erlang

