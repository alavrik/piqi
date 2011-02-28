
ERL_SOURCES = \
	$(TEST_ERL_FILES)


PIQI_FILE = example.piqi

MOD = example
ERL_RPC_MOD = $(MOD)_piqi_rpc.erl

TEST_HRL_FILES = $(MOD)_piqi.hrl $(MOD)_piqi_impl.hrl
TEST_ERL_FILES = $(MOD)_piqi.erl \
		 $(MOD)_piqi_default_impl.erl \
		 $(ERL_RPC_MOD)



ERL_APP_PATH = $(PIQI_ROOT)/piqirun-erlang
ERLC_FLAGS = #-pa $(PIQI_ROOT)/piqirun-erlang #-I $(PIQI_ROOT)/piqirun-erlang

EBIN_DIR = .


PIQIC = piqic
PIQIC_FLAGS = --gen-default-impl


all: ebin


$(TEST_ERL_FILES) $(TEST_HRL_FILES): $(PIQI_FILE)
	$(PIQIC) erlang $(PIQIC_FLAGS) $<
	piqic-erlang-rpc $< > $(ERL_RPC_MOD)


clean::
	rm -f $(TEST_HRL_FILES) $(TEST_ERL_FILES)


include $(PIQI_ROOT)/make/Makefile.erlang

