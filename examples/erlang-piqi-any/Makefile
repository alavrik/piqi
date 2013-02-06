.PHONY: all erlang test clean


all: erlang test


erlang:
	$(MAKE) -f Makefile.erlang


test: erlang
	./test


clean:
	$(MAKE) -f Makefile.erlang clean

