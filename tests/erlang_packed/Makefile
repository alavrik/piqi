
all:
	$(MAKE) -f Makefile.erlang
	piqi convert -t pb test-all.piq
	./test
	cmp test-all.piq.pb test-all.piq.pb.pb


clean:
	$(MAKE) -f Makefile.erlang clean
	rm -f test-all.piq.*
