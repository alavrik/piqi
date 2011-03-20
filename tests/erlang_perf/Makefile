.PHONY: all erlang test clean


all: erlang test


erlang: piqi.piqi
	$(MAKE) -f Makefile.erlang


piqi.piqi: ../../piqi.org/piqi.piqi
	piqi expand $< | sed -e 's/piqi.org\///' >$@


test: erlang
	piqi convert -t pb addressbook.piq

	echo ":piqi/piqi [" > piqi.piq
	cat piqi.piqi >> piqi.piq
	echo "]" >> piqi.piq

	piqi convert --add-defaults -t pb piqi.piq
	#./test


clean:
	$(MAKE) -f Makefile.erlang clean
	rm -f addressbook.piq.pb piqi-expanded.piqi piqi.*

