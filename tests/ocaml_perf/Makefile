.PHONY: all ocaml test clean


all: ocaml test


ocaml: piqi-obj.piqi
	$(MAKE) -f Makefile.ocaml


piqi-obj.piqi: ../../piqi.org/piqi.piqi
	ln -s ../../piqi.org/piqi.piqi
	ln -s ../../piqic/piqi.ocaml-types.piqi piqi.ocaml.piqi
	piqic expand piqi.piqi > piqi.expanded
	cat piqi.expanded | sed -e 's/piqi.org\/piqi/piqi-obj/;s/Piqi/Piqi_obj/' >$@


test: ocaml
	piqi convert -t pb addressbook.piq

	echo ":piqi-obj/piqi [" > piqi.piq
	cat piqi-obj.piqi >> piqi.piq
	echo "]" >> piqi.piq

	piqi convert --no-warnings --add-defaults -t pb piqi.piq
	#./test


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f addressbook.piq.pb piqi-expanded.piqi piqi.* piqi-obj.*

