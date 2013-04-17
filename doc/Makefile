SRCS=$(filter-out %-patched.md,$(wildcard *.md))
HTML=$(patsubst %.md,%.html,$(SRCS))

.PHONY: all clean man html

all: man html

man: piqi.1

html: $(HTML) CHANGES.html

piqi.1: tools.md
	pandoc -s -t man $< -o $@

CHANGES.html: ../CHANGES
	pandoc -s $< -o $@

%-patched.md: %.md
	sed -e 's|](\(/doc/erlang\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/doc/piqi-rpc\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/doc/faq\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/examples\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/self-definition\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/rationale\)| (external)](http://piqi.org\1|g' \
		-e '# Local stuff 2: /doc(/?)*#stuff) points to *.html#stuff' \
		-e 's|(/doc/\([^/]*\)/\?#\([^)]*\))|(\1.html#\2)|g' \
		-e '# Local stuff: /doc/*/) points to *.html' \
		-e 's|(/doc/\([^/]*\)/\?)|(\1.html)|g' $< > $@
	if [ "$@" = "tools-patched.md" ]; then \
		sed -i 's|#environmentvariables|#environment-variables|g' $@; \
	fi
	if [ "$@" = "index-patched.md" ]; then \
		sed -i 's|https://github.com/alavrik/piqi/blob/master/CHANGES|CHANGES.html|' $@; \
	fi

%.html: %-patched.md
	pandoc -s $< -o $@

clean:
	rm -f $(HTML) CHANGES.html piqi.1
