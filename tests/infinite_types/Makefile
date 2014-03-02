INFINITE = \
	infinite_alias.piqi \
	infinite_alias2.piqi \
	infinite_list.piqi \
	infinite_list2.piqi \
	infinite_record.piqi \
	infinite_record2.piqi \
	infinite_variant.piqi \
	infinite_variant2.piqi \


test:
	piqi check finite.piqi
	for i in $(INFINITE); do \
		piqi check $$i; \
		test $$? -eq 0 && exit 1; \
	done; true


clean:

