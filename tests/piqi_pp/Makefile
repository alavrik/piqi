
all:
	piqi pp t.piq | cmp - t.piq.pp
	piqi pp --expand-abbr t.piq | cmp - t.piq.pp-expand-abbr
	piqi pp --expand-abbr --parse-literals t.piq | cmp - t.piq.pp-expand-abbr-parse-literals


clean:

