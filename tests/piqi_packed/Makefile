
all:
	cat packed.piqi | sed -e 's/\.wire-packed//' > unpacked.piqi
	cat test-all.piq | sed -e 's/:packed/:unpacked/' > test-all-unpacked.piq
	./test


clean:
	rm -f *.pb *.proto* *.wire unpacked.piqi test-all-unpacked.piq

