
test:
	piqi convert --piq-relaxed-parsing true 1.piq | cmp - 1.piq.converted

	piqi convert --piq-relaxed-parsing true -t pib 1.piq
	piqi convert 1.piq.pib | cmp - 1.piq.converted

	piqi convert --piq-relaxed-parsing true -t json 1.piq
	piqi convert 1.piq.json | cmp - 1.piq.json.converted


clean:
	rm -f *.pib *.json

