
# Web setup

setup:
	mkdir gen
	templates2ada -d templates -o gen/templates.cds \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds src/
	rm -f gen/templates.cds
	templates2ada -d templates -o templates.cds -e .txml \
		-t templates/templates.tads
	gnat chop -wpq templates.cds gen/
	rm -f gen/templates.cds
	cp gen/v2p-template_defs-* src/
	rm -fr gen
