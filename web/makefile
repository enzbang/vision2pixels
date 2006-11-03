
# Web setup

CP=cp -p

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

install-dir:
	-mkdir -p $(INSTALL)
	-mkdir -p $(INSTALL)/bin
	-mkdir -p $(INSTALL)/templates
	-mkdir -p $(INSTALL)/css
	-mkdir -p $(INSTALL)/we_js

install: install-dir
	$(CP) bin/vision2pixels$(EXEXT) $(INSTALL)/bin
	$(CP) css/*.css $(INSTALL)/css
	$(CP) templates/*.*html $(INSTALL)/templates
	$(CP) templates/*.txml $(INSTALL)/templates
	$(CP) we_js/*js $(INSTALL)/we_js
