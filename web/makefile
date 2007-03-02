###########################################################################
#                              Vision2Pixels
#
#                          Copyright (C) 2006-2007
#                       Pascal Obry - Olivier Ramonat
#
#   This library is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or (at
#   your option) any later version.
#
#   This library is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this library; if not, write to the Free Software Foundation,
#   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
#  Simple makefile to build Vision2Pixels
###########################################################################

# Web setup

INSTALL_TESTS := $(PWD)/web/v2p-testdir

LOGFILE := $(INSTALL_TESTS)/web_harness.log.${shell date +%Y%m%d-%H%M%S}

all:
	gnat make -XPRJ_BUILD=$(MODE) -Pweb
	gnat make -XPRJ_BUILD=$(MODE) -Pweb_test

setup:
	-$(MKDIR) gen
	templates2ada -d templates -o gen/templates.cds \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds tsrc/
	$(RM) gen/templates.cds
	templates2ada -d templates -o gen/templates.cds -e .txml \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds gen/
	$(RM) gen/templates.cds
	$(CP) gen/v2p-template_defs-* tsrc/
	$(RM) -r gen

install-dir:
	-$(MKDIR) $(INSTALL)
	-$(MKDIR) $(INSTALL)/bin
	-$(MKDIR) $(INSTALL)/templates
	-$(MKDIR) $(INSTALL)/css
	-$(MKDIR) $(INSTALL)/css/img
	-$(MKDIR) $(INSTALL)/we_js
	-$(MKDIR) $(INSTALL)/uploads
	-$(MKDIR) $(INSTALL)/images
	-$(MKDIR) $(INSTALL)/xml

install: install-dir
	$(CP) css/*.css $(INSTALL)/css
	$(CP) css/img/*.jpg $(INSTALL)/css/img
	$(CP) css/img/*.gif $(INSTALL)/css/img
	$(CP) templates/*.*html $(INSTALL)/templates
	$(CP) templates/*.txml $(INSTALL)/templates
	$(CP) we_js/*js $(INSTALL)/we_js
	$(CP) xml/*xml $(INSTALL)/xml
	$(CP) bin/vision2pixels$(EXEXT) $(INSTALL)/bin

clean:
	-$(RM) -r gen src/v2p-template_defs*
	-gnat clean -r -Pweb
	$(RM) -fr $(INSTALL_TESTS)
	gnat clean -Pweb_test

setup_tests:
	-$(MKDIR) -p $(INSTALL_TESTS)/db
	echo "db_name db/testing.db" > $(INSTALL_TESTS)/v2p.ini

install_tests:
	$(MAKE) INSTALL=$(INSTALL_TESTS) install
	$(CP) ../db/data/*.s* $(INSTALL_TESTS)/db/
	(cd $(INSTALL_TESTS)/db; rm -f testing.db; ./create_database.sh)
	$(CP) test/bin/web_harness $(INSTALL_TESTS)/bin

run_tests:
	(cd $(INSTALL_TESTS); bin/web_harness > $(LOGFILE))
	grep "Failed Tests" $(LOGFILE) >> $(LOG)
	grep "Unexpected Errors" $(LOGFILE) >> $(LOG)

runtests: setup_tests install_tests run_tests
