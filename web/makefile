###########################################################################
#                              Vision2Pixels
#
#                            Copyright (C) 2006
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

INSTALL_TESTS = $(PWD)/web/v2p-testdir

all:
	gnat make -XPRJ_Build=$(MODE) -Pweb
	gnat make -XPRJ_Build=$(MODE) -Pweb_test

setup:
	-$(MKDIR) gen
	templates2ada -d templates -o gen/templates.cds \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds src/
	$(RM) gen/templates.cds
	templates2ada -d templates -o gen/templates.cds -e .txml \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds gen/
	$(RM) gen/templates.cds
	$(CP) gen/v2p-template_defs-* src/
	$(RM) -r gen

install-dir:
	-$(MKDIR) $(INSTALL)
	-$(MKDIR) $(INSTALL)/bin
	-$(MKDIR) $(INSTALL)/templates
	-$(MKDIR) $(INSTALL)/css
	-$(MKDIR) $(INSTALL)/we_js
	-$(MKDIR) $(INSTALL)/uploads
	-$(MKDIR) $(INSTALL)/images

install: install-dir
	$(CP) css/*.css $(INSTALL)/css
	$(CP) templates/*.*html $(INSTALL)/templates
	$(CP) templates/*.txml $(INSTALL)/templates
	$(CP) we_js/*js $(INSTALL)/we_js
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
	(cd $(INSTALL_TESTS); bin/web_harness)
	echo "***** OK - no regression on module WEB"

runtests: setup_tests install_tests run_tests
