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

INSTALL_MAIN   := $(PWD)/web/v2p-testdir
INSTALL_PLUGIN := $(PWD)/web/v2p-testdir/plugins/vision2pixels

LOGFILE := $(INSTALL_MAIN)/web_harness.log.${shell date +%Y%m%d-%H%M%S}

build:
	-chmod a+w lib_ali/*
	gnat make -XPRJ_BUILD=$(MODE) -Pweb
	gnat make -XPRJ_BUILD=$(MODE) -Pweb_test

check:
	gnat check -dd -Pweb -rules -from=../v2p.check
	gnat check -dd -Pweb_test -rules -from=../v2p.check

setup:
	-$(MKDIR) gen tsrc
	templates2ada -d templates/ -o gen/templates.cds \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds tsrc/
	$(RM) gen/templates.cds
	templates2ada -d templates/ -o gen/templates.cds -e .txml \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds gen/
	$(RM) gen/templates.cds
	$(CP) gen/v2p-template_defs-* tsrc/
	$(RM) -r gen

install-plugin-dir:
	-$(MKDIR) $(INSTALL)
	-$(MKDIR) $(INSTALL)/templates
	-$(MKDIR) $(INSTALL)/css
	-$(MKDIR) $(INSTALL)/css/img
	-$(MKDIR) $(INSTALL)/we_js
	-$(MKDIR) $(INSTALL)/uploads
	-$(MKDIR) $(INSTALL)/images
	-$(MKDIR) $(INSTALL)/xml

install-plugin: install-plugin-dir
	$(CP) css/*.css $(INSTALL)/css
	$(CP) css/img/*.jpg $(INSTALL)/css/img
	$(CP) css/img/*.gif $(INSTALL)/css/img
	$(CP) templates/*.*html $(INSTALL)/templates
	$(CP) templates/*.txml $(INSTALL)/templates
	$(CP) we_js/*js $(INSTALL)/we_js
	$(CP) xml/*xml $(INSTALL)/xml

install-dir:
	-$(MKDIR) $(INSTALL_MAIN)/bin
	-$(MKDIR) $(INSTALL_MAIN)/lib
	-$(MKDIR) $(INSTALL_MAIN)/lib/websites
	-$(MKDIR) $(INSTALL_MAIN)/lib/services

install: install-dir
	$(RM) $(INSTALL_MAIN)/lib/websites/*vision2pixels$(SOEXT)
	$(CP) lib/*vision2pixels$(SOEXT) $(INSTALL_MAIN)/lib/websites

clean:
	-$(RM) -r gen src/v2p-template_defs*
	-gnat clean -r -Pweb
	$(RM) -fr $(INSTALL_MAIN)
	gnat clean -Pweb_test

setup_tests:
	-$(MKDIR) -p $(INSTALL_PLUGIN)/db
	echo "server_port 8042" > $(INSTALL_MAIN)/aws.ini
	echo "db_name db/testing.db" > $(INSTALL_PLUGIN)/v2p.ini

install_tests: install
	$(MAKE) INSTALL=$(INSTALL_PLUGIN) install-plugin
	$(CP) ../db/data/*.s* $(INSTALL_PLUGIN)/db/
	(cd $(INSTALL_PLUGIN)/db; rm -f testing.db; ./create_database.sh)
	$(CP) test/bin/web_harness $(INSTALL_MAIN)/bin

run_tests:
	(cd $(INSTALL_MAIN); bin/web_harness > $(LOGFILE))
	-grep "Failed Tests" $(LOGFILE) >> $(LOG)
	-grep "Unexpected Errors" $(LOGFILE) >> $(LOG)

runtests: setup_tests install_tests run_tests

check_mem: setup_tests install_tests
	$(GNATMAKE) -Pcheck_mem
	$(CP) test/bin/check_mem $(INSTALL_MAIN)/bin
	(cd $(INSTALL_MAIN); \
	rm -f check_mem.*; \
	rm -f run1 run2; \
	echo "server_port 8043" > aws.ini;\
	bin/check_mem 2 > check_mem.res;\
	gnatmem 3 -i gmem.out bin/check_mem > check_mem.run1; \
	grep "   Final" check_mem.run1 > run1;\
	echo ",  run it again";\
	echo "server_port 8044" > aws.ini;\
	bin/check_mem 10 >> check_mem.res;\
	gnatmem 3 -i gmem.out bin/check_mem > check_mem.run2;\
	grep "   Final" check_mem.run2 > run2;\
	rm -f alldifs;\
	$(DIFF) run1 run2 1>> alldifs 2>> alldifs)
