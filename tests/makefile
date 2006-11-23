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

INSTALL_TESTS = $(PWD)/tests/v2p-testdir

build:
	gnat make -Pregtests

setup install:

setup_tests:
	-$(MKDIR) -p $(INSTALL_TESTS)/db
	echo "db_name db/testing.db" > $(INSTALL_TESTS)/v2p.ini

install_tests:
	$(MAKE) -C ../web INSTALL=$(INSTALL_TESTS) install
	$(CP) ../db/data/*.s* $(INSTALL_TESTS)/db/
	(cd $(INSTALL_TESTS)/db; rm testing.db; ./create_database.sh)
	$(CP) bin/web_harness $(INSTALL_TESTS)/bin

runtests: setup_tests install_tests

clean:
	$(RM) -fr $(INSTALL_TESTS)
	gnat clean -Pregtests
