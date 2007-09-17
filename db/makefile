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

# Targets

LOGFILE := ${shell pwd}/test/db_harness.log.${shell date +%Y%m%d-%H%M%S}

build: create_db
	gnat make -XPRJ_BUILD=$(MODE) -Pdb_test

check:
	gnat check -dd -Pdb -rules -from=../v2p.check
	gnat check -dd -Pdb_test -rules -from=../v2p.check

setup:

install:
	(cd data; $(CP) testing.db $(GWIAD_ROOT)/plugins/vision2pixels/db)

create_db:
	(cd data; rm -f testing.db; ./create_database.sh)

run_tests:
	(cd test; ./db_harness > $(LOGFILE))
	-grep "Failed Tests" $(LOGFILE) >> $(LOG)
	-grep "Unexpected Errors" $(LOGFILE) >> $(LOG)

runtests: create_db run_tests

clean:
	gnat clean -XPRJ_Build=$(MODE) -Pdb_test
	-$(RM) test/*.res test/db_harness.log.*
