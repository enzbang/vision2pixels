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

# Targets

all:
	gnat make -XPRJ_Build=$(MODE) -Pdb_test

setup install:

setup_tests:
	(cd data; rm -f testing.db; ./create_database.sh)

run_tests:
	(cd test; ./db_harness)
	echo "***** OK - no regression on module DB"

runtests: setup_tests run_tests

clean:
	gnat clean -XPRJ_Build=$(MODE) -Pdb_test
	-$(RM) test/*.res
