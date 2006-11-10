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

CP=cp -p

all:
	gnat make -XPRJ_Build=$(MODE) -Pweb

setup:
	-mkdir gen
	templates2ada -d templates -o gen/templates.cds \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds src/
	rm -f gen/templates.cds
	templates2ada -d templates -o gen/templates.cds -e .txml \
		-t templates/templates.tads
	gnat chop -wpq gen/templates.cds gen/
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
	$(CP) css/*.css $(INSTALL)/css
	$(CP) templates/*.*html $(INSTALL)/templates
	$(CP) templates/*.txml $(INSTALL)/templates
	$(CP) we_js/*js $(INSTALL)/we_js
	$(CP) bin/vision2pixels$(EXEXT) $(INSTALL)/bin

clean:
	-rm -fr gen src/v2p-template_defs*
	-gnat clean -r -Pweb
