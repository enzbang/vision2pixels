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

# Options

INSTALL=$(HOME)/opt/v2p
MODE=Debug

ifeq ($(OS),Windows_NT)
EXEXT=.exe
else
EXEXT=
endif

OPTIONS = INSTALL="$(INSTALL)" EXEXT="$(EXEXT)" MODE="$(MODE)"

# Modules support

MODULES = web

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_CLEAN = ${MODULES:%=%_clean}

MODULES_INSTALL = ${MODULES:%=%_install}

# Targets

all: $(MODULES_SETUP) $(MODULES_BUILD)

setup: $(MODULES_SETUP)

install: $(MODULES_INSTALL)

clean: $(MODULES_CLEAN)

${MODULES_BUILD}:
	${MAKE} -C ${@:%_build=%} $(OPTIONS)

${MODULES_SETUP}:
	${MAKE} -C ${@:%_setup=%} setup $(OPTIONS)

${MODULES_CLEAN}:
	${MAKE} -C ${@:%_clean=%} clean $(OPTIONS)

${MODULES_INSTALL}:
	${MAKE} -C ${@:%_install=%} install $(OPTIONS)
