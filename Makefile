###########################################################################
#                              Vision2Pixels
#
#                         Copyright (C) 2006-2009
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

all::

# Root dir
MODE              = Release
JOBS              = 1
BUILD_DIR         = ${shell pwd}/.build
GNATMAKE_OPTIONS  = -XPRJ_BUILD=$(MODE)
VALGRIND          =
TAR_DIR           = tar czf

LMODE             = $(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])


# Install prefix
GNAT_ROOT            = $(dir $(shell which gnatls))..
DIOUZHTU_DYNAMIC_LIB = $(GNAT_ROOT)/share/diouzhtu/dlib
ARGWIAD_ROOT         = $(shell echo $$ARGWIAD_ROOT)
prefix	             = $(shell cat $(BUILD_DIR)/gnat.root 2>/dev/null)

# Add GPR Library_Kind for AWS (force to shared)
GNATMAKE_OPTIONS += -XLIBRARY_TYPE=relocatable

GNAT              = gnat
GNATMAKE          = $(GNAT) make -p -j$(JOBS) $(GNATMAKE_OPTIONS)
GNATCLEAN         = $(GNAT) clean $(GNATMAKE_OPTIONS)
GNATCHECK         = $(GNAT) check $(GNATMAKE_OPTIONS) -rules -from=v2p.check
TEMPLATES2ADA     = templates2ada
RM                = rm -f
LN                = ln
CP                = cp -p
MKDIR             = mkdir -p

#  Version

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

uname_M := $(shell sh -c 'uname -m 2>/dev/null || echo not')

LIBRARIES =

ifeq (${OS},Windows_NT)
	LIBEXT = .dll
	EXEEXT = .exe
	DISTRIB_OS = win32-$(uname_M)
	LIBRARIES += ext_lib
else
	LIBEXT = .so
	uname_S       := $(shell sh -c 'uname -s 2>/dev/null || echo not')
	uname_S_lower := $(shell sh -c \
		'uname -s 2>/dev/null \
			| tr [[:upper:]] [[:lower:]] || echo not')

	DISTRIB_OS  = $(uname_S_lower)-$(uname_M)
endif
DISTRIB = $(shell pwd)/v2p-$(DISTRIB_OS)-$(VERSION_ALL)

LOG := ${shell pwd}/log.${shell date +%Y%m%d-%H%M%S}

LIBRARIES += web_lib

GPR =
GPR += web/web
GPR += image/image
GPR += image/image_test
GPR += web/web_test
GPR += db/db
GPR += db/db_test

BLD_GPR := $(addprefix bld-, $(GPR))
CLN_GPR := $(addprefix cln-, $(GPR))
CHK_GPR := $(addprefix chk-, $(GPR))

$(BLD_GPR): bld-% :
	$(GNATMAKE) -P$*

$(CLN_GPR): cln-% :
	$(GNATCLEAN) -P$*

$(CHK_GPR): chk-% :
	$(GNATCHECK) -P$*

all:: mkdirs kernel/src/v2p-version.ads $(LIBRARIES)

clean:: $(CLN_GPR)
check:: $(CHK_GPR)

kernel/src/v2p-version.ads:
#  If git is not present then use the version.ads provided in distrib
ifneq ("$(VERSION)", "")
	sed -e 's,\$$VERSION\$$,$(VERSION),g' \
	-e 's,\$$VERSION_ALL\$$,$(VERSION_ALL),g' \
	kernel/src/v2p-version.tads > kernel/src/v2p-version.ads
endif

LOG_SUFFIX := log.${shell date +%Y%m%d-%H%M%S}
LOG          = $(BUILD_DIR)/log.$(LOG_SUFFIX)
IMAGE_TEST_LOGFILE = $(BUILD_DIR)/image_harness.log.$(LOG_SUFFIX)
WEB_TEST_LOGFILE = $(BUILD_DIR)/web_harness.log.$(LOG_SUFFIX)
DB_TEST_LOGFILE = $(BUILD_DIR)/db_harness.log.$(LOG_SUFFIX)

WEB_TESTDIR = $(BUILD_DIR)/web_testdir
WEB_TESTPLUGIN  = $(WEB_TESTDIR)/plugins/vision2pixels

regtests: mkdirs regtests_db_internal \
	regtests_image_internal regtests_web_internal
	@if [ `grep ": 0" $(LOG) | wc -l` = 6 ]; then \
	   echo "=====>>>>>> Ok, all tests have passed"; \
	else \
	   echo "=====>>>>>> NOk, some tests have failed"; \
	   cat $(LOG) | grep -v ': 0'; \
	fi;

regtests_image_internal: bld-image/image_test
	# Image tests
	-(cd image/test; ./image_harness > $(IMAGE_TEST_LOGFILE))
	@echo " * Image regtests" >> $(LOG)
	@echo "     report file is $(IMAGE_TEST_LOGFILE)" >> $(LOG)
	@-grep "Failed Assertions" $(IMAGE_TEST_LOGFILE) >> $(LOG)
	@-grep "Unexpected Errors" $(IMAGE_TEST_LOGFILE) >> $(LOG)

regtests_image: regtests_image_internal
	@cat $(IMAGE_TEST_LOGFILE)

regtests_db_internal: bld-db/db_test
# DB tests
# For tests we need to rebuild the database in order to have
# post in 'today view'
	@touch db/data/schema-sqlite.sql
	@$(MAKE) --quiet install_gwiad_plugin \
		DBNAME=testing.db ARGWIAD_ROOT=$(WEB_TESTDIR)
	@$(CP) $(WEB_TESTDIR)/plugins/vision2pixels/db/testing.db \
		$(BUILD_DIR)/db_test/obj/
	-(cd $(BUILD_DIR)/db_test/obj/; \
		./db_harness$(EXEEXT) > $(DB_TEST_LOGFILE))
	@echo " * Database regtests" >> $(LOG)
	@echo "     report file is $(DB_TEST_LOGFILE)" >> $(LOG)
	@-grep "Failed Assertions" $(DB_TEST_LOGFILE) >> $(LOG)
	@-grep "Unexpected Errors" $(DB_TEST_LOGFILE) >> $(LOG)

regtests_db: regtests_db_internal
	@cat $(DB_TEST_LOGFILE)

regtests_web_internal: bld-web/web_test
	# Web test
	$(MKDIR) $(WEB_TESTPLUGIN)/db
	echo "server_port 8042" > $(WEB_TESTDIR)/aws.ini
	echo "upload_directory uploads" >> $(WEB_TESTDIR)/aws.ini
	echo "db_name db/testing.db" > $(WEB_TESTPLUGIN)/v2p.ini
	$(CP) image/test/troll.jpg $(WEB_TESTDIR)
	$(CP) $(BUILD_DIR)/web/test/bin/web_harness$(EXEEXT)\
	       	$(WEB_TESTDIR)/bin/
	-(cd $(WEB_TESTDIR); bin/web_harness$(EXEEXT) > $(WEB_TEST_LOGFILE))
	@echo " * Web regtests" >> $(LOG)
	@echo "     report file is $(WEB_TEST_LOGFILE)" >> $(LOG)
	-grep "Failed Assertions" $(WEB_TEST_LOGFILE) >> $(LOG)
	-grep "Unexpected Errors" $(WEB_TEST_LOGFILE) >> $(LOG)

regtests_web: regtests_web_internal
	@cat $(WEB_TEST_LOGFILE)

mkdirs:
	$(MKDIR) $(BUILD_DIR)/web/gen
	$(MKDIR) $(BUILD_DIR)/web/tsrc
	$(MKDIR) $(WEB_TESTPLUGIN)

$(BUILD_DIR)/web/tsrc/v2p-template_defs.adb::
	(cd web && $(TEMPLATES2ADA) -d templates/ -o \
	       	$(BUILD_DIR)/web/gen/templates.cds \
		-t templates/templates.tads)
	$(GNAT) chop -wpq $(BUILD_DIR)/web/gen/templates.cds \
	       	$(BUILD_DIR)/web/tsrc
	$(RM) $(BUILD_DIR)/web/gen/templates.cds
	(cd web && $(TEMPLATES2ADA) -d templates/ -o \
		$(BUILD_DIR)/web/gen/templates.cds \
		-e .txml -t templates/templates.tads)
	$(GNAT) chop -wpq $(BUILD_DIR)/web/gen/templates.cds \
		$(BUILD_DIR)/web/gen/
	$(RM) $(BUILD_DIR)/web/gen/templates.cds
	$(CP) $(BUILD_DIR)/web/gen/v2p-template_defs-* $(BUILD_DIR)/web/tsrc
	$(RM) $(BUILD_DIR)/web/gen/*
	(cd web && $(TEMPLATES2ADA) -d templates/ -o \
		$(BUILD_DIR)/web/gen/templates.cds \
		-e .tmplt -t templates/templates.tads)
	$(GNAT) chop -wpq $(BUILD_DIR)/web/gen/templates.cds \
		$(BUILD_DIR)/web/gen/
	$(RM) $(BUILD_DIR)/web/gen/templates.cds
	$(CP) $(BUILD_DIR)/web/gen/v2p-template_defs-* $(BUILD_DIR)/web/tsrc
	$(RM) $(BUILD_DIR)/web/gen/*
	(cd web && $(TEMPLATES2ADA) -d templates/ -o \
		$(BUILD_DIR)/web/gen/templates.cds \
		-e .incl -t templates/templates.tads)
	$(GNAT) chop -wpq $(BUILD_DIR)/web/gen/templates.cds \
		$(BUILD_DIR)/web/gen/
	$(CP) $(BUILD_DIR)/web/gen/v2p-template_defs-* $(BUILD_DIR)/web/tsrc
	$(RM) -r $(BUILD_DIR)/gen

web_lib:: $(BUILD_DIR)/web/tsrc/v2p-template_defs.adb bld-web/web

DBNAME = testing.db
install_gwiad_plugin:: install_dirs db/data/$(DBNAME) web/tools/wmaint$(EXEEXT)
	$(CP) db/data/$(DBNAME) $(ARGWIAD_ROOT)/plugins/vision2pixels/db
	$(CP) web/tools/wmaint$(EXEXT) $(ARGWIAD_ROOT)/bin

install_dirs:
	@if test ! "$(ARGWIAD_ROOT)"; then \
		echo "NO ARGWIAD_ROOT ?" 1>&2; \
		exit 1; \
	fi
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/db
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/templates/
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/xml
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/web_data
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/we_js
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/css
	$(MKDIR) $(ARGWIAD_ROOT)/plugins/vision2pixels/css/img
	$(MKDIR) $(ARGWIAD_ROOT)/lib/websites
	$(MKDIR) $(ARGWIAD_ROOT)/lib/services
	$(MKDIR) $(ARGWIAD_ROOT)/bin
	$(MKDIR) $(ARGWIAD_ROOT)/uploads
	$(CP) -r web/templates/*.tmplt \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/templates/
	$(CP) -r web/templates/*.thtml \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/templates/
	$(CP) -r web/templates/*.txml \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/templates/
	$(CP) -r web/templates/*.incl \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/templates/
	$(CP) -r web/xml/*xml \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/xml/
	$(CP) -r web/we_js/*js \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/we_js/
	$(CP) -r web/css/*css \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/css/
	$(CP) -r web/css/img/* \
		$(ARGWIAD_ROOT)/plugins/vision2pixels/css/img/
	$(CP) -f $(BUILD_DIR)/web/lib/*$(LIBEXT) $(ARGWIAD_ROOT)/lib/websites
	$(CP) image/lib/*$(LIBEXT) $(ARGWIAD_ROOT)/bin
	$(CP) kernel/lib/*$(LIBEXT) $(ARGWIAD_ROOT)/bin
	-$(CP) lib/gnadelite/.build/$(LMODE)/lib/*$(LIBEXT) $(ARGWIAD_ROOT)/bin
	$(CP) -f $(DIOUZHTU_DYNAMIC_LIB)/*wiki_service$(LIBEXT) \
		$(ARGWIAD_ROOT)/lib/services

web/tools/wmaint$(EXEEXT):
	$(GNATMAKE) -Pweb/tools -gnat05

db/data/v2p.db: db/data/schema-sqlite.sql
	(cd db/data/; ./create_database.sh)

db/data/testing.db: db/data/schema-sqlite.sql db/data/initial-test-data.sql \
  db/data/vote_ponderated.sql db/data/vote.sql
	(cd db/data; ./create_test_database.sh)

install-distrib:
	-$(RM) -r $(DISTRIB)
	$(MAKE) install_gwiad_plugin ARGWIAD_ROOT=$(DISTRIB)/dist
	(cd $(DISTRIB)/dist; $(TAR_DIR) ../dist.tgz .)
	$(RM) -r $(DISTRIB)/dist
	$(CP) releases/do-install.sh $(DISTRIB)
	$(CP) releases/adduser.sh $(DISTRIB)
	$(TAR_DIR) $(shell basename $(DISTRIB)).tgz \
		$(shell basename $(DISTRIB))
	$(RM) -r $(DISTRIB)

ext_lib:
	$(MAKE) -C image/lib_ext

.PHONY: all install clean regtests regtests_image regtests_web regtests_db
.PHONY: web/tools/wmaint$(EXEEXT)
.PHONY: kernel/src/v2p-version.ads
