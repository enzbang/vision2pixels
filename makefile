
# Simple makefile to build Vision2Pixels

INSTALL=$(HOME)/opt/v2p

ifeq ($(OS),Windows_NT)
EXEXT=.exe
else
EXEXT=
endif

OPTIONS = INSTALL="$(INSTALL)" EXEXT="$(EXEXT)"

all: setup
	gnat make -XPRJ_Build=Debug -Pweb/web

setup:
	make -C web setup

install:
	make -C web install $(OPTIONS)

clean:
	gnat clean -r -Pweb/web
