
# Simple makefile to build Vision2Pixels

all: setup
	gnat make -XPRJ_Build=Debug -Pweb/web

setup:
	make -C web setup

clean:
	gnat clean -r -Pweb/web
