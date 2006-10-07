
# Simple makefile to build Vision2Pixels

all:
	gnat make -Pweb/web

clean:
	gnat clean -r -Pweb/web
