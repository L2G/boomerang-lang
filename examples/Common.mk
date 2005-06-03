# This is the common makefile for all the Harmony examples

all: test

buildandtest: build test

build:
	make -C $(HARMONYSRC)

rebuild:
	make -C $(HARMONYSRC) clean all

clean::
	rm -f *.tmp junk.* *.cmo *.cmi *~
