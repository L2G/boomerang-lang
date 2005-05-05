# This is the common makefile for all the Harmony examples

TOP = ../..

HARMONYSRC = $(TOP)/src

LENSLIB = $(TOP)/src/lib/lenses
LENSPATH = -I $(LENSLIB) -I .

HARMONY = $(HARMONYSRC)/harmony $(LENSPATH)

all: test

buildandtest: build test

build:
	make -C $(HARMONYSRC)

clean::
	rm -f *.tmp junk.* *~