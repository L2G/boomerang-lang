####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$ 

TOP = ../..
include $(TOP)/Top.Makefile

all: test

test:: 
	$(HARMONY) -check Main -test-all

get:
	$(HARMONY) test.xml -lensr1 Main.l 

sync:
	$(HARMONY) -r1 r1.xml -lensr1 Main.l \
	           -r2 r2.xml -lensr2 Main.l \
	           -ar archive.xml -lensar Main.l \
	           -newr1 r1.xml \
	           -newr2 r2.xml \
	           -newar archive.xml \
		   -schema Prelude.Any

