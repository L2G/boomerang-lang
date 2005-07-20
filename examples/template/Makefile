####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile

all: test

test:: 
	$(HARMONY) -check Main -test-all

get:
	$(HARMONY) get -lens Main.l -concrete test.xml -output temp.meta
	cat temp.meta

sync:
	$(HARMONY) sync -replica1 r1.xml -lensr1 Main.l \
	                -replica2 r2.xml -lensr2 Main.l \
	                -archive archive.xml -lensar Main.l \
	                -newreplica1 r1.xml \
	                -newreplica2 r2.xml \
	                -newarchive archive.xml \
		        -schema Prelude.Any

