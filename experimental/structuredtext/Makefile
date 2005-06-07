####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ../..
include $(TOP)/Top.Makefile

all: test

test::
	$(HARMONY) check -module Structuredtext -test-all

DEMOFILES = a.txt b.txt o.txt

clean::
	rm -rf $(DEMOFILES)

demo:: 
	@$(MAKE) buildharmony
	@if [ -f a.txt ] && [ -f b.txt ]; then $(MAKE) continue-demo; else $(MAKE) init-demo; fi

init-demo:
	-rm -f $(DEMOFILES)
	echo "* HEADER" >> a.txt
	echo "** SUBHEADER" >> a.txt
	echo "text" >> a.txt
	echo "** SUBHEADER" >> a.txt
	echo "more text" >> a.txt
	cp a.txt b.txt
	$(MAKE) continue-demo
	echo
	echo Demo initialized: You can now edit a.txt and/or b.txt and 
	echo then do 'make demo' to synchronize them.

continue-demo:
	$(HARMONY) sync -schema Structuredtext.NestedListOfValues \
	   -archive o.txt  -lensar Structuredtext.star_headers \
	   -replica1 a.txt  -lensr1 Structuredtext.star_headers \
	   -replica2 b.txt  -lensr2 Structuredtext.star_headers \
	   -newarchive o.txt -newreplica1 a.txt -newreplica2 b.txt \
	   -debug harmony

