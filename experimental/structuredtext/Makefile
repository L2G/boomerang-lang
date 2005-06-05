####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile

all: test

test::
	$(HARMONY) check -module Structuredtext -test-all
	$(HARMONY) get -concrete eg1.txt:blob -lens Structuredtext.from_blob -output junk.tmp:meta && cat junk.tmp
