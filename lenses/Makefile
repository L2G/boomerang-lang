####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
include $(TOP)/Top.Makefile

clean::
	rm -f $(GENERATEDFCLFILES)

test::
	$(HARMONY) check -module Relational -test-all

all: $(GENERATEDFCLFILES)

test::
	$(HARMONY) check -module Spinelist -test-all 
	$(HARMONY) check -module Relational -test-all 
