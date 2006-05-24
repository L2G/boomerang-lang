####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ../..
include $(TOP)/Top.Makefile

RESULT = harmonize-listexamples

all: $(RESULT)

$(RESULT):
	ln -s $(TOP)/src/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

test::
	$(HARMONY) -check Cycleexamples -test-all -cycle
	$(HARMONY) -check Listexamples -test-all


