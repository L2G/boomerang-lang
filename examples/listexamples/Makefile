####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 413 2005-06-07 19:18:34Z bcpierce $

TOP = ../..
include $(TOP)/Top.Makefile

RESULT = harmonize-listexamples

all: $(RESULT)

$(RESULT):
	ln -s $(TOP)/src/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

test::
	$(HARMONY) -check Listexamples -test-all


