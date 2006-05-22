####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$ 

TOP = ../..
include $(TOP)/Top.Makefile

RESULT = harmonize-relational

all: $(RESULT)

$(RESULT):
	ln -s $(TOP)/src/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

test:: 
	$(HARMONY) -check Database -test-all



