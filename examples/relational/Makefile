####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile

RESULT = harmonize-relational

all: $(RESULT)

$(RESULT):
	ln -s $(SRCDIR)/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

test:: 
	$(HARMONY) -check Database -test-all



