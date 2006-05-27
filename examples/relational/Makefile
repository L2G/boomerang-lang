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
	ln -s $(SRCDIR)/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

test:: 
	$(HARMONY) -check Database -test Database



