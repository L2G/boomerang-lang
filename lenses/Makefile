####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
include $(TOP)/Top.Makefile

clean::
	rm -f $(GENERATEDFCLFILES)

all: $(GENERATEDFCLFILES)

test:: $(GENERATEDFCLFILES)
	$(HARMONY) check -module Xml -test-all 
	$(HARMONY) check -module Spinelist -test-all 
	$(HARMONY) check -module Relational -test-all 

doc: $(GENERATEDFCLFILES) buildharmony
	$(HARMONY) check -module Relational -test-all 
	make -C $(DOCDIR) ps