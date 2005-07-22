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
	$(HARMONY) -check Prelude -test-all 
	$(HARMONY) -check List -test-all 
	$(HARMONY) -check Xml -test-all 
	$(HARMONY) -check Spinelist -test-all 
	$(HARMONY) -check Relational -test-all 
	$(HARMONY) -check Units -test-all

units:
	$(HARMONY) -check Units -test-all

doc: $(GENERATEDFCLFILES) buildharmony
	$(HARMONY) check -module Xml -test-all 
	make -C $(DOCDIR) ps