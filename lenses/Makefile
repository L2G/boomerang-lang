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
	$(HARMONY) -check Prelude -test Prelude 
	$(HARMONY) -check Units -test Units
	$(HARMONY) -check List -test List
	$(HARMONY) -check Xml -test Xml
	$(HARMONY) -check Plist -test Plist
	$(HARMONY) -check Relational -test Relational

doc: $(GENERATEDFCLFILES) buildharmony
	$(HARMONY) check -module Xml -test Xml
	make -C $(DOCDIR) ps