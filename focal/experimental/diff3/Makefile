####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ../..

all: makeharmonylib 

include $(TOP)/Top.Makefile

test:: 
	$(MAKE) -C $(SRCDIR)
	$(HARMONY) -diff3 -check Diff3 -test-all

include $(OCAMLMAKEFILE)


