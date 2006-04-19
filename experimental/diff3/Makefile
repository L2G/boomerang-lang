####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 413 2005-06-07 19:18:34Z bcpierce $

TOP = ../..

all: makeharmonylib 

include $(TOP)/Top.Makefile

test:: 
	$(MAKE) -C $(SRCDIR)
	$(HARMONY) -diff3 -check Diff3 -test-all

include $(OCAMLMAKEFILE)


