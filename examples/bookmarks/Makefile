####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile

all: test

test:: common.fcl mozilla.fcl plist.fcl safari.fcl
	$(HARMONY) check -module Mozilla -test-all
	$(HARMONY) check -module Safari -test-all



