####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 1747 2006-05-27 23:16:06Z kkunal $

TOP = ../..
include $(TOP)/Top.Makefile

RESULT = harmonize-relational

all: $(RESULT)

$(RESULT):
	ln -s $(TOP)/src/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

test::
	$(HARMONY) -check Relationaltests -test-all


