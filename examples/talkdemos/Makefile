####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ../..
include $(TOP)/Top.Makefile

RESULT = harmonize-talkdemos

all: $(RESULT)

$(RESULT):
	ln -s $(TOP)/src/harmony $(RESULT)

clean::
	rm -rf $(RESULT)

bcp:
	$(HARMONY) -check Test -test-all
