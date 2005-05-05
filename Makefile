####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Top-level Makefile					           #
####################################################################

# $Id: Common.Makefile 121 2005-05-05 00:19:32Z bcpierce $

all: test

test:
	$(MAKE) -C src test
	$(MAKE) -C examples test

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
