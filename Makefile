####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Common.Makefile 121 2005-05-05 00:19:32Z bcpierce $

SUBDIRS = src lenses examples papers tools 
SUBDIRSCLEANONLY = experimental visual

TOP = .
include $(TOP)/Top.Makefile

all: 
	$(MAKE) -C src all



