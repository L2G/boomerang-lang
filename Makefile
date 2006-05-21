####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

SUBDIRS = src lenses examples tools doc
SUBDIRSCLEANONLY = experimental visual papers extern html

TOP = .
include $(TOP)/Top.Makefile

all: buildsubdirs

###########################################################################
## SVN Commit -- append msg to ChangeLog
VERSION=$(shell sed .version -e "s/[^R]*R[^:]*: //" -e "s/ .*//")
DATE=$(shell date)

