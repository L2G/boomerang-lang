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

checkin: logmsg remembernews
	svn commit --file logmsg
	$(RM) logmsg

remembernews: logmsg
	echo "CHANGES FROM VERSION" $(VERSION) > /tmp/ChangeLog.tmp
	echo "\$$Revision$$" $(shell date) > .version
	echo >> /tmp/ChangeLog.tmp
	cat logmsg >> /tmp/ChangeLog.tmp
	echo >> /tmp/ChangeLog.tmp
	echo "-------------------------------" >> /tmp/ChangeLog.tmp
	-cat ChangeLog >> /tmp/ChangeLog.tmp
	mv -f /tmp/ChangeLog.tmp ChangeLog

