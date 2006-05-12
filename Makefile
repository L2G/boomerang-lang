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
checkin: logmsg remembernews
	svn commit --file logmsg
	$(RM) logmsg

VERSION=$(shell Sed .version -e "s/[^R]*R[^:]*: //" -e "s/ .*//")
DATE=$(shell date)
remembernews: logmsg
	echo "CHANGES FROM VERSION" $(VERSION) > /tmp/ChangeLog.tmp
	echo "\$$Revision$$" $(shell date) > .version
	echo >> /tmp/ChangeLog.tmp
	cat logmsg >> /tmp/ChangeLog.tmp
	echo >> /tmp/ChangeLog.tmp
	echo "-------------------------------" >> /tmp/ChangeLog.tmp
	-cat ChangeLog >> /tmp/ChangeLog.tmp
	mv -f /tmp/ChangeLog.tmp ChangeLog

###########################################################################
## Tarball export - to be run by harmony@halfdome.cis.upenn.edu
WEBDIR = $(HOME)/html/
DOWNLOADDIR=$(WEBDIR)/download

ifdef HARMONY_BUILD_TAG
REAL_HARMONY_BUILD_TAG=-$(HARMONY_BUILD_TAG)
else
REAL_HARMONY_BUILD_TAG=
endif

EXPORTNAME=harmony$(REAL_HARMONY_BUILD_TAG)-$(shell date "+20%y%m%d")
EXPORTDIR=/tmp/$(EXPORTNAME)

export: 
	rm -rf $(EXPORTDIR)
	mkdir $(EXPORTDIR)
	(cd $(EXPORTDIR)/..; svn export --force file:///mnt/saul/plclub1/svnroot/harmony/trunk $(EXPORTNAME))
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) -C $(DOCDIR) main.pdf
	cp $(DOCDIR)/main.pdf $(EXPORTDIR)/doc/manual.pdf
	(cd $(EXPORTDIR)/..; tar zcvf $(EXPORTNAME).tar.gz $(EXPORTNAME))
	rm -f $(DOWNLOADDIR)/harmony-nightly*.tar.gz
	mv $(EXPORTDIR)/../$(EXPORTNAME).tar.gz $(DOWNLOADDIR)
	rm -fr $(EXPORTDIR)
	$(MAKE) all
	cp $(DOCDIR)/main.pdf $(DOCDIR)/manual.pdf
	$(MAKE) -C html
	cp -r html/* doc $(WEBDIR)
	cp -r src lenses examples extern $(WEBDIR)/cgi-bin/
	chmod -R 755 $(WEBDIR)
