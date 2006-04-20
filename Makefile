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

WEBDIR = $(HOME)/html/
DOWNLOADDIR=$(WEBDIR)/download

###########################################################################
## Tarball export - to be run by harmony@halfdome.cis.upenn.edu

EXPORTNAME=harmony-$(shell date "+20%y%m%d")
EXPORTDIR=/tmp/$(EXPORTNAME)

tar: 
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) -C $(DOCDIR) main.pdf
	rm -rf $(EXPORTDIR)
	mkdir $(EXPORTDIR)
	(cd $(EXPORTDIR)/..; svn export --force file:///mnt/saul/plclub1/svnroot/harmony/trunk $(EXPORTNAME))
	cp $(DOCDIR)/main.pdf $(EXPORTDIR)/doc/manual.pdf
	(cd $(EXPORTDIR)/..; tar zcvf $(EXPORTNAME).tar.gz $(EXPORTNAME))
	mv $(EXPORTDIR)/../$(EXPORTNAME).tar.gz $(DOWNLOADDIR)

###########################################################################
## Web Install - to be run by harmony@halfdome.cis.upenn.edu
web:
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) all
	cp $(DOCDIR)/main.pdf $(DOCDIR)/manual.pdf
	$(MAKE) -C html
	cp -r html/* doc $(WEBDIR)
	cp -r src lenses examples extern $(WEBDIR)/cgi-bin/
	chmod -R 755 $(WEBDIR)

export:
	$(MAKE) tar
	$(MAKE) web
