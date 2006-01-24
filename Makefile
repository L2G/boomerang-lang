####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

SUBDIRS = src lenses examples tools doc
SUBDIRSCLEANONLY = experimental visual papers extern

TOP = .
include $(TOP)/Top.Makefile

all: buildsubdirs

WEBDIR = $(HOME)/html/
DOWNLOADDIR=$(WEBDIR)/download

###########################################################################
## Tarball export - to be run by harmony@halfdome.cis.upenn.edu

EXPORTNAME=harmony-$(shell date "+20%y%m%d")
TMP=/tmp
TMPDIR=$(TMP)/$(EXPORTNAME)

tar: 
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) -C $(DOCDIR) main.pdf
	rm -rf $(TMPDIR)
	(cd $(TMP); svn export file:///mnt/saul/plclub1/svnroot/harmony/trunk $(EXPORTNAME))
	cp $(DOCDIR)/main.pdf $(TMPDIR)/doc/manual.pdf
	(cd $(TMP); tar zcvf $(EXPORTNAME).tar.gz $(EXPORTNAME))
	mv $(TMP)/$(EXPORTNAME).tar.gz $(DOWNLOADDIR)

###########################################################################
## Web Install - to be run by harmony@halfdome.cis.upenn.edu
web:
	echo \\draftfalse > $(DOCDOR)/temp.tex
	$(MAKE) all
	cp $(DOCDIR)/main.pdf $(DOCDIR)/doc/manual.pdf
	$(MAKE) -C html
	cp -r html/* doc $(WEBDIR)
	cp -r src lenses examples extern $(WEBDIR)/cgi-bin/
	chmod -R 755 $(WEBDIR)

export:
	$(MAKE) tar
	$(MAKE) web