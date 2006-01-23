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

###########################################################################
## Tarball Export

WEBDIR = $(HOME)/html/
DOWNLOADDIR=$(WEBDIR)/download
TMPDIR=/tmp

EXPORTNAME=harmony-$(shell date "+20%y%m%d")
TMP=$(TMPDIR)/$(EXPORTNAME)
HARMONYUSER?=$(USER)

export: 
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) -C $(DOCDIR) main.pdf
	rm -rf $(TMPDIR)/$(EXPORTNAME)
	(cd $(TMPDIR); svn export svn+ssh://$(HARMONYUSER)@halfdome.cis.upenn.edu/mnt/saul/plclub1/svnroot/harmony/trunk $(EXPORTNAME))
	cp $(DOCDIR)/main.pdf $(TMP)/doc/manual.pdf
	(cd $(TMPDIR); tar cvf - $(EXPORTNAME) \
           | gzip --force --best > $(EXPORTNAME).tar.gz)
	mv $(TMPDIR)/$(EXPORTNAME).tar.gz $(DOWNLOADDIR)

###########################################################################
## Web Install

web:
	$(MAKE) all
	$(MAKE) -C html
	cp -r html/* $(WEBDIR)
	cp -r src lenses examples doc extern $(WEBDIR)/cgi-bin/
	chmod -R 755 $(WEBDIR)
