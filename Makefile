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
## Export

EXPORTNAME=harmony-$(shell date "+20%y%m%d")
TMPDIR=/tmp
TMP=$(TMPDIR)/$(EXPORTNAME)
DOWNLOADDIR=$(HOME)/pub/harmony/download
HARMONYUSER?=$(USER)

export: 
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) -C $(DOCDIR) pdf
	rm -rf $(TMPDIR)/$(EXPORTNAME)
	(cd $(TMPDIR); svn export svn+ssh://$(HARMONYUSER)@halfdome.cis.upenn.edu/mnt/saul/plclub1/svnroot/harmony4 $(EXPORTNAME))
	rm -rf $(TMP)/Personal.Makefile
	rm -rf $(TMP)/papers
	rm -rf $(TMP)/talks
	rm -rf $(TMP)/old
	rm -rf $(TMP)/notes
	rm -rf $(TMP)/readings
	rm -rf $(TMP)/TODO.txt
	cp $(DOCDIR)/main.pdf $(TMP)/doc/manual.pdf
	(cd $(TMPDIR); tar cvf - $(EXPORTNAME) \
           | gzip --force --best > $(EXPORTNAME).tar.gz)
	mv $(TMPDIR)/$(EXPORTNAME).tar.gz $(DOWNLOADDIR)
