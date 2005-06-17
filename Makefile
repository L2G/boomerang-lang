####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

SUBDIRS = src lenses examples tools doc
SUBDIRSCLEANONLY = experimental visual papers

TOP = .
include $(TOP)/Top.Makefile

all: test


###########################################################################
## Export

TMPDIR=/tmp
TMP=$(TMPDIR)/harmony
EXPORTNAME=harmony-prerelease
DOWNLOADDIR=$(HOME)/pub/harmony/download

export: 
	echo \\draftfalse > $(DOCDIR)/temp.tex
	$(MAKE) -C $(DOCDIR) pdf
	rm -rf $(TMPDIR)/$(EXPORTNAME)
	(cd $(TMPDIR); svn export svn+ssh://bcpierce@halfdome.cis.upenn.edu/mnt/saul/plclub1/svnroot/harmony4 $(EXPORTNAME))
	rm -rf $(TMP)/Personal.Makefile
	rm -rf $(TMP)/papers
	rm -rf $(TMP)/talks
	rm -rf $(TMP)/old
	rm -rf $(TMP)/notes
	rm -rf $(TMP)/readings
	cp $(DOCDIR)/main.pdf $(TMP)/manual.pdf
	(cd $(TMPDIR); tar cvf - $(EXPORTNAME) \
           | gzip --force --best > $(EXPORTNAME).tar.gz)
	mv $(TMPDIR)/$(EXPORTNAME).tar.gz $(DOWNLOADDIR)
