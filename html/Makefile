####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP=../
include $(TOP)/Top.Makefile 

all: index.html related.html

WEBDIR=$(HOME)/html
CGIDIR=$(WEBDIR)/cgi-bin
CGIURL=http://alliance.seas.upenn.edu/~harmony/cgi-bin/

####################################################################
# BibTeX2HTML Configuration
B2HFLAGS = -dl \
	   -s ./bold-title.bst \
           --nodoc -nofooter -noheader -noabstract -nokeys -nokeywords\
           --named-field slides slides \
           --named-field full 'full version' \
           --named-field short 'short version' \
           --named-field prelim 'preliminary version' \
           --named-field conf 'conference version' \
           --named-field official 'official version' \
           --named-field homepage 'home page' \
           --named-field manuscript 'manuscript' \
           --named-field TR 'tech report' \
           --named-field errata 'errata' \
           --named-field sources 'source code' \
           --reverse-sort --sort-by-date

bcp.bib:
	wget -O bcp.bib http://www.cis.upenn.edu/~bcpierce/papers/bcp.bib

jnf.bib: 
	wget -O jnf.bib http://www.cis.upenn.edu/~jnfoster/papers/jnf.bib

harmony.bib: bcp.bib jnf.bib 
	cat bcp.bib jnf.bib > harmony.bib

papers.html: harmony.bib Makefile
	bib2bib -ob /dev/null -oc harmony.keys -c 'keys:"'harmony'"' harmony.bib
	bibtex2html -o papers $(B2HFLAGS) -citefile harmony.keys harmony.bib

####################################################################
# Convert PHP to HTML

%.html: %.php header.php footer.php
	cp header.php footer.php $(CGIDIR)
	cp $< $(CGIDIR)/tmp-$<
	chmod 755 $(CGIDIR)/tmp-$<
	wget -O $@ $(CGIURL)/tmp-$<
	rm $(CGIDIR)/tmp-$< $(CGIDIR)/header.php $(CGIDIR)/footer.php

###
# FIXME: want index.html to also depend on $(WEBDIR)/download
index.html: index.php header.php footer.php papers.html
	cp index.php $(CGIDIR)/tmp-index.php
	cp header.php footer.php papers.html $(CGIDIR)
	chmod 755 $(CGIDIR)/tmp-index.php
	wget -O index.html $(CGIURL)/tmp-index.php
	rm $(CGIDIR)/tmp-index.php $(CGIDIR)/papers.html $(CGIDIR)/header.php $(CGIDIR)/footer.php


clean::
	rm -f bcp.bib jnf.bib harmony.bib harmony.keys *.html

