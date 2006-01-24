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

harmony-papers.html: bcp.bib jnf.bib 
	bib2bib -ob harmony-papers.bib -oc harmony-papers.keys\
          -c '(keys:"harmony") & (($$type="INPROCEEDINGS") | ($$type="ARTICLE"))' $?
	bibtex2html -o harmony-papers $(B2HFLAGS) -citefile harmony-papers.keys harmony-papers.bib

harmony-talks.html: bcp.bib jnf.bib
	bib2bib -ob harmony-talks.bib -oc harmony-talks.keys\
          -c '(keys:"harmony") & (note:"talk\|keynote")' $?
	bibtex2html -o harmony-talks $(B2HFLAGS) -citefile harmony-talks.keys harmony-talks.bib

harmony-misc.html: bcp.bib jnf.bib 
	bib2bib -ob harmony-misc.bib -oc harmony-misc.keys -c 'keys:"'harmony'"' \
          -c '(keys:"harmony") & ($$type <> "INPROCEEDINGS") & ($$type <> "ARTICLE") & & !(note:talk\|keynote)' $?
	bibtex2html -o harmony-misc $(B2HFLAGS) -citefile harmony-misc.keys harmony-misc.bib

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
index.html: index.php header.php footer.php harmony-papers.html harmony-talks.html armony-misc.html
	cp $? $(CGIDIR)/
	chmod 755 $(CGIDIR)/index.php
	wget -O $? $(CGIURL)/index.php
	rm $(CGIDIR)/$?


clean::
	rm -f *.bib *.keys *.html

