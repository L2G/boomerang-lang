
all: index.html download.html related.html

CGI=$(HOME)/html/cgi-bin

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

%.html: %.php header.php footer.php
	cp header.php footer.php $(CGI)
	cp $< $(CGI)/tmp-$<
	chmod 755 $(CGI)/tmp-$<
	wget -O $@ http://alliance.seas.upenn.edu/~harmony/cgi-bin/tmp-$<
	rm $(CGI)/tmp-$< $(CGI)/header.php $(CGI)/footer.php

index.html: index.php header.php footer.php papers.html
	cp index.php $(CGI)/tmp-index.php
	cp header.php footer.php papers.html $(HOME)/html/cgi-bin/
	chmod 755 $(CGI)/tmp-index.php
	wget -O index.html http://alliance.seas.upenn.edu/~harmony/cgi-bin/tmp-index.php
	rm $(CGI)/tmp-index.php $(CGI)/papers.html $(CGI)/header.php $(CGI)/footer.php

TRASH := $(TRASH) bcp.bib jnf.bib harmony.bib harmony.keys *.html

clean:
	rm -f $(TRASH)