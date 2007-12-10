####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$ 

SUBDIRS = iCalendar ocaml-csv-1.0.3 genepi

clean:
	rm -rf *.aux *.bbl *.blg *.log *.dvi TAGS *.cmo *.cmi *.cmx *.o 
	@for i in $(SUBDIRS); do \
	    $(MAKE) -C $$i clean || exit $$?; done

all:

