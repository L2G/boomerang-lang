####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common Makefile infrastructure			           #
####################################################################

# $Id: Common.Makefile 121 2005-05-05 00:19:32Z bcpierce $

default: all

clean::
	rm -rf *.tmp *.aux *.bbl *.blg *.log *.dvi *.bak *~ temp.* TAGS *.cmo *.cmi *.cmx
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done

test::
	for i in $(SUBDIRS); do $(MAKE) -C $$i test; done
