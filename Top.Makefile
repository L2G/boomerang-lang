####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common Makefile infrastructure			           #
####################################################################

# $Id: Common.Makefile 121 2005-05-05 00:19:32Z bcpierce $

####################################################################
# Navigation

LENSESDIR = $(TOP)/lenses
TOOLSDIR = $(TOP)/tools

SRC2F = $(TOOLSDIR)/src2f
SRC2TEX = $(TOOLSDIR)/src2tex

####################################################################
# Common targets

default: all

clean::
	rm -rf *.tmp *.aux *.bbl *.blg *.log *.dvi *.bak *~ temp.* TAGS *.cmo *.cmi *.cmx *.o *.annot 
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done

test::
	for i in $(SUBDIRS); do $(MAKE) -C $$i test; done
