####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
SUBDIRSCLEANONLY = ubase iCalendar
include $(TOP)/Top.Makefile

RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLLDFLAGS = -linkpkg

# Why did we have this?  The ocaml manual says it is not recommended!
# OCAMLLEX = ocamllex -ml

include $(SRCDIR)/Common.Makefile

test:: all

######################################################################
# Documentation generation for Harmony sources

DOCDIR = doc
READINORDER = name.mli v.mli lens.mli surveyor.mli sync.mli harmony.mli
ALLMLIS = $(wildcard *.mli)

temp: 
	echo 

docs: 
	-mkdir $(DOCDIR)
	ocamldoc -d $(DOCDIR) -html $(READINORDER) $(filter-out $(READINORDER) parser.mli metay.mli, $(wildcard *.mli))

clean::
	rm -rf lenses doc parser.mly lexer.mll