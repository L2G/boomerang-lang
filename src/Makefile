####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
SUBDIRSCLEANONLY = ubase
include $(TOP)/Top.Makefile

SRC = $(shell pwd)

RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLLDFLAGS = -linkpkg

DOCDIR = doc

OCAMLLEX = ocamllex -ml

include $(SRC)/Common.Makefile

test:: all

######################################################################
# Documentation generation for Harmony sources

READINORDER = name.mli v.mli lens.mli surveyor.mli sync.mli harmony.mli

ALLMLIS = $(wildcard *.mli)

temp: 
	echo 

docs: 
	-mkdir $(DOCDIR)
	ocamldoc -d $(DOCDIR) -html $(READINORDER) $(filter-out $(READINORDER) parser.mli metay.mli, $(wildcard *.mli))

clean::
	rm -rf lenses doc