####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
SUBDIRSCLEANONLY = ubase iCalendar
include $(TOP)/Top.Makefile

SRC = $(shell pwd)

RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLLDFLAGS = -linkpkg

OCAMLLEX = ocamllex -ml

all: $(EXTERNDIR)/ocaml-csv-1.0.3/csv.cma

$(EXTERNDIR)/ocaml-csv-1.0.3/csv.cma:
	$(MAKE) -C $(EXTERNDIR)/ocaml-csv-1.0.3

include $(SRC)/Common.Makefile

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
	rm -rf lenses doc