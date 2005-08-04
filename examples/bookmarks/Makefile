####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..

DEMO1 = R1ORIG=SafariExample.plist  R2ORIG=empty.meta
DEMO2 = R1ORIG=MozillaExample.html  R2ORIG=empty.meta
DEMO3 = R1ORIG=MozillaExample.html  R2ORIG=empty.plist
DEMO4 = R1ORIG=SafariExample.plist R2ORIG=empty.html 

DEMOCMD = ./$(RESULT) $(HARMONYFLAGS) $(LENSPATH) -compress=false

include $(TOP)/Top.Makefile

RESULT = harmonize-bookmarks
SOURCES = harmonize-bookmarks.ml 
LIBS += harmony

all: makeharmonylib nc xml2moz moz2xml

include $(OCAMLMAKEFILE)

moz2xml: moz2xml.mll
	ocamllex moz2xml.mll
	ocamlopt -o moz2xml moz2xml.ml

xml2moz: xml2moz.mll
	ocamllex xml2moz.mll
	ocamlopt -o xml2moz xml2moz.ml

test:: $(LOCALGENERATEDFCLFILES)
	$(HARMONY) -check Mozilla -test-all
	$(HARMONY) -check Safari -test-all

tidy::
	rm -rf moz.xml moz.meta moz2.xml moz3.xml xml2moz xml2moz.ml moz2xml moz2xml.ml saf.xml saf.meta saf2.xml *.fcl archive.meta archive2.meta moz2.meta saf2.meta 
	rm -rf temp*
	rm -rf .*.bak .*.tmp *.cmo *.cmi *.cmx ._d .harmonyar* ._bcdi $(RESULT) *.annot

