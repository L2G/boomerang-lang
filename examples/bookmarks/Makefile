####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile

moz2xml: moz2xml.mll
	ocamllex moz2xml.mll
	ocamlopt -o moz2xml moz2xml.ml

xml2moz: xml2moz.mll
	ocamllex xml2moz.mll
	ocamlopt -o xml2moz xml2moz.ml

test:: $(LOCALGENERATEDFCLFILES)
	$(HARMONY) -check Mozilla -test-all
	$(HARMONY) -check Safari -test-all

temp: $(LOCALGENERATEDFCLFILES)
	time $(HARMONY) get -lens Safari.l -concrete Bookmarks.plist:xml -output out.tmp:meta

tidy:
	rm -rf moz.xml moz.meta moz2.xml moz3.xml xml2moz xml2moz.ml moz2xml moz2xml.ml saf.xml saf.meta saf2.xml *.fcl archive.meta archive2.meta moz2.meta saf2.meta 
	rm -rf temp*
	rm -rf .*.bak .*.tmp *.cmo *.cmi *.cmx ._d .harmonyar* ._bcdi $(RESULT) *.annot

clean:: tidy



##############################################################################

RESULT = harmonize-bookmarks
SOURCES = harmonize-bookmarks.ml 
LIBS += harmony

all: makeharmonylib nc

include $(OCAMLMAKEFILE)

test-script: all $(LOCALGENERATEDFCLFILES) moz2xml xml2moz 
	if [ ! -e temp1.html ]; then cp MozillaExample.html temp1.html; fi
	if [ ! -e temp2.html ]; then cp MozillaExample.html temp2.html; fi
	./harmonize-bookmarks temp1.html temp2.html $(HARMONYFLAGS) $(LENSPATH) -debug startup

test-script-safari: $(LOCALGENERATEDFCLFILES) moz2xml xml2moz harmonize-bookmarks
	if [ ! -e temp1.plist ]; then cp SafariExample.plist temp1.plist; fi
	if [ ! -e temp2.plist ]; then cp SafariExample.plist temp2.plist; fi
	./harmonize-bookmarks temp1.plist temp2.plist $(HARMONYFLAGS) $(LENSPATH) -debug startup

