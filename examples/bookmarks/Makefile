####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile
MOZSRC = "/Users/nate/Library/Application Support/Firefox/Profiles/03gr5tt5.default/bookmarks.html"
SAFSRC = "/Users/nate/Library/Safari/bookmarks.plist"

all: test

moz2xml: moz2xml.mll
	ocamllex moz2xml.mll
	ocamlopt -o moz2xml moz2xml.ml

xml2moz: xml2moz.mll
	ocamllex xml2moz.mll
	ocamlopt -o xml2moz xml2moz.ml

$(MOZSRC):

$(SAFSRC):

moz.xml: moz2xml $(MOZSRC)
	./moz2xml < $(MOZSRC) > moz.xml

moz.meta: common.fcl mozilla.fcl moz.xml
	$(HARMONY) get -l Mozilla.l -c moz.xml -o moz.meta

moz2.xml: common.fcl mozilla.fcl moz2.meta
	$(HARMONY) put -l Mozilla.l -a moz2.meta -c moz.xml -o moz2.xml

moz3.xml: xml2moz moz2.xml
	./xml2moz < moz2.xml > moz3.xml

update-moz: xml2moz moz3.xml
	cat moz3.xml > $(MOZSRC)

saf.xml: $(SAFSRC)
	plutil -convert xml1 -o saf.xml $(SAFSRC)

saf.meta: common.fcl plist.fcl safari.fcl saf.xml
	$(HARMONY) get -l Safari.l2 -c saf.xml -o saf.meta

saf2.xml: common.fcl plist.fcl safari.fcl saf2.meta
	$(HARMONY) put -l Safari.l2 -a saf2.meta -c saf.xml -o saf2.xml

update-saf: saf2.xml
	plutil -convert binary1 -o $(SAFSRC) saf2.xml

sync: 
	$(HARMONY) sync \
          -s Common.Contents \
          -ar archive.meta -la Prelude.id \
          -r1 saf.meta  -l1 Prelude.id \
          -r2 moz.meta  -l2 Prelude.id \
	  -newar archive2.meta \
	  -newr1 saf2.meta \
	  -newr2 moz2.meta \

test:: common.fcl mozilla.fcl plist.fcl safari.fcl
	$(HARMONY) check -module Mozilla -test-all
	$(HARMONY) check -module Safari -test-all

temp: common.fcl mozilla.fcl plist.fcl safari.fcl
	time $(HARMONY) get -lens Safari.l -concrete Bookmarks.plist:xml -output out.tmp:meta

clean::
	rm -rf moz.xml moz.meta moz2.xml moz3.xml xml2moz xml2moz.ml moz2xml moz2xml.ml saf.xml saf.meta saf2.xml *.fcl archive.meta archive2.meta moz2.meta saf2.meta 
	cp .archive.meta archive.meta


