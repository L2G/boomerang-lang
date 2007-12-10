####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOOLSDIR=$(shell pwd)/../../common/tools
SRC2TEX =$(TOOLSDIR)/src2tex

FOCALDIR = $(shell pwd)/..
SRCDIR = $(FOCALDIR)/src
LENSESDIR = $(FOCALDIR)/lenses

FCLFILES = $(shell (cd $(LENSESDIR); ls *.fcl))

SRCFILES += $(wildcard *.src) $(shell (cd $(SRCDIR); ls *.src*))

GENERATEDTEXFILES = $(patsubst %.src,tmp/%.tex, \
                    $(patsubst %.srcl,tmp/%.tex, \
                    $(patsubst %.srcy,tmp/%.tex, $(SRCFILES)))) \
                    $(patsubst %.fcl,tmp/%.tex, $(FCLFILES))

all: main.pdf main.ps

main.pdf: $(GENERATEDTEXFILES)
	@$(MAKE) pdf

pdf:
	pdflatex main
	-bibtex main
	pdflatex main
	pdflatex main

main.ps: $(GENERATEDTEXFILES)
	@$(MAKE) ps

ps:
	latex main
	-bibtex main
	dvips -tletter -omain.ps main

$(SRC2TEX):
	omake $(SRC2TEX)

tmp/%.tex : $(LENSESDIR)/%.fcl $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp/%.tex : $(SRCDIR)/%.src $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp/%.tex : $(SRCDIR)/%.srcl $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp/%.tex : $(SRCDIR)/%.srcy $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp/%.tex : %.src $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp:
	-mkdir tmp

clean::
	rm -rf tmp *.log *.pdf *.ps *.dvi *.aux temp.tex *.toc 


