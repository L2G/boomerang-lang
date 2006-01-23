####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
include $(TOP)/Top.Makefile

SRCFILES += $(wildcard *.src) $(shell (cd $(SRCDIR); ls *.src))

GENERATEDTEXFILES = $(patsubst %.src,tmp/%.tex, $(SRCFILES)) \
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

tmp/%.tex : $(LENSESDIR)/%.fcl $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp/%.tex : $(SRCDIR)/%.src $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp/%.tex : %.src $(SRC2TEX) tmp
	$(SRC2TEX) $< > $@

tmp:
	-mkdir tmp

clean::
	rm -rf tmp *.log *.pdf *.ps *.dvi *.aux temp.tex *.toc 


