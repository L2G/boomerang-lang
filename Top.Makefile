####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common Makefile infrastructure			           #
####################################################################

# $Id$

default: all

####################################################################
# Navigation

CWD = $(shell pwd)
FULLTOP = $(CWD)/$(TOP)

LENSESDIR = $(FULLTOP)/lenses
TOOLSDIR = $(FULLTOP)/tools
EXTERNDIR = $(FULLTOP)/extern
SRCDIR = $(FULLTOP)/src
DOCDIR = $(FULLTOP)/doc

ALLSUBDIRS = $(shell find * -type d -print -prune)

SRC2F = $(TOOLSDIR)/src2f
SRC2TEX = $(TOOLSDIR)/src2tex


####################################################################
# Setup for running harmony 

LENSPATH = -I $(LENSESDIR) -I $(CWD)
HARMONYBIN = $(SRCDIR)/harmony 
HARMONY = $(HARMONYBIN) $(HARMONYFLAGS) $(LENSPATH)

$(HARMONYBIN):
	$(MAKE) -C $(SRCDIR)


####################################################################
# OCaml compilation

OCAMLMAKEFILE = $(SRCDIR)/OCamlMakefile

PACKS = "netstring,unix,str,pxp,pxp-engine,pxp-lex-utf8" 
YFLAGS = -v 
OCAMLFLAGS = -dtypes
OCAMLCPFLAGS = f

LIBS += csv 

LIBDIRS = $(SRCDIR) $(SRCDIR)/ubase $(EXTERNDIR)/ocaml-csv-1.0.3
INCDIRS = $(SRCDIR) $(SRCDIR)/ubase $(EXTERNDIR)/ocaml-csv-1.0.3
export BCPDEPFLAGS = -I $(SRCDIR) -I $(SRCDIR)/ubase $(EXTERNDIR)/ocaml-csv-1.0.3

# Tell ocamlfind to use .opt versions of OCaml tools
OCAMLFIND_COMMANDS=$(foreach c,ocamlc ocamlopt ocamlcp ocamldoc ocamldep,$(c)=$(c).opt)

$(SRCDIR)/harmony.cmxa: 
	$(MAKE) -C $(SRCDIR) native-code-library

makeharmonylib:
	$(MAKE) -C $(SRCDIR) native-code-library


####################################################################
# Generating .fcl and .tex files from .src

SRCFILES = $(shell (cd $(LENSESDIR); ls *.src))
GENERATEDFCLFILES = $(subst .src,.fcl, $(SRCFILES:%=$(LENSESDIR)/%))

LOCALSRCFILES = $(shell (ls *.src))
LOCALGENERATEDFCLFILES = $(subst .src,.fcl, $(LOCALSRCFILES))

%.fcl : %.src $(SRC2F)
	-rm -f $@
	$(SRC2F) $< $@
	chmod -w $@

%.mly: %.src $(SRC2F)
	-rm -f $@
	$(SRC2F) $< $@
	chmod -w $@

%.mll: %.src $(SRC2F)
	-rm -f $@
	$(SRC2F) $< $@
	chmod -w $@

$(SRC2F):
	$(MAKE) -C $(TOOLSDIR)

$(SRC2TEX):
	$(MAKE) -C $(TOOLSDIR)


####################################################################
# Common targets

clean:: tidy
	rm -rf *.aux *.bbl *.blg *.log *.dvi TAGS *.cmo *.cmi *.cmx *.o 
	@for i in $(SUBDIRS) $(SUBDIRSCLEANONLY); do \
	    echo "###### cleaning $(CWD)/$$i ######"; \
	    $(MAKE) -C $$i clean; done

tidy::
	rm -rf *.tmp *.bak .*.bak .bak.* *~ temp.* *.annot 
	rm -rf r1.* r2.* ar.*

cleanall:
	$(MAKE) -C $(TOP) clean

test:: $(HARMONYBIN) $(GENERATEDFCLFILES) 
	@for i in $(SUBDIRS); do \
	   echo \
	   echo "###### testing $(CWD)/$$i ######"; \
	   $(MAKE) -C $$i test;  \
	   if [ $$? -ne 0 ]; then exit $$?; fi; \
	done

buildharmony: 
	@$(MAKE) -C $(SRCDIR) all
	@$(MAKE) -C $(LENSESDIR) all

t: buildharmony test

updateall:
	cd $(TOP); svn update

.PHONY: tags

buildtags:
	$(MAKE) clean
	(cd $(TOP); \
	 etags `find .  -name "*.src" \
	           -or -name "*.fcl" \
	           -or -name "*.ml" \
	           -or -name "*.mli" \
	           -or -name "*Makefile*" | egrep -v ".svn|./old|./papers|./talks"`)


####################################################################
# Personalization

# Load file of individual harmony flags from home dir (if present)

-include $(HOME)/.harmony.mk

