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

LENSESDIR = $(CWD)/$(TOP)/lenses
TOOLSDIR = $(TOP)/tools
EXTERNDIR = $(CWD)/$(TOP)/extern
SRCDIR = $(TOP)/src
DOCDIR = $(TOP)/doc

ALLSUBDIRS = $(shell find * -type d -print -prune)

SRC2F = $(TOOLSDIR)/src2f
SRC2TEX = $(TOOLSDIR)/src2tex

####################################################################
# Setup for running harmony 

LENSPATH = -I $(LENSESDIR) -I $(CWD)
HARMONYBIN = $(SRCDIR)/harmony 
HARMONY = $(HARMONYBIN) $(HARMONYFLAGS) $(LENSPATH)

HARMONYBIN = $(SRCDIR)/harmony 

$(HARMONYBIN):
	$(MAKE) -C $(SRCDIR)

####################################################################
# Generating .fcl and .tex files from .src

SRCFILES = $(shell (cd $(LENSESDIR); ls *.src))
GENERATEDFCLFILES = $(subst .src,.fcl, $(SRCFILES:%=$(LENSESDIR)/%))

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

clean::
	rm -rf *.tmp *.aux *.bbl *.blg *.log *.dvi *.bak *~ temp.* TAGS *.cmo *.cmi *.cmx *.o *.annot 
	@for i in $(SUBDIRS) $(SUBDIRSCLEANONLY); do \
	    echo "###### cleaning $(CWD)/$$i ######"; \
	    $(MAKE) -C $$i clean; done

test:: $(HARMONYBIN) $(GENERATEDFCLFILES) 
	@for i in $(SUBDIRS); do \
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

