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


##############################################################################
## Boilerplate for running demos

QUIET=@

ifneq (,$(DEMO1))
  DEMOS += demo1
endif
ifneq (,$(DEMO2))
  DEMOS += demo2
endif
ifneq (,$(DEMO3))
  DEMOS += demo3
endif
ifneq (,$(DEMO4))
  DEMOS += demo4
endif
ifneq (,$(DEMO5))
  DEMOS += demo5
endif
ifneq (,$(DEMO6))
  DEMOS += demo6
endif
ifneq (,$(DEMO7))
  DEMOS += demo7
endif
ifneq (,$(DEMO8))
  DEMOS += demo8
endif
ifneq (,$(DEMO9))
  DEMOS += demo9
endif

MAKEDEMO = $(MAKE) R1ORIG=$(R1ORIG) R2ORIG=$(R2ORIG) 

# Default (can be overridden if extra flags are needed, e.g.)
DEMOCMD = ./$(RESULT) $(HARMONYFLAGS) $(LENSPATH)

demo%:
	$(QUIET)$(MAKE) init-demo $(DEMO$*)
	$(QUIET)$(MAKEDEMO) demo $(DEMO$*)

export R1=r1$(suffix $(R1ORIG))
export R2=r2$(suffix $(R2ORIG))

init-demo: all
	$(QUIET)echo "Copying $(R1ORIG) to $(R1) and $(R2ORIG) to $(R2)"
	$(QUIET)rm -rf $(R1) $(R2) ar.meta
	$(QUIET)cp $(R1ORIG) $(R1)
	$(QUIET)if [ -f $(R2) ]; then cp $(R2ORIG) $(R2); fi

demo: 
	$(QUIET)if [ ! -e $(R1) ]; then $(MAKEDEMO) demo1; exit 1; fi
	$(DEMOCMD) $(R1) $(R2) -ar ar.meta 
	$(QUIET)if [ -z $(EDITOR) ]; then \
	   echo ; \
	   echo 'Please edit $(R1) and/or $(R2) and do "make demo"...'; \
         else \
	   $(EDITOR) $(R1) $(R2); \
	   $(MAKEDEMO) demo; \
         fi

########

DEMOTESTS = $(subst demo,test,$(DEMOS))

test:: 
	$(QUIET)if [ -n "$(DEMOTESTS)" ]; then $(MAKEDEMO) $(DEMOTESTS); fi

test%:
	$(MAKE) init-demo $(DEMO$*)
	$(MAKE) run-test $(DEMO$*)

run-test:
	$(DEMOCMD) $(R1) $(R2) -ar ar.meta 


####################################################################
# Personalization

# Load file of individual harmony flags from home dir (if present)

-include $(HOME)/.harmony.mk

