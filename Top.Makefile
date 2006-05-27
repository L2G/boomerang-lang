####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common Makefile infrastructure			           #
####################################################################

# $Id$

default: all

####################################################################
# Configure Path Locations

CWD = $(shell pwd)
FULLTOP = $(CWD)/$(TOP)

LENSESDIR = $(FULLTOP)/lenses
EXAMPLESDIR = $(FULLTOP)/examples
TOOLSDIR = $(FULLTOP)/tools
EXTERNDIR = $(FULLTOP)/extern
SRCDIR = $(FULLTOP)/src
DOCDIR = $(FULLTOP)/doc

ALLSUBDIRS = $(shell find * -type d -print -prune)

# This can be overriden by editing the following line, 
# or by including "INSTALLDIR=<path>" on the command line
INSTALLDIR = $(HOME)/bin

####################################################################
# Configure Harmony and some standard tools

LENSPATH = -I $(LENSESDIR) -I $(CWD)
HARMONYBIN = $(SRCDIR)/harmony 
HARMONY = $(HARMONYBIN) $(HARMONYFLAGS) $(LENSPATH)

$(HARMONYBIN):
	$(MAKE) -C $(SRCDIR)

SRC2F = $(TOOLSDIR)/src2f
SRC2TEX = $(TOOLSDIR)/src2tex

####################################################################
# Configure OCamlMakefile

OCAMLFIND_COMMANDS=$(foreach c,ocamlc ocamlopt ocamlcp ocamldoc ocamldep,$(c)=$(c).opt)

OCAMLMAKEFILE = $(SRCDIR)/OCamlMakefile

PACKS = "netstring,unix,str,pxp,pxp-engine,pxp-lex-utf8" 
YFLAGS = -v 
OCAMLFLAGS = -dtypes -rectypes
OCAMLCPFLAGS = a
OCAMLLDFLAGS = -cc g++

LIBS=csv omega

LIBDIRS = $(EXTERNDIR)/ocaml-csv-1.0.3/ \
          $(EXTERNDIR)/omega/ocaml/

INCDIRS = $(EXTERNDIR)/ocaml-csv-1.0.3 \
          $(EXTERNDIR)/omega/ocaml

$(SRCDIR)/harmony.cmxa: 
	$(MAKE) -C $(SRCDIR) native-code-library

makeharmonylib:
	$(MAKE) -C $(SRCDIR) native-code-library

install: all
	$(QUIET)$(MAKE) justinstall

justinstall: 
	$(QUIET)if [ ! -z $(RESULT) ]; then echo cd $(CWD); echo cp  $(RESULT) $(INSTALLDIR); mkdir -p $(INSTALLDIR); cp $(RESULT) $(RESULT-HELPERS) $(INSTALLDIR)/; fi 
	@for i in $(SUBDIRS); do \
	   $(MAKE) -C $$i justinstall;  \
	   if [ $$? -ne 0 ]; then exit $$?; fi; \
	done

####################################################################
# Generating .fcl and .tex files from .src

FCLFILES = $(shell (cd $(LENSESDIR); ls *.fcl))

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

buildsubdirs:
	@for i in $(SUBDIRS); do \
	    echo "###### Building $(CWD)/$$i ######"; \
	    $(MAKE) -C $$i; done


tidy::
	rm -rf *.tmp *.bak .*.bak .bak.* *~ temp.* *.annot 
	rm -rf r1.* r2.* ar.*

cleanall:
	$(MAKE) -C $(TOP) clean

test:: $(HARMONYBIN) $(GENERATEDFCLFILES) 
	@for i in $(SUBDIRS); do \
	   echo ;\
	   echo "###### testing $(CWD)/$$i ######"; \
	   $(MAKE) -C $$i test;  \
	   if [ $$? -ne 0 ]; then exit $$?; fi; \
	done

buildharmony: 
	@$(MAKE) -C $(SRCDIR) all
	@$(MAKE) -C $(LENSESDIR) all

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
