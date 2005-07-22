####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
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

##############################################################################


DEMO1 = R1ORIG=SafariExample.plist  R2ORIG=empty.meta
DEMO2 = R1ORIG=MozillaExample.html  R2ORIG=empty.meta
DEMO3 = R1ORIG=MozillaExample.html  R2ORIG=empty.plist

# Not working right now for silly reasons (should be fixed)
DEMO4 = R1ORIG=SafariExample.plist R2ORIG=empty.html 

DEMOCMD = ./$(RESULT) $(HARMONYFLAGS) $(LENSPATH) -compress=false

##################################################
##################################################
##################################################
##################################################
##################################################
## MOVE to Top.Makefile

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

