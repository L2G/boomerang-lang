####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 413 2005-06-07 19:18:34Z bcpierce $

TOP = ../..
include $(TOP)/Top.Makefile

all: makeharmonylib nc

include $(OCAMLMAKEFILE)

test::
	$(HARMONY) -check Structuredtext -test-all

RESULT = harmonize-structuredtext
SOURCES = harmonize-structuredtext.ml 
LIBS += harmony


##############################################################################
## Demos

DEMO1 = R1ORIG=example1.txt       R2FORMAT=txt     HARMONYFLAGS=-simplified
DEMO2 = R1ORIG=example2.txt       R2FORMAT=meta    HARMONYFLAGS=-simplified
DEMO3 = R1ORIG=example3.txt       R2FORMAT=meta    
DEMO4 = R1ORIG=example4.txt       R2FORMAT=txt
DEMO5 = R1ORIG=example5.txt       R2FORMAT=txt
DEMO6 = R1ORIG=example6.txt       R2FORMAT=txt


