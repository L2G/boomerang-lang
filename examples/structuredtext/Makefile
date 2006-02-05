####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 413 2005-06-07 19:18:34Z bcpierce $

TOP = ../..

DEMO1 = R1ORIG=demo1r1.txt        R2FORMAT=txt     HARMONYFLAGS=-simplified   
DEMO2 = R1ORIG=demo2r1.txt        R2FORMAT=meta    HARMONYFLAGS=-simplified   
DEMO3 = R1ORIG=example3.txt       R2FORMAT=meta    
DEMO4 = R1ORIG=example4.txt       R2FORMAT=txt
DEMO5 = R1ORIG=example5.txt       R2FORMAT=txt
DEMO6 = R1ORIG=example6.txt       R2FORMAT=txt

# Just for testing:
DEMO9 = R1ORIG=demo1r1.txt       

all: makeharmonylib nc

# Testing a bug -- nuke later
temp: 
	harmonize-structuredtext -ar test-tasks.txt -r1 test-unisonmerge1-tasks.txt -r2 test-unisonmerge2-tasks.txt -newar foo.tmp -newr1 bar.tmp -newr2 baz.tmp

test::
	$(HARMONY) -check Structuredtext -test-all

include $(TOP)/Top.Makefile

RESULT = harmonize-structuredtext
SOURCES = harmonize-structuredtext.ml 
LIBS += harmony

include $(OCAMLMAKEFILE)


