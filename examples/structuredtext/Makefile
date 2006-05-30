####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

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

temp1: 
	make -C ../../src
	-rm harmonize-structuredtext harmonize-structuredtext.cmx
	make
	./harmonize-structuredtext -ar TODO-o.txt -r1 TODO-a.txt -r2 TODO-b.txt -newar TODO-o.txt.tmp -newr1 TODO-a.txt.tmp -newr2 TODO-b.txt.tmp

temp2:
	make -C ../../src
	-rm harmonize-structuredtext harmonize-structuredtext.cmx
	make
	harmonize-structuredtext -ar temp-o.txt -r1 temp-a.txt -r2 temp-b.txt -newar temp-o.txt.tmp -newr1 temp-a.txt.tmp -newr2 temp-b.txt.tmp -debug all

test::
	$(HARMONY) -check Structuredtext -test-all

include $(TOP)/Top.Makefile

RESULT = harmonize-structuredtext
SOURCES = harmonize-structuredtext.ml 
LIBS += harmony

include $(OCAMLMAKEFILE)


