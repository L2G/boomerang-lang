####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ../..

DEMO1 = R1ORIG=archive.xmi        R2FORMAT=xmi

all: makeharmonylib nc

test::
	$(HARMONY) -check Xmi -test-all
	$(HARMONY) -check XmiUml -test-all

include $(TOP)/Top.Makefile

RESULT = harmonize-xmi
SOURCES = harmonize-xmi.ml 
LIBS += harmony

include $(OCAMLMAKEFILE)