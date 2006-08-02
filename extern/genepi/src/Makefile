####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################
# $Id$

#########################
# include top-level Harmony Makefile,
# setup some useful directory definitions
TOP=../../..
include $(TOP)/Top.Makefile

LIB_SRC=ccl-1-1 genepi-1-0  prestaf-1-2  prestaf2fast-1-0  sataf-1-1 mona2fast-1-0
LIB_BUILDDIRS=$(foreach d, $(LIB_SRC), $(d)/build)

GENEPIDIR=$(EXTERNDIR)/genepi
BUILDDIR=$(GENEPIDIR)/build

all: 
	$(MAKE) -C $(GENEPIDIR)/ocaml

clean::
	rm -fr $(LIB_BUILDDIRS)
