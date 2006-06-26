####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################
# $Id$

#########################
# include top-level Harmony Makefile,
# setup some useful directory definitions
TOP=../..
include $(TOP)/Top.Makefile
SUBDIRS = ocaml
SUBDIRSCLEANONLY = src

GENEPIDIR=$(EXTERNDIR)/genepi
BUILDDIR=$(GENEPIDIR)/build

all: buildsubdirs

clean::
	rm -fr $(BUILDDIR)