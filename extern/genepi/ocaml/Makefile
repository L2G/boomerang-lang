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

GENEPIDIR=$(EXTERNDIR)/genepi
BUILDDIR=$(GENEPIDIR)/build
THISDIR=$(GENEPIDIR)/ocaml

#########################
# C++ compiler settings
CC = gcc
CFLAGS= -O3

all: interface.o

#########################
#include paths
GENEPI_INCLUDES=-I $(BUILDDIR)/include/genepi
OCAML_INCLUDES=-I $(shell ocamlc -where)
INCLUDES=$(GENEPI_INCLUDES) $(OCAML_INCLUDES)

COMMON_LIBS= -linterface -L$(THISDIR) \
             -lomega -L$(OMEGADIR)/omega_lib/obj \
             -lstdc++

##################################################
# libinterface.a:
#    a few additional object files needed to 
#    link against libomega.a, plus the actual
#    C interface
##################################################

interface.o: interface.cpp
	$(CC) $(CFLAGS) -c $< -o $@ $(INCLUDES)

$(THISDIR)/libinterface.a: interface.o
	$(AR) cr $@ interface.o

##################################################
# utility targets
##################################################
clean::
	rm -f core *.o *.cmo *.cma *.cmi *.a *.cmxa *.cmx
