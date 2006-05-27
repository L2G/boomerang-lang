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

include ../Makefile.config

OMEGADIR=$(EXTERNDIR)/omega
THISDIR=$(OMEGADIR)/ocaml

all: omega.cma omega.cmxa

#########################
# C++ compiler settings
CFLAGS= $(OPTIMIZATION_CFLAGS)

#########################
#include paths
OMEGA_INCLUDES=-I $(OMEGADIR)/omega_lib/include/ -I $(OMEGADIR)/basic/include/
OCAML_INCLUDES=-I $(shell ocamlc -where)
INCLUDES=$(OMEGA_INCLUDES) $(OCAML_INCLUDES)

COMMON_DEPS=$(OMEGADIR)/omega_lib/obj/libomega.a $(THISDIR)/libinterface.a

COMMON_LIBS= -linterface -L$(THISDIR) \
             -lomega -L$(OMEGADIR)/omega_lib/obj \
             -lstdc++

#########################
# main targets
omega.cma: $(COMMON_DEPS) omegaLibrary.cmo
	ocamlmklib -custom -o omega omegaLibrary.cmo $(COMMON_LIBS)

omega.cmxa: $(COMMON_DEPS) omegaLibrary.cmx
	ocamlmklib -o omega omegaLibrary.cmx $(COMMON_LIBS)

#########################
# ocaml modules
omegaLibrary.cmo: omegaLibrary.ml
	ocamlc -c omegaLibrary.ml

omegaLibrary.cmx: omegaLibrary.ml
	ocamlopt -c omegaLibrary.ml

##################################################
# libinterface.a:
#    a few additional object files needed to 
#    link against libomega.a, plus the actual
#    C interface
##################################################

ConstString.o: $(OMEGADIR)/basic/src/ConstString.c
	$(CXX) $(CFLAGS) -c $< -o $@ $(INCLUDES) 

Exit.o: $(OMEGADIR)/basic/src/Exit.c
	$(CXX) $(CFLAGS) -c $< -o $@ $(INCLUDES) 

interface.o: interface.cpp
	$(CXX) $(CFLAGS) -c $< -o $@ $(INCLUDES) 

$(THISDIR)/libinterface.a: ConstString.o Exit.o interface.o
	$(AR) cr $@ ConstString.o Exit.o interface.o

#########################
# libomega.a

$(OMEGADIR)/omega_lib/obj/libomega.a: 
	$(MAKE) -C $(OMEGADIR)/omega_lib/obj

##################################################
# utility targets
##################################################
clean::
	rm -f core *.o *.cmo *.cma *.cmi *.a *.cmxa *.cmx
