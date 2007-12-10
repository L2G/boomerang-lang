####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################
# $Id$

GENEPIDIR=$(shell pwd)/..
BUILDDIR=$(GENEPIDIR)/build
THISDIR=$(GENEPIDIR)/ocaml

#########################
# C compiler settings
CC = gcc
CFLAGS= -O3

all: $(THISDIR)/genepi.cma $(THISDIR)/genepi.cmxa

##
# C Libraries
## 
#GENEPI_LIBS=$(foreach l,genepi ccl sataf prestaf prestaf2fast,$(BUILDDIR)/lib/lib$(l).a) 
GENEPI_LIBS=$(foreach l,genepi mona2fast,$(BUILDDIR)/lib/lib$(l).a) 

$(BUILDDIR)/lib/libgenepi.a:
	(cd $(GENEPIDIR)/src/genepi-1-0 && \
         ./do-build --make-install --install-dir $(BUILDDIR) && \
         ranlib $@)

$(BUILDDIR)/lib/libccl.a:
	(cd $(GENEPIDIR)/src/ccl-1-1 && \
         ./do-build --make-install --install-dir $(BUILDDIR) && \
         ranlib $@)

$(BUILDDIR)/lib/libsataf.a:
	(cd $(GENEPIDIR)/src/sataf-1-1 && \
         ./do-build --make-install --install-dir $(BUILDDIR) && \
         ranlib $@)

$(BUILDDIR)/lib/libprestaf.a:
	(cd $(GENEPIDIR)/src/prestaf-1-2 && \
         ./do-build --make-install --install-dir $(BUILDDIR) && \
         ranlib $@)

$(BUILDDIR)/lib/libprestaf2fast.a:
	(cd $(GENEPIDIR)/src/prestaf2fast-1-0 && \
         ./do-build --make-install --install-dir $(BUILDDIR))

$(BUILDDIR)/lib/libmona2fast.a:
	(cd $(GENEPIDIR)/src/mona2fast-1-0 && \
         ./do-build --make-install --install-dir $(BUILDDIR))

##################################################
# libgenepi-interface.a:
#    a few additional object files needed to 
#    link against libomega.a, plus the actual
#    C interface
##################################################

GENEPI_INCLUDES=-I $(BUILDDIR)/include -I $(BUILDDIR)/include/genepi
OCAML_INCLUDES=-I $(shell ocamlc -where)
INCLUDES=$(GENEPI_INCLUDES) $(OCAML_INCLUDES)

interface.o: interface.c
	$(CC) $(CFLAGS) -c $< -o $@ $(INCLUDES)

$(THISDIR)/libgenepi-interface.a: interface.o
	$(AR) cr $@ interface.o

#########################
#include paths
COMMON_DEPS=$(GENEPI_LIBS) $(THISDIR)/libgenepi-interface.a

#             -lprestaf2fast -lprestaf -lsataf -lccl \
#             -lmona2fast -ldfa -lbdd -lmem \

COMMON_LIBS= -lgenepi-interface \
             -lmona2fast -ldfa -lbdd -lmem \
             -lgenepi \
             -L$(THISDIR) -L$(BUILDDIR)/lib

genepiLibrary.cmo: genepiLibrary.ml
	ocamlc -c genepiLibrary.ml

$(THISDIR)/genepi.cma: $(COMMON_DEPS) genepiLibrary.cmo
	ocamlmklib -custom -o genepi genepiLibrary.cmo $(COMMON_LIBS)

genepiLibrary.cmx: genepiLibrary.ml
	ocamlopt -c genepiLibrary.ml

$(THISDIR)/genepi.cmxa: $(COMMON_DEPS) genepiLibrary.cmx
	ocamlmklib -custom -o genepi genepiLibrary.cmx $(COMMON_LIBS)

##################################################
# utility targets
##################################################
clean::
	rm -f core *.o *.cmo *.cma *.cmi *.a *.cmxa *.cmx
