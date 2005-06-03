####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Makefile - main Makefile for harmony sources                     #
####################################################################

TOP = ..
# SUBDIRS = ubase
include $(TOP)/Top.Makefile

SRC = $(shell pwd)

RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLLDFLAGS = -linkpkg

DOCDIR = doc

OCAMLLEX = ocamllex -ml

include $(SRC)/Common.Makefile

test:: all

docs: all
	-rm -rf $(DOCDIR)
	-mkdir $(DOCDIR)
	ocamldoc -d $(DOCDIR) -html v.mli 
