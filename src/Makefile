####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Makefile - main Makefile for harmony sources                     #
####################################################################

SRC = $(shell pwd)

RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLLDFLAGS = -linkpkg

DOCDIR = doc

include $(SRC)/Common.Makefile

test: all

docs: all
	-rm -rf $(DOCDIR)
	-mkdir $(DOCDIR)
	ocamldoc -d $(DOCDIR) -html v.mli 
