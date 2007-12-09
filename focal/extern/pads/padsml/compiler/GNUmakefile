########################################################################
#                                                                      #
#             This software is part of the padsml package              #
#           Copyright (c) 2006-2007 Knowledge Ventures Corp.           #
#                         All Rights Reserved                          #
#        This software is licensed by Knowledge Ventures Corp.         #
#           under the terms and conditions of the license in           #
#                    www.padsproj.org/License.html                     #
#                                                                      #
#  This program contains certain software code or other information    #
#  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     #
#  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY#
#  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      #
#  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  #
#  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF#
#  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  #
#  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              #
#  reserved.  AT&T is a registered trademark of AT&T Corp.             #
#                                                                      #
#                   Network Services Research Center                   #
#                   Knowledge Ventures Labs Research                   #
#                           Florham Park NJ                            #
#                                                                      #
#            Yitzhak Mandelbaum <yitzhak@research.att.com>>            #
#                 Robert Gruber <bob.gruber@gmail.com>                 #
#                                                                      #
########################################################################

 # This is a GNU makefile.

ifndef PML_HOME
  export PML_HOME=$(shell cd ..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

SRC=..
BUILD := $(AST_ARCH)

CURDIR := $(shell pwd)

ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory

PREPROC=-pp "camlp4o pa_extend.cmo q_MLast.cmo -loc loc "
CAMLP4_DBG=camlp4 pa_o.cmo pa_op.cmo pa_extend.cmo q_MLast.cmo pr_o.cmo pr_op.cmo  -loc loc

# core modules all have interfaces
CORE_MODULES=Id Description PError Names Ast Common HostLanguage \
	TypeChecker TypeGenerator GenPDGenerator ParserGenerator \
	TraversalGenerator UntraversalGenerator PrinterGenerator
MODULES=$(CORE_MODULES) Compiler padsML_pp

# this variable is used by the depend target in common.mk
SOURCES=$(foreach module,$(CORE_MODULES),$(module).mli) \
        $(foreach module,$(MODULES),$(module).ml)

INTERFACES=$(foreach module,$(MODULES),$(module).cmi)
OBJS=$(foreach module,$(MODULES),$(module).cmo)
OPT_OBJS=

all : $(OBJS) 

.PHONY: all complib install padsml clean

include $(PML_HOME)/mk/common.mk
-include .depend

complib : $(PMLC_LIB)
install: complib

padsML_pp_comp.ml : $(SRC)/padsML_pp.ml
	$(CAMLP4_DBG) $< > $@

# install compiler library and interfaces
$(PMLC_LIB): $(OBJS)
	+@(if [ ! -d $(PMLC_LIB_DIR) ] ; then \
		mkdir -p $(PMLC_LIB_DIR); \
	  fi)
	$(OCAMLC) -a $(OBJS) -o $@
	cp $(INTERFACES) $(PML_LIB_DIR)

# prog1 : $(PROG1_OBJS)
#         $(OCAMLC) -o prog1 $(OCAMLFLAGS) $(PROG1_OBJS)

# End of rules that are run from the build directory
endif

# Clean up
veryclean:
	rm -rf *~ .build

