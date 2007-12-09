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
ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDEP=ocamldep
INCLUDES=-I +camlp4
DEPINCLUDES=-I +camlp4 
# Set the PREPROC variable to use a preprocessor
OCAMLFLAGS=$(INCLUDES) $(PREPROC) -g 
OCAMLOPTFLAGS=$(INCLUDES) $(PREPROC) -inline 10 -ccopt -O3 -noassert
ifdef DEBUG_OCAMLOPT
OCAMLOPTFLAGS += -ccopt "-Xlinker -Y -Xlinker 3" -verbose
endif

# if CAMLIDL_LIB_DIR, then its probably different than the default ocaml lib directory, so add it to the includes.
ifdef CAMLIDL_LIB_DIR
INCLUDES +=-I $(CAMLIDL_LIB_DIR)
endif

PML_LIB=$(PML_HOME)/arch/$(AST_ARCH)/lib
PML_LIB_DIR:=$(PML_LIB)

PMLC_DIR=$(PML_HOME)/compiler
PMLC_LIB_DIR=$(PML_LIB)/comp
PMLC_LIB=$(PMLC_LIB_DIR)/pmlcomp.cma

PMLRUNTIME_DIR=$(PML_HOME)/runtime
PMLRUNTIME_LIB_DIR=$(PML_LIB)/runtime
PMLRUNTIME=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cma
PMLRUNTIME_OPT=$(PMLRUNTIME_LIB_DIR)/pmlruntime.cmxa
PMLRUNTIME_OPT_CLIB=$(PMLRUNTIME_LIB_DIR)/pmlruntime.a

PADSC_RUNTIME_DIR=$(PML_HOME)/runtime/padsc_interface
PADSC_RUNTIME_LIB_DIR=$(PMLRUNTIME_LIB_DIR)
PADSC_RUNTIME=$(PADSC_RUNTIME_LIB_DIR)/padsc.cma
PADSC_RUNTIME_OPT=$(PADSC_RUNTIME_LIB_DIR)/padsc.cmxa
PADSC_RUNTIME_OPT_CLIB=$(PADSC_RUNTIME_LIB_DIR)/padsc.a

# XMLLIGHT_LIB_DIR specifies the installed location of the
# MotionTwin XMLLight libraries
ifndef XMLLIGHT_LIB_DIR
XMLLIGHT_LIB_DIR := $(PML_LIB)/xml
endif
INCLUDES +=-I $(XMLLIGHT_LIB_DIR)
# XML-Light library to be linked
XML_LIB:=$(XMLLIGHT_LIB_DIR)/xml-light.cma
XML_LIB_OPT:=$(XMLLIGHT_LIB_DIR)/xml-light.cmxa

ifdef SRC
  # Tell make to search in source directory.
  # We use GPATH (in addition to VPATH) so that
  # updates are done to files in the directories 
  # they are found.

  vpath %.mli $(SRC)
  vpath %.ml $(SRC)
  GPATH = $(SRC)
endif

ifndef SRC
  # override this variable to look for sources in different directory
  SRC=.
endif

# ifndef BUILD
#   # override this variable to place compiled output in different directory
#   BUILD=.
# endif

.PHONY: depend

# Common rules

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     mv $(dir $<)$@ $@; \
          fi)

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     if [ -e $(dir $<)$*.cmi ]; then \
	        mv $(dir $<)$*.cmi $(dir $@);	\
	     fi; \
	     mv $(dir $<)$@ $@; \
          fi)

%.cmx: %.ml 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     if [ -e $(dir $<)$*.cmi ]; then \
	        mv $(dir $<)$*.cmi $(dir $@);	\
	     fi; \
	     mv $(dir $<)$@ $@; \
	     mv $(dir $<)$*.o $*.o; \
          fi)

# Dependencies. Executed from build directory.  

# Note: this target will always return "Nothing to be done for..." as
# the file .depend will automatically be updated by make, if the
# makefile contains "include .depend". So, by the time this rule is
# executed, .depend will always be up to date. Still, this target is a
# useful dummy to force it to update.
depend: .depend

# Note: this target will always return ".depend is up to date" when
# called explicitly by user when file ".depend" is included by
# makefile. See target "depend" for explanation.
.depend: $(SOURCES)
        # use $(CURDIR), which is set before "cd $(SRC)" to the location of the $(BUILD) directory.
        # We need to do this from the source directory so that the path
        # names in .depend come out correctly. If a source bar.ml is in directory foo,
        # then ocamldep assumes that the .cm... files are also in foo. Given our use of
        # build directories, source and object files are rarely if ever in the same directory.
	(cd $(SRC);\
	$(OCAMLDEP) $(DEPINCLUDES) $(PREPROC) $(SOURCES) > $(CURDIR)/.depend)
