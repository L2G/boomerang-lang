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

# set the argument of the cd command appropriately
ifndef PML_HOME
  export PML_HOME=$(shell cd ../..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

CURDIR := $(shell pwd)

SRC=..
BUILD := $(AST_ARCH)
DEBUG_DIR=debug
P_DIR=../../pml
GEN_DIR= ../../gen
# GEN directory relative to pml directory.
GEN_P_DIR= ../gen

TOOL_DIR := ../../tools
TOOL_LIB_DIR := $(TOOL_DIR)/$(BUILD)
# List of tools used by the test modules
TOOLS := harmony_tool harmony_untool debug_tool xml_formatter acctool cisco_xml_formatter
TOOLS_CMIS:=$(foreach tool,$(TOOLS),$(TOOL_LIB_DIR)/$(tool).cmi)
TOOLS_CMOS:=$(foreach tool,$(TOOLS),$(TOOL_LIB_DIR)/$(tool).cmo)
TOOLS_CMXS:=$(foreach tool,$(TOOLS),$(TOOL_LIB_DIR)/$(tool).cmx)

DESCRS= bibtex

#datatype_implicit intTriple intlist \
#	match_dt newick record regulus sirius to_do_list

ifdef MKCISCO
DESCRS += patterns arg_map perlre cisco
endif

DESCR_CMOS:=$(foreach descr,$(DESCRS),$(descr).cmo)
DESCR_CMOS_D:=$(foreach descr,$(DESCRS),$(DEBUG_DIR)/$(descr).cmo)
DESCR_CMXS:=$(foreach descr,$(DESCRS),$(descr).cmx)
DESCR_OBJS:=$(DESCR_CMXS:.cmx=.o)
TEST_SOURCES:=$(foreach descr,$(DESCRS),test_$(descr).ml)
TEST_CMXS:=$(TEST_SOURCES:.ml=.cmx)
TEST_PROGS:=$(foreach descr,$(DESCRS),test_$(descr))
TEST_PROGS_DEBUG:=$(foreach descr,$(DESCRS),test_$(descr)_d)
RDONLY_PROGS:=$(foreach descr,$(DESCRS),rdonly_$(descr))
DESCR_SOURCES:=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).ml)
DESCR_SCMOS:=$(foreach descr,$(DESCRS),$(GEN_DIR)/$(descr).cmo)

ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory

.PHONY: all tests clean tester $(DESCRS) $(DESCR_SOURCES) $(DESCR_SCMOS) $(TOOLS_USED)

all: $(DESCRS)

tests: $(TEST_PROGS)

$(DESCRS): % : test_%
	./test_$* ../../data/$*.txt 

include $(PML_HOME)/mk/common.mk

INCLUDES+=-I $(PML_LIB_DIR) -I $(TOOL_LIB_DIR) -I $(HARMONY_HOME) 

# make the PML compiler library
$(PMLC_LIB):
	$(MAKE) -C $(PMLC_DIR) $@

# make the PML runtime
$(PMLRUNTIME):
	$(MAKE) -C $(PMLRUNTIME_DIR) $@

# compile .pml to source code
$(DESCR_SOURCES) : $(GEN_DIR)/%.ml:
	$(MAKE) -C $(P_DIR) $(GEN_P_DIR)/$*.ml

# compile source version of example
$(DESCR_SCMOS): $(GEN_DIR)/%.cmo :
ifdef DEBUG
	$(MAKE) DEBUG=$(DEBUG) -C $(P_DIR) $(GEN_P_DIR)/$*.cmo
else
	$(MAKE) -C $(P_DIR) $(GEN_P_DIR)/$*.cmo
endif
	@(if [ -e $(GEN_DIR)/$*.cmi ]; then \
	    mv $(GEN_DIR)/$*.cmi .;	\
	  fi)
	@mv $@ .

# compile the .pml to a .cmo
$(DESCR_CMOS): PREPROC=-pp "camlp4o -I $(PMLC_LIB_DIR) $(PMLC_LIB) -impl "
$(DESCR_CMOS): %.cmo :$(P_DIR)/%.pml $(PMLC_LIB) $(PMLRUNTIME) $(HARMONY_LIB_OPT)
	$(OCAMLC) $(OCAMLFLAGS) $(PREPROC) -c -impl $<
	@(if [ -e $(P_DIR)/$*.cmi ]; then \
	    mv $(P_DIR)/$*.cmi .;	\
	  fi)
	@mv $(P_DIR)/$@ .

# compile the .pml to debug-version .cmo
$(DESCR_CMOS_D): PREPROC = -pp "camlp4o -I $(PMLC_LIB_DIR) $(PMLC_LIB) -impl "
# Using "+=" below would not work correctly. Fails to capture new PREPROC definition.
$(DESCR_CMOS_D): OCAMLFLAGS := $(OCAMLFLAGS) -g  
$(DESCR_CMOS_D): $(DEBUG_DIR)/%.cmo :$(P_DIR)/%.pml $(PMLC_LIB) $(PMLRUNTIME) $(HARMONY_LIB)
	$(OCAMLC) $(OCAMLFLAGS) -c -impl $<
	@(if [ -e $(P_DIR)/$*.cmi ]; then \
	    mv $(P_DIR)/$*.cmi .;	\
	  fi)
	@mv $(P_DIR)/$(@F) $(DEBUG_DIR)/$(@F)

# compile the .pml to .cmx and .o
$(DESCR_OBJS) $(DESCR_CMXS): PREPROC=-pp "camlp4o -I $(PMLC_LIB_DIR) $(PMLC_LIB) -impl "

$(DESCR_CMXS): %.cmx :$(P_DIR)/%.pml $(PMLC_LIB) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -impl $<
	@(if [ -e $(P_DIR)/$*.cmi ]; then \
	    mv $(P_DIR)/$*.cmi .;	\
	  fi)
	@mv $(P_DIR)/$@ .
	@mv $(P_DIR)/$*.o .

$(DESCR_OBJS) : %.o :$(P_DIR)/%.pml $(PMLC_LIB) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -impl $<
	@(if [ -e $(P_DIR)/$*.cmi ]; then \
	    mv $(P_DIR)/$*.cmi .;	\
	  fi)
	@mv $(P_DIR)/$@ .
	@mv $(P_DIR)/$*.cmx .

$(TOOLS_CMXS):
	$(MAKE) -C $(TOOL_DIR) $(@F)

# make byte-code version of a test program
# $(TEST_PROGS): test_%: test_%.cmx %.cmo $(TOOLS_CMXS) $(PMLRUNTIME) tester.cmo tester.cmi
# 	$(OCAMLC) $(OCAMLFLAGS) $(PMLRUNTIME) $(XML_LIB) tester.cmo \
# 	$(TOOLS_CMOS) $*.cmo $< -o $@

# make optimized version of test program
$(TEST_CMXS):INCLUDES+=-I $(BUILD)
$(TEST_CMXS): test_%.cmx : %.cmx tester.cmi $(TOOLS_CMXS)

$(TEST_PROGS): test_%: test_%.cmx %.cmx %.o $(TOOLS_CMXS) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT) tester.o tester.cmx tester.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) \
        $(OCAML_LIB_DIR)/str.cmxa $(OCAML_LIB_DIR)/unix.cmxa $(HARMONY_LIB_OPT) \
	 $(XML_LIB_OPT) $(TOOLS_CMXS) tester.cmx $*.cmx $< -o $@

# make optimized version of perlre program
proc_perlre: PREPROC=
proc_perlre: proc_perlre.cmx perlre.o arg_map.cmx perlre.cmx $(TOOLS_CMXS) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT) tester.o tester.cmx tester.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) \
	 $(XML_LIB_OPT) tester.cmx \
	$(TOOLS_CMXS) arg_map.cmx perlre.cmx $< -o $@

# generate cisco description

$(P_DIR)/cisco.pml : test_patterns proc_perlre
	./test_patterns ~/projects/netDB/parser-pattern.txt | ./proc_perlre ~/projects/netDB/parser-arg_map.txt >| $@

# make a test program with debug info.
$(TEST_PROGS_DEBUG): test_%_d: test_%.cmo $(DEBUG_DIR)/%.cmo $(TOOLS_CMXS) \
			$(PMLRUNTIME) $(HARMONY_LIB) $(DEBUG_DIR)/tester.cmo tester.cmi
	$(OCAMLC) $(OCAMLFLAGS) -g -verbose $(PMLRUNTIME) $(HARMONY_LIB) $(XML_LIB) \
	$(TOOLS_CMOS) $(DEBUG_DIR)/tester.cmo $(DEBUG_DIR)/$*.cmo $< -o $@

# make optimized version of read-only program
$(RDONLY_PROGS): rdonly_%: rdonly_%.cmx %.o %.cmx $(TOOLS_CMXS) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT) tester.o tester.cmx tester.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT) \
	 $(XML_LIB_OPT) $(TOOLS_CMXS) tester.cmx $*.cmx $< -o $@

$(DEBUG_DIR)/tester.cmo: $(SRC)/tester.ml tester.cmi $(PMLRUNTIME) $(HARMONY_LIB)
	$(OCAMLC) $(OCAMLFLAGS) -g -c $(SRC)/tester.ml 
	@mv $(SRC)/$(@F) $(DEBUG_DIR)/$(@F)

# dependencies for the test driver module
tester.o tester.cmo tester.cmx : tester.cmi
tester : PREPROC=
tester : tester.cmo 


compact_to_do_list : $(SRC)/compact_to_do_list.ml to_do_list.o to_do_list.cmx $(TOOLS_CMXS) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT) tester.o tester.cmx tester.cmi
	$(OCAMLOPT) -verbose  $(OCAMLOPTFLAGS) $(PADSC_RUNTIME_OPT) $(PMLRUNTIME_OPT) $(HARMONY_LIB_OPT) \
	 $(XML_LIB_OPT) tester.cmx \
	$(TOOLS_CMXS) to_do_list.cmx $< -o $@


clean:
	$(RM) *.cm[iox]	
	$(RM) *.o
	$(RM) test_*

# End of rules that are run from the build directory
endif
