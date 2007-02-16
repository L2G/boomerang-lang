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
SUBDIRS := padsc_interface
SUBDIRS_VC := $(foreach subdir,$(SUBDIRS),$(subdir).vc)

BASE_TYPE_DIR := $(PML_HOME)/runtime
BT_SPECS := $(BASE_TYPE_DIR)/base_type_specs.txt
BT_MK := $(BASE_TYPE_DIR)/base_types.mk
# BT_IMPORT := import_bt
# BT_IMPORT_ALL := import_all_bt
BT_IMPORT := $(BASE_TYPE_DIR)/import_bt
BT_IMPORT_ALL := $(BASE_TYPE_DIR)/import_all_bt


ifneq ($(BT_SETUP),done)
ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else
export BT_SETUP := done

include $(PML_HOME)/mk/common.mk

# No dependencies
$(BT_SPECS) import_bt.ml import_all_bt.ml : ;

$(BT_IMPORT) : import_bt.cmo
	$(OCAMLC) $(OCAMLFLAGS) $< -o $@

$(BT_IMPORT_ALL) : import_all_bt.cmo
	$(OCAMLC) $(OCAMLFLAGS) str.cma $< -o $@

$(BT_MK) : $(BT_SPECS) $(BT_IMPORT) $(BT_IMPORT_ALL)
	(cd $(BASE_TYPE_DIR); $(BT_IMPORT_ALL) $(BT_SPECS) $(BT_MK) GEN_MODULES)

.PHONY: remake

# Every target not explicitly mentioned will "call" this phony target.
# As it depends on $(BT_MK), make will check whether to 
# remake $(BT_MK). However, as its phony, make will still execute the commands
# no matter what the result of checking on $(BT_MK).
remake : $(BT_MK) 
	@$(MAKE) --no-print-directory -C .. -f GNUmakefile $(MAKECMDGOALS)

# Need an implicit rule to catch any path prefix
%GNUmakefile : ;

GNUmakefile : ;

%.mk :: ;

# I think that it should be :: so that if a : rule exists, this rule will be overridden.
# Otherwise, this prereq just gets added to the other rule.
% :: remake ;

# End of setup rules run from BUILD
endif 

else

# The following rules are only run once base-type related setup is complete.

#	pstring pstring_ME pstring_SE pstring_FW
MODULES := pads \
	pint punit peof peor \
	plist_gen plist_ch plist_st plist_re plist_nosep plist_np \
	plist_gen_pred plist_gen_spec plist_longest plist_longest_pred\
	precord popt pcommit ptry\
	generic_tool generic_untool type built_ins

# used in depend target of common.mk
SOURCES := $(foreach module,$(MODULES),$(module).mli) $(foreach module,$(MODULES),$(module).ml)

# import variable GEN_MODULES from $(BT_MK)
ifneq ($(MAKECMDGOALS),clean)
  ifneq ($(MAKECMDGOALS),veryclean)
    -include $(BT_MK)
  endif
endif
GEN_SOURCES := $(foreach module,$(GEN_MODULES),$(module).mli) $(foreach module,$(GEN_MODULES),$(module).ml)
SOURCES += $(GEN_SOURCES)
MODULES += $(GEN_MODULES)

INTERFACES := $(foreach module,$(MODULES),$(module).cmi)
OBJS := $(foreach module,$(MODULES),$(module).cmo)
OBJS_OPT := $(foreach module,$(MODULES),$(module).cmx)
C_OBJS_OPT := $(OBJS_OPT:.cmx=.o)


ifneq ($(BUILD),$(notdir $(CURDIR)))
  include $(PML_HOME)/mk/redirect.mk
else

 # The following rules are run from the build directory

# Must unset BT_SETUP so that calls to other makefiles (in other directories) don't 
# inherit it as set.
unexport BT_SETUP

all: $(SUBDIRS) $(OBJS)

$(SUBDIRS):
	$(MAKE) --print-directory -C ../$@ $(MAKECMDGOALS)

.PHONY: all runtime install clean $(SUBDIRS)

include $(PML_HOME)/mk/common.mk

ifneq ($(MAKECMDGOALS),clean)
  ifneq ($(MAKECMDGOALS),veryclean)
    -include .depend
  endif
endif

# This next line must be after including common.mk so as to use the
# settings from common.mk.
DEPINCLUDES += -I $(PADSC_RUNTIME_DIR)
# This next line is commented-out becuase all .cmi files
# are now dumped in $(PML_LIB_DIR).
# INCLUDES += -I $(PADSC_RUNTIME_LIB_DIR)
INCLUDES += -I $(PML_LIB_DIR)

$(C_OBJS_OPT) : %.o : %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
	@(if [ ! "$(dir $<)" = "$(dir $@)" ]; then \
	     mv $(dir $<)$@ $@; \
	     mv $(dir $<)$*.cmx $*.cmx; \
          fi)

# make the pads/c runtime
$(PADSC_RUNTIME) $(PADSC_RUNTIME_OPT) :
	+$(MAKE) -C $(PADSC_RUNTIME_DIR) $@

# install runtime library and interfaces
$(PMLRUNTIME): $(PADSC_RUNTIME) $(OBJS) 
	+@(if [ ! -d $(PMLRUNTIME_LIB_DIR) ] ; then \
		mkdir -p $(PMLRUNTIME_LIB_DIR); \
	  fi)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -a  $(PADSC_RUNTIME) $(OBJS)
	cp $(INTERFACES) $(PML_LIB_DIR)

# install runtime library and interfaces
$(PMLRUNTIME_OPT): $(PADSC_RUNTIME_OPT) $(OBJS_OPT) $(C_OBJS_OPT)
	+@(if [ ! -d $(PMLRUNTIME_LIB_DIR) ] ; then \
		mkdir -p $(PMLRUNTIME_LIB_DIR); \
	  fi)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -verbose -o $@ -a $(OBJS_OPT)
	(lib=$(PMLRUNTIME_OPT_CLIB); $(FixStaticLib))
	cp $(INTERFACES) $(PML_LIB_DIR)

$(PMLRUNTIME_OPT_CLIB): $(PADSC_RUNTIME_OPT) $(OBJS_OPT) $(C_OBJS_OPT)
	+@(if [ ! -d $(PMLRUNTIME_LIB_DIR) ] ; then \
		mkdir -p $(PMLRUNTIME_LIB_DIR); \
	  fi)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -verbose -o $@ -a $(OBJS_OPT)
	(lib=$@; $(FixStaticLib))

runtime: $(PMLRUNTIME) $(PMLRUNTIME_OPT) $(PMLRUNTIME_OPT_CLIB)

install: $(SUBDIRS) runtime

# Clean up
clean:
	mv import_bt.cmi import_bt.cmo import_all_bt.cmi import_all_bt.cmo ..
	$(RM) *.cm[iox]
	$(RM) *.o
	(cd ..; mv import_bt.cmi import_bt.cmo import_all_bt.cmi import_all_bt.cmo $(BUILD))

$(GEN_SOURCES): $(BT_SPECS) $(BT_IMPORT) $(BT_IMPORT_ALL)
	(cd $(BASE_TYPE_DIR); $(BT_IMPORT_ALL) $(BT_SPECS) $(BT_MK) GEN_MODULES)

# End of rules that are run from the build directory
endif

.PHONY: veryclean $(SUBDIRS) $(SUBDIRS_VC)

# Rules not in the above conditional will override the 
# catchall rule in redirect.mk. Therefore, if specified by
# the user, they will be run from original directory. If specified
# as prereqs, then will be run from same directory as rule for which
# they are a prereq.

$(SUBDIRS_VC): %.vc : 
	$(MAKE) --print-directory -C $* veryclean

veryclean:  $(SUBDIRS_VC)
	$(RM) *~ 
	-$(BT_IMPORT_ALL) --clean $(BT_SPECS)
	$(RM) $(BT_MK)
	$(RM) $(BT_IMPORT) $(BT_IMPORT_ALL)
	$(RM) -r $(BUILD)

# veryclean: $(SUBDIRS_VC)
# 	$(RM) *~ 
# 	$(RM) $(GEN_SOURCES)
# 	$(RM) $(BT_MK)
# 	$(RM) $(BT_IMPORT) $(BT_IMPORT_ALL)
# 	$(RM) -r .build

# btclean:
# 	-$(BT_IMPORT_ALL) --clean $(BT_SPECS)

# End of rules that are only run after setting up the base-type related
# files
endif
