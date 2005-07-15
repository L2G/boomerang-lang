####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common.Makefile - common rules for OCaml parts of Harmony        #
####################################################################
# $Id$  

tags:
	etags $(SOURCES)

#################
# EXTERNAL LIBS #
#################

all: $(EXTERNDIR)/ocaml-csv-1.0.3/csv.cmxa
native-code: $(EXTERNDIR)/ocaml-csv-1.0.3/csv.cmxa
native-code-library: $(EXTERNDIR)/ocaml-csv-1.0.3/csv.cmxa

LIBS += $(EXTERNDIR)/ocaml-csv-1.0.3/csv

profiling: 
	$(MAKE) pnc

$(EXTERNDIR)/ocaml-csv-1.0.3/csv.cma:
	$(MAKE) -C $(EXTERNDIR)/ocaml-csv-1.0.3

$(EXTERNDIR)/ocaml-csv-1.0.3/csv.cmxa:
	$(MAKE) -C $(EXTERNDIR)/ocaml-csv-1.0.3

###################
# DEFAULT TARGETS #
###################

ifeq ($(COMPILEHARMONYASBYTECODE),yes)
all: byte-code
else
all: native-code
endif

#########
# UBASE #
#########

UBASE_LIB_SOURCES = safelist.ml uprintf.ml util.ml uarg.ml prefs.ml trace.ml

#########################
# BASE/COMPILER SOURCES #
#########################

BASE_SOURCES = info.mli info.ml error.mli error.ml misc.mli misc.ml \
               mapplus.mli mapplus.ml \
               name.mli name.ml v.mli v.ml lens.mli lens.ml \
               relation.mli relation.ml \
	       treedb.mli treedb.ml csvdb.mli csvdb.ml \
               surveyor.mli surveyor.ml \
               syntax.mli syntax.ml parser.mly lexer.mli lexer.mll \
               value.mli value.ml schema.mli schema.ml \
               env.mli env.ml registry.mli registry.ml \
	       sync.mli sync.ml \
               compiler.mli compiler.ml \
	       toplevel.ml  

VIEWER_SOURCES = metay.mly metal.mll \
		 viewers.ml

###########
# PLUGINS #
###########

RELATIONAL_SOURCES = rlens.mli rlens.ml \
                     dblens.mli dblens.ml \
                     relational.ml

NATIVE_LENS_SOURCES = prelude.ml \
                 $(RELATIONAL_SOURCES)

##################
# COMMON SOURCES #
##################

COMMON_SOURCES = $(UBASE_LIB_SOURCES:%=$(SRCDIR)/ubase/%) \
		 $(BASE_SOURCES:%=$(SRCDIR)/%) \
		 $(VIEWER_SOURCES:%=$(SRCDIR)/%) \
		 $(NATIVE_LENS_SOURCES:%=$(LENSESDIR)/%)

TRASH := $(TRASH) $(SRCDIR)/parser.output $(SRCDIR)/metay.output 

include $(OCAMLMAKEFILE)
