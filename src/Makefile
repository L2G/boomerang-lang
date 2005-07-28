####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
SUBDIRSCLEANONLY = ubase 
include $(TOP)/Top.Makefile

RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLLDFLAGS = -linkpkg

# PRE_TARGETS = lexer.mll parser.mly

# Why did we have this?  The ocaml manual says it is not recommended!
# OCAMLLEX = ocamllex -ml

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

SRC2FCL_SOURCES = src2fcl.mll
BASE_SOURCES = info.mli info.ml error.mli error.ml misc.mli misc.ml \
               mapplus.mli mapplus.ml \
               name.mli name.ml v.mli v.ml lens.mli lens.ml \
               relation.mli relation.ml \
	       treedb.mli treedb.ml csvdb.mli csvdb.ml \
               surveyor.mli surveyor.ml \
               syntax.mli syntax.ml parser.mly lexer.mli lexer.mll \
               schema.mli schema.ml value.mli value.ml \
               env.mli env.ml registry.mli registry.ml \
	       sync.mli sync.ml \
               compiler.mli compiler.ml 

VIEWER_SOURCES = metay.mly metal.mll \
		 viewers.ml

TOPLEVEL_SOURCES = toplevel.ml  


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

COMMON_SOURCES = $(SRC2FCL_SOURCES:%=$(TOOLSDIR)/%) \
                 $(UBASE_LIB_SOURCES:%=$(SRCDIR)/ubase/%) \
		 $(BASE_SOURCES:%=$(SRCDIR)/%) \
		 $(VIEWER_SOURCES:%=$(SRCDIR)/%) \
		 $(NATIVE_LENS_SOURCES:%=$(LENSESDIR)/%) \
		 $(TOPLEVEL_SOURCES:%=$(SRCDIR)/%) 

TRASH := $(TRASH) $(SRCDIR)/parser.output $(SRCDIR)/metay.output 

include $(OCAMLMAKEFILE)


######################################################################
# Documentation generation for Harmony sources

DOCDIR = doc
READINORDER = name.mli v.mli lens.mli surveyor.mli sync.mli harmony.mli
ALLMLIS = $(wildcard *.mli)

temp: 
	echo 

docs: 
	-mkdir $(DOCDIR)
	ocamldoc -d $(DOCDIR) -html $(READINORDER) $(filter-out $(READINORDER) parser.mli metay.mli, $(wildcard *.mli))

clean::
	rm -rf lenses doc parser.mly lexer.mll
