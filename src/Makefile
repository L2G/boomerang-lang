####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id$

TOP = ..
SUBDIRSCLEANONLY = ubase 
include $(TOP)/Top.Makefile

STATIC = 1
RESULT = harmony
SOURCES = $(COMMON_SOURCES) harmony.ml 
OCAMLCPFLAGS = a

all: TAGS

TAGS: 
	$(MAKE) tags

tags:
	-etags $(SOURCES)

###########################
# EXTERNAL TOOLS AND LIBS #
###########################


GENERATED_AND_EXTERNAL_NATIVE=$(SRCDIR)/bakery.ml \
     $(EXTERNDIR)/ocaml-csv-1.0.3/csv.cmxa \
     $(EXTERNDIR)/genepi/ocaml/genepi.cmxa

GENERATED_AND_EXTERNAL_BYTECODE=$(GENERATED_AND_EXTERNAL_NATIVE:%.cmxa=%.cma)

all: native-code

native-code: $(GENERATED_AND_EXTERNAL_NATIVE)

native-code-library: $(GENERATED_AND_EXTERNAL_NATIVE)

profiling-native-code: $(GENERATED_AND_EXTERNAL_NATIVE)

profiling-byte-code: $(GENERATED_AND_EXTERNAL_BYTECODE)

$(EXTERNDIR)/ocaml-csv-1.0.3/csv.cma:
	$(MAKE) -C $(EXTERNDIR)/ocaml-csv-1.0.3

$(EXTERNDIR)/ocaml-csv-1.0.3/csv.cmxa:
	$(MAKE) -C $(EXTERNDIR)/ocaml-csv-1.0.3

$(EXTERNDIR)/genepi/ocaml/genepi.cma:
	$(MAKE) -C $(EXTERNDIR)/genepi/ocaml/

$(EXTERNDIR)/genepi/ocaml/genepi.cmxa:
	$(MAKE) -C $(EXTERNDIR)/genepi/ocaml/

$(SRCDIR)/baker: $(SRCDIR)/baker.ml
	ocamlfind ocamlopt -package unix -linkpkg -o $@ $^

$(SRCDIR)/bakery.ml: $(SRCDIR)/baker
	$^ $(EXAMPLESDIR) $(LENSESDIR) > $@

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

UBASE_LIB_SOURCES = safelist.ml uprintf.ml util.ml uarg.ml prefs.ml trace.ml rx.ml

#########################
# BASE/COMPILER SOURCES #
#########################

SRC2FCL_SOURCES = src2fcl.mll
BASE_SOURCES = info.mli info.ml error.mli error.ml misc.mli misc.ml \
	       unittest.mli unittest.ml \
               mapplus.mli mapplus.ml \
	       name.mli name.ml int.mli int.ml \
               memo.mli memo.ml \
               db.mli db.ml tree.ml tree.mli v.mli v.ml \
	       dbschema.mli dbschema.ml \
               syntax.mli syntax.ml \
	       lens.mli lens.ml \
	       csvdb.mli csvdb.ml \
	       treedb.mli treedb.ml \
               surveyor.mli surveyor.ml \
               presburger.mli presburger.ml \
               env.mli env.ml \
               treeschema.mli treeschema.ml \
	       schema.mli schema.ml \
	       parser.mly lexer.mli lexer.mll \
               value.mli value.ml bakery.ml \
               registry.mli registry.ml \
	       diff3.mli diff3.ml sync.mli sync.ml \
               compiler.mli compiler.ml \

VIEWER_SOURCES = viewers.ml

TOPLEVEL_SOURCES = toplevel.ml  

###########
# PLUGINS #
###########

#RELATIONAL_SOURCES = rlens.mli rlens.ml \
#                     dblens.mli dblens.ml \
#                     relational.ml

RELATIONAL_SOURCES = relational.ml

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

TRASH := $(TRASH) $(SRCDIR)/bakery.ml $(SRCDIR)/baker $(SRCDIR)/parser.output $(SRCDIR)/metay.output 

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
