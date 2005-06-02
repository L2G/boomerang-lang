####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common.Makefile - source dependencies: included in Makefiles     #
####################################################################
# $Id$  

OCAMLMAKEFILE = $(SRC)/OCamlMakefile

PACKS = "netstring,unix,str,pxp,pxp-engine,pxp-lex-utf8" 
YFLAGS = -v 
OCAMLFLAGS = -dtypes
OCAMLCPFLAGS = f

all: native-code

tags:
	etags $(SOURCES)

#######
# LIB #
#######

UBASE_LIB_SOURCES = safelist.ml uprintf.ml util.ml uarg.ml prefs.ml trace.ml

#########################
# BASE/COMPILER SOURCES #
#########################

BASE_SOURCES = misc.mli misc.ml mapplus.mli mapplus.ml name.ml v.mli v.ml lens.mli \
               lens.ml info.ml error.ml surveyor.mli surveyor.ml \
               pretty.ml syntax.ml parser.mly lexer.mll \
               type.ml value.mli value.ml registry.mli registry.ml \
               compiler.mli compiler.ml

VIEWER_SOURCES = metay.mly metal.mll meta.ml xml.ml

################
# SYNC SOURCES #
################

SYNC_SOURCES = sync.ml

###########
# PLUGINS #
###########

RELATIONAL_SOURCES = relation.mli relation.ml \
                     rlens.mli rlens.ml \
                     dblens.mli dblens.ml \
                     treedb.ml relational.ml

PLUGIN_SOURCES = native.ml \
                 $(RELATIONAL_SOURCES:%=relational/%)

##################
# COMMON SOURCES #
##################

COMMON_SOURCES = $(UBASE_LIB_SOURCES:%=$(SRC)/lib/ubase/%) \
		 $(BASE_SOURCES:%=$(SRC)/%) \
		 $(SYNC_SOURCES:%=$(SRC)/%) \
		 $(VIEWER_SOURCES:%=$(SRC)/lib/viewers/%) \
		 $(PLUGIN_SOURCES:%=$(SRC)/lib/native/%)

TRASH := $(TRASH) $(SRC)/parser.output $(SRC)/lib/viewers/metay.output

include $(SRC)/OCamlMakefile
