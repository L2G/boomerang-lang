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

BASE_SOURCES = misc.ml mapplus.ml name.ml v.ml lens.ml surveyor.ml \
               pretty.ml info.ml error.ml \
	       syntax.ml parser.mly lexer.mll \
               type.ml value.ml registry.ml \
               compiler.ml

VIEWER_SOURCES = metay.mly metal.mll meta.ml xml.ml

################
# SYNC SOURCES #
################

SYNC_SOURCES = sync.ml

###########
# PLUGINS #
###########

PLUGIN_SOURCES = native.ml

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
