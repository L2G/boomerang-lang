#####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common.Makefile - source dependencies: included in Makefiles     #
#                                                                  #
# $Id$ #
#                                                                  #
#####################################################################

OCAMLMAKEFILE = $(SRC)/OCamlMakefile

OCAMLC = ocamlfind ocamlc -dtypes -package "pxp,pxp-engine,pxp-lex-iso88591,netstring,unix,str" 
OCAMLOPT = ocamlfind ocamlopt -dtypes -package "pxp,pxp-engine,pxp-lex-iso88591,netstring,unix,str" 
OCAMLMKTOP = ocamlfind ocamlmktop -dtypes -package "pxp,pxp-engine,pxp-lex-iso88591,netstring,unix,str" 

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

BASE_SOURCES = config.ml misc.ml mapplus.ml name.ml v.ml lens.ml surveyor.ml xml.ml \
               pretty.ml info.ml error.ml syntax.ml parser.mly lexer.mll type.ml value.ml registry.ml \
               checker.ml compiler.ml

###########
# PLUGINS #
###########

NATIVE_PLUGIN_SOURCES = native.ml

PLUGINS_SOURCES = $(NATIVE_PLUGIN_SOURCES:%=native/%)

##################
# COMMON SOURCES #
##################

COMMON_SOURCES = $(UBASE_LIB_SOURCES:%=$(SRC)/lib/ubase/%) \
		 $(BASE_SOURCES:%=$(SRC)/%) \
		 $(PLUGINS_SOURCES:%=$(SRC)/plugins/%)

include $(SRC)/OCamlMakefile
