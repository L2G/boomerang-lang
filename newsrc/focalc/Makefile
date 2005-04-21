OCAMLC = ocamlfind ocamlc -dtypes -package "netstring,unix,str" 
OCAMLOPT = ocamlfind ocamlopt -dtypes -package "netstring,unix,str" 
OCAMLMKTOP = ocamlfind ocamlmktop -dtypes -package "netstring,unix,str" 
OCAMLYACC = ocamlyacc -v 
OCAMLLDFLAGS = -linkpkg

SRC := $(shell pwd)/..

OCAMLMAKEFILE = $(SRC)/OCamlMakefile

UBASE_LIB_SOURCES = safelist.ml uprintf.ml util.ml \
                    uarg.ml prefs.ml trace.ml

BASE_SOURCE = misc.ml mapplus.ml name.ml v.ml lens.ml info.ml error.ml \
              syntax.ml type.ml value.ml pretty.ml

COMPILER_SOURCE = parser.mly lexer.mll checker.ml compiler.ml 

###########
# PLUGINS #
###########

PERVASIVES_PLUGIN_SOURCE = pervasives_plugin.ml

PLUGINS_SOURCES = $(PERVASIVES_PLUGIN_SOURCE:%=pervasives/%) 

COMMON_SOURCES = $(UBASE_LIB_SOURCES:%=$(SRC)/lib/ubase/%) \
		 $(BASE_SOURCE:%=$(SRC)/common/%) \
                 $(COMPILER_SOURCE:%=$(SRC)/compiler/%) \
		 $(PLUGINS_SOURCE:%=$(SRC)/plugins/%

RESULT = focalc
SOURCES = $(COMMON_SOURCES) main.ml 

-include $(SRC)/OCamlMakefile

all: native-code