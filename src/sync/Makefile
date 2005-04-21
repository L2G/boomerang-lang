SRC = $(shell pwd)/..

RESULT = harmony
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES) config.ml input.ml \
	cmdlineparser.mly cmdlinelexer.mll sync.ml cmdlineui.ml \
	test.ml ui.ml harmony.ml 
OCAMLLDFLAGS = -linkpkg

include $(SRC)/Common.Makefile

