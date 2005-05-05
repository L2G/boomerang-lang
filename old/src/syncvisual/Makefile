SRC := $(shell pwd)/..

RESULT = syncvisual
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES) ../sync/sync.ml widgets.ml callbacks.ml syncvisual.ml 
INCDIRS := +lablgtk2
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx
OCAMLLDFLAGS = -linkpkg
STATIC = 1

include $(SRC)/Common.Makefile

test: all
	(cd examples; ../$(RESULT))