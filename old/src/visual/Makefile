SRC := $(shell pwd)/..

RESULT = visual
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES) ui_common.ml ui_example.ml ui_trace.ml \
         ui_focal.ml callbacks.ml visual.ml 
INCDIRS := +lablgtk2
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx
OCAMLLDFLAGS = -linkpkg 
STATIC = 1

include $(SRC)/Common.Makefile

test: all
	(cd examples; ../$(RESULT))
