SRC = $(shell pwd)/..
UBASELIB = $(SRC)/lib/ubase/ubase.cma

RESULT = harmony
CLIBS =
SOURCES = unix.cma $(UBASELIB) config.ml harmony.ml
OCAMLLDFLAGS = -linkpkg

default:
	ocamlc -o $(RESULT) $(SOURCES)
