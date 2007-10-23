SOURCES=lex hist oracle struct

PROGRAM_SOURCES=driver

MLI_FILES=$(foreach source,$(SOURCES),$(source).mli)
ML_FILES=$(foreach source,$(SOURCES) $(PROGRAM_SOURCES),$(source).ml)
CMI_FILES=$(foreach source,$(SOURCES),$(source).cmi)
PROGRAM_CMI_FILES=$(foreach source,$(PROGRAM_SOURCES),$(source).cmi)
CMO_FILES=$(foreach source,$(SOURCES) $(PROGRAM_SOURCES),$(source).cmo)

PROGRAM=learn

ifndef OCAML_LIB_DIR
OCAML_LIB_DIR=/usr/lib/ocaml
endif

ifndef OCAMLC
OCAMLC=$(shell which ocamlc)
endif

STR_CMA=$(OCAML_LIB_DIR)/str.cma

.DEFAULT: $(PROGRAM)

.PHONY: clean tidy all

all: $(CMO_FILES)

$(PROGRAM) : .depend $(STR_CMA) $(CMO_FILES)
	$(OCAMLC) -o $@ $(STR_CMA) $(CMO_FILES)

$(CMO_FILES) : %.cmo : %.ml
	$(OCAMLC) -c $<

$(CMI_FILES) : %.cmi : %.mli
	$(OCAMLC) -c $<

clean:
	rm -f .depend $(CMO_FILES) $(CMI_FILES) $(PROGRAM_CMI_FILES) $(PROGRAM)

veryclean: clean
	rm -f *~ \#*

test:

.depend: GNUmakefile $(MLI_FILES) $(ML_FILES)
	ocamldep $(MLI_FILES) $(ML_FILES) > $@

-include .depend
