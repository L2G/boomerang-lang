####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ..
include $(TOP)/Top.Makefile

TARGETS = src2tex src2f wrap dump_oracle

all:  $(TARGETS)

src2tex : % : %.ml
	ocamlopt -o $@ $<

%.o : %.ml
	ocamlopt -c $<

src2f: src2fcl.ml src2f.ml
	ocamlopt -o $@ src2fcl.ml src2f.ml


%.ml : %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

dump_oracle : % : %.ml
	ocamlopt -o $@ $<

wrap: wrap.ml
	ocamlfind ocamlopt -package unix -linkpkg -o $@ $<

clean::
	rm -rf *.cmi *.cmo *.o $(TARGETS) *.cmx src2fcl.ml src2tex.ml *.tex *.nav *.snm *.toc
