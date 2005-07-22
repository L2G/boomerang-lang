####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ..
include $(TOP)/Top.Makefile

TARGETS = src2tex src2f 

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

clean::
	rm -rf *.cmi *.cmo *.o $(TARGETS) *.cmx src2fcl.ml src2tex.ml *.tex *.nav *.snm *.toc
