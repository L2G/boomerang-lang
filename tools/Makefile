####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ..
include $(TOP)/Top.Makefile

TARGETS = src2tex src2f 

all:  $(TARGETS)

src2f src2tex : % : %.ml
	ocamlopt -o $@ $<

%.ml : %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

clean::
	rm -rf *.cmi *.cmo *.o $(TARGETS) *.cmx *.ml tex *.nav *.snm *.toc
