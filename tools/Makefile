TARGETS = src2tex src2f 

all:  $(TARGETS)

src2f src2tex : % : %.ml
	ocamlopt -o $@ $<

%.ml : %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

clean:
	clean
	rm -rf *.cmi *.cmo *.o $(TARGETS) *.cmx *.ml tex *.nav *.snm *.toc

test: all
	-mkdir tex
	src2tex -basename testbase test.src

#	src2f test.src
#	cat test.3
