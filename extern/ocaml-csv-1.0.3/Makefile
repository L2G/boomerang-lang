# $Id: Makefile,v 1.5 2005/05/24 13:53:45 rich Exp $

PACKAGE		:= ocaml-csv
VERSION		:= 1.0.3

OCAMLC		:= ocamlc
OCAMLCINCS	:=
OCAMLCFLAGS	:= -w s -g
OCAMLCLIBS	:=

OCAMLOPT	:= ocamlopt
OCAMLOPTINCS	:= $(OCAMLCINCS)
OCAMLOPTFLAGS	:= -w s
OCAMLOPTLIBS	:=

OBJS		:= csv.cmo
XOBJS		:= $(OBJS:.cmo=.cmx)

all: csv.cma csv.cmxa example csvtool

csv.cma: $(OBJS)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $^

csv.cmxa: $(XOBJS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $^

example: csv.cma example.ml
	$(OCAMLC) $^ -o $@

test:	csv.cma test.ml
	$(OCAMLC) $^ -o $@
	./test

csvtool: csv.cmxa csvtool.ml
	$(OCAMLOPT) $^ -o $@

# Common rules for building OCaml objects.

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) $(OCAMLCINCS) -c $<
.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) $(OCAMLCINCS) -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLOPTINCS) -c $<

.SUFFIXES:	.cmo .cmi .cmx .ml .mli

# Clean.

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.a *~ core csvtool example

# Dependencies.

depend: .depend

.depend: $(wildcard *.mli) $(wildcard *.ml)
	rm -f .depend
	ocamldep $^ > $@

ifeq ($(wildcard .depend),.depend)
include .depend
endif

# Build a distribution.

dist:
	$(MAKE) check-manifest
	rm -rf $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)
	tar -cf - -T MANIFEST | tar -C $(PACKAGE)-$(VERSION) -xf -
	tar zcf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	rm -rf $(PACKAGE)-$(VERSION)
	ls -l $(PACKAGE)-$(VERSION).tar.gz

check-manifest:
	@for d in `find -type d -name CVS | grep -v '^\./debian/'`; \
	do \
	b=`dirname $$d`/; \
	awk -F/ '$$1 != "D" {print $$2}' $$d/Entries | \
	sed -e "s|^|$$b|" -e "s|^\./||"; \
	done | sort > .check-manifest; \
	sort MANIFEST > .orig-manifest; \
	diff -u .orig-manifest .check-manifest; rv=$$?; \
	rm -f .orig-manifest .check-manifest; \
	exit $$rv

.PHONY: depend dist check-manifest
