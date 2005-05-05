# $Id: Makefile,v 1.2 2005/01/27 15:06:48 schmitta Exp $

OCAMLC = ocamlfind ocamlc 
OCAMLMKTOP = ocamlfind ocamlmktop

RESULT = iCalendar

# Add new source files here -- order matters. 
# Don't forget .mli files (needed for computing dependencies)
SOURCES = \
        iCalendar_syntax.ml iCalendar_print.ml iCalendar_lextypes.ml iCalendarparse.mly iCalendarlex.mll\
	iCalendar.mli iCalendar.ml

# bc means "bytecode executable". there are other targets for native code,
# library creation, toplevels, etc. which we can get into if we need
# dc means "bc with debug info"
all: bcl ncl test

.PHONY: install
install:
	ocamlfind install iCalendar *.mli *.cmi *.cma *.cmxa *.a META

.PHONY: uninstall
uninstall:
	ocamlfind remove iCalendar

test: iCalendar.cma
	ocamlc iCalendar.cma iCalendartest.ml -o iCalendartest
	./iCalendartest calendar.ics

-include OCamlMakefile

TRASH += TAGS *.annot iCalendartest
