####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ../..

DEMO1 = R1ORIG=example1.ics R2FORMAT=ics
DEMO2 = R1ORIG=example2.ics R2FORMAT=meta
DEMO3 = R1ORIG=example3.ics R2FORMAT=meta

all: makeharmonylib icalviewerlib native-code

include $(TOP)/Top.Makefile

STATIC=1
RESULT = harmonize-calendars
SOURCES = $(CWD)/harmonize-calendars.ml 
INCDIRS += $(EXTERNDIR)/iCalendar
LIBDIRS += $(EXTERNDIR)/iCalendar
LIBS += harmony iCalViewer

icalviewerlib:
	$(MAKE) -C $(EXTERNDIR)/iCalendar native-code-library

clean::
	rm -rf ar*.{ics,meta} r1*.{ics,meta} r2*.{ics,meta} .harmonyar*.meta .harmonyar*.tmp

include $(OCAMLMAKEFILE)

####################################################################
# Tests

test:: buildharmony unit-tests 

unit-tests:
	$(HARMONY) -check ICalendar -test-all