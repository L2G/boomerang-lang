TOP	:= ../..
include $(TOP)/Top.Makefile

SRC := ../../src

RESULT = iCalViewer
LIBS += harmony
SOURCES = $(COMMON_SOURCES)  iCalendar_syntax.ml iCalendar_print.ml \
                 iCalendar_lextypes.ml iCalendarparse.mly \
		 iCalendarlex.mll ical.mli ical.ml \
		 iCalendar.mli iCalendar.ml \
		 iCalViewer.ml

all: makeharmonylib nc

clean::
	rm -rf *.output

include $(OCAMLMAKEFILE)
