TOP	:= ../..
include $(TOP)/Top.Makefile

SRC := ../../src

RESULT = iCalViewer
LIBS += harmony
SOURCES = $(COMMON_SOURCES)  iCalendar_syntax.ml iCalendar_print.ml \
                 iCalendar_lextypes.ml iCalendarparse.mly \
		 iCalendarlex.mll iCalendar.mli iCalendar.ml \
		 ical.mli ical.ml iCalViewer.ml

all: makeharmonylib nc

makeharmonylib:
	$(MAKE) -C $(SRCDIR) native-code-library

include $(OCAMLMAKEFILE)
