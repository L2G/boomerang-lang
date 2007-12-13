TOP	:= ../..

SRC := ../../src

RESULT = iCalViewer
LIBS += harmony

INCDIRS += $(SRCDIR) $(SRCDIR)/ubase

SOURCES = $(COMMON_SOURCES)  iCalendar_syntax.ml iCalendar_print.ml \
                 iCalendar_lextypes.ml iCalendarparse.mly \
		 iCalendarlex.mll ical.mli ical.ml \
		 iCalendar.mli iCalendar.ml \
		 iCalViewer.ml

all: native-code

clean::
	rm -rf *.output

