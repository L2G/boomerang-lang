TOP	:= ../..
include $(TOP)/Top.Makefile

SRC := ../../src

RESULT = iCalViewer
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES)  iCalendar_syntax.ml iCalendar_print.ml \
                 iCalendar_lextypes.ml iCalendarparse.mly \
		 iCalendarlex.mll iCalendar.mli iCalendar.ml \
		 ical.mli ical.ml iCalViewer.ml

include $(SRC)/Common.Makefile

