TOP = ../..
include $(TOP)/Top.Makefile

ICALREAD = $(TOP)/extern/iCalendar/iCalViewer ascal
ICALWRITE = $(TOP)/extern/iCalendar/iCalViewer asmeta
DUM1 = dum1.meta
DUM2 = dum2.meta
TMPC = tmpc.meta
FINALC = finalc.meta

all: test

test::
	$(ICALREAD) calendar.ics $(TMPC)
	$(HARMONY) -ar $(TMPC) -lensar ICalendar.l \
		   -r1 $(TMPC) -lensr1 ICalendar.l \
		   -r2 $(TMPC) -lensr2 ICalendar.l \
		   -newr1 $(FINALC) -newar $(DUM1) -newr2 $(DUM2) \
		   -schema ICalendar.ICalendar_A
	echo
	$(ICALWRITE) $(FINALC) updated.ics
	cat $(FINALC)

clean::
	rm $(TMPC) $(DUM1) $(DUM2) updated.ics $(FINALC)