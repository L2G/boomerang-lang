TOP = ../..
include $(TOP)/Top.Makefile

ICALREAD = $(TOP)/extern/iCalendar/iCalViewer ascal
ICALWRITE = $(TOP)/extern/iCalendar/iCalViewer asmeta
TMPA = tmpa.meta
TMPC = tmpc.meta
FINALC = finalc.meta

all: test

test::
	$(ICALREAD) calendar.ics $(TMPC)
	$(HARMONY) get -c $(TMPC) -o $(TMPA) -l ICalendar.l
	cat $(TMPA)
	echo
	echo
	$(HARMONY) put -a $(TMPA) -c $(TMPC) -o $(FINALC) -l ICalendar.l
	$(ICALWRITE) $(FINALC) updated.ics
	cat $(FINALC)

clean::
	rm $(TMPA) $(TMPC) updated.ics $(FINALC)