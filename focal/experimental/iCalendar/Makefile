TOP = ../..
include $(TOP)/Top.Makefile

ICALREAD = $(TOP)/extern/iCalendar/iCalViewer ascal
ICALWRITE = $(TOP)/extern/iCalendar/iCalViewer asmeta
DUM1 = dum1.meta
DUM2 = dum2.meta
TMPC = tmpc.meta
TMPA = tmpa.meta
TMPU = updated.meta
FINALC = finalc.meta
FINALU = finalu.meta

all: harmonize-calendars

sync:
	unison default2 -ui text

modifs:
	korganizer a/mycal.ics&
	korganizer b/mycal.ics&

harmonize-calendars: harmonize-calendars.ml
	ocamlc -o harmonize-calendars unix.cma harmonize-calendars.ml

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

update:
	echo [] > archive.meta
	$(ICALREAD) calendar.ics $(TMPC)
	$(ICALREAD) mycal.ics $(TMPU)
	$(HARMONY) -ar archive.meta -lensar ICalendar.l \
		   -r1 $(TMPU) -lensr1 ICalendar.l \
		   -r2 $(TMPC) -lensr2 ICalendar.l \
		   -newr1 $(FINALU) -newar archive.meta -newr2 $(FINALC) \
		   -schema ICalendar.ICalendar_A
	echo
	$(ICALWRITE) $(FINALC) newnewnew.ics
	cat $(FINALC)

get:
	$(ICALREAD) mycal.ics $(TMPC)
	$(HARMONY) $(TMPC) -lensr1 ICalendar.l_stamps
	echo

clean::
	rm -rf $(TMPC) $(DUM1) $(DUM2) updated.ics $(FINALC)
