TOP = ../..
include $(TOP)/Top.Makefile

all: test

test::
	$(HARMONY) get -c calendar.ics -o calendar.meta -l ICalendar.l -I .
	cat calendar.meta

	echo
	echo
	$(HARMONY) put -a calendar.meta -c calendar.raw.meta -o calendar.put.meta -l ICalendar.l
	cat calendar.put.meta