####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

TOP = ../..

DEMO1 = R1ORIG=example1.xml R2FORMAT=xml
DEMO2 = R1ORIG=example2.xml

DEMO3 = R1ORIG=example3.xml
DEMO4 = R1ORIG=example4.csv 
DEMO5 = R1ORIG=example4.csv R2FORMAT=xml

all: makeharmonylib nc

include $(TOP)/Top.Makefile

RESULT = harmonize-addresses
SOURCES = $(CWD)/harmonize-addresses.ml 
LIBS += harmony

INCDIRS += $(SRCDIR) $(SRCDIR)/ubase


reinstall: addr.fcl 	
	-rm -f $(RESULT)
	$(MAKE)

clean::
	rm -rf ar*.{xml,csv} r1*.{xml,csv} r2*.{xml,csv} .harmonyar*.meta .harmonyar*.tmp
include $(OCAMLMAKEFILE)


####################################################################
# Tests

test:: buildharmony unit-tests 

unit-tests:
	$(HARMONY) -check Addr -test Addr



#############################################################################
## Old/dead:

testbug:: buildharmony
	./harmonize-addresses testbug.csv

test2:: buildharmony
	$(HARMONY) get -lens Addr.l -concrete $(HOME)/current/z/addresses.txt:xml -output -:meta

test4:: buildharmony
	$(MAKE) -C ../../extern/pilot-link-0.12.0-pre4/src pilot-addresses
	../../extern/pilot-link-0.12.0-pre4/src/pilot-addresses --read=a.csv -a

test5:: 
	$(MAKE) -C ../../extern/ocaml-csv-1.0.3
	$(MAKE) buildharmony 
	echo 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21 > temp.csv
	sed 1,1d < sample.csv >> temp.csv
	$(HARMONY) get -lens Prelude.id -concrete temp.csv -output sampleout.csv
	cat sampleout.csv

test6:: 
	$(MAKE) -C ../../extern/ocaml-csv-1.0.3
	$(MAKE) buildharmony 
	$(HARMONY) get -lens Prelude.id -concrete sample.csv -output -:meta

test7:: 
	$(MAKE) -C ../../extern/ocaml-csv-1.0.3
	$(MAKE) buildharmony 
	echo 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21 > temp.csv
	sed 1,1d < sample.csv >> temp.csv
	$(HARMONY) get -lens Addr.csv -concrete temp.csv -output -:meta

test8:: buildharmony
	cp $(HOME)/current/z/addresses.txt r1.xml
	$(HARMONY) sync -schema Addr.AddrBook \
	   -archive o.meta  -lensar Prelude.id \
	   -replica1 r1.xml  -lensr1 Addr.xcard \
	   -replica2 r2.csv  -lensr2 Addr.csv \
	   -newarchive onew.meta -newreplica1 r1new.xml  -newreplica2 r2new.csv \
	   -debug harmony

test9:: buildharmony
	cp sample.xml r1.xml
	$(HARMONY) sync -schema Addr.AddrBook \
	   -archive o.meta  -lensar Prelude.id \
	   -replica1 r1.xml  -lensr1 Addr.xcard \
	   -replica2 r2.csv  -lensr2 Addr.csv \
	   -newarchive onew.meta -newreplica1 r1new.xml  -newreplica2 r2new.csv \
	   -debug harmony
