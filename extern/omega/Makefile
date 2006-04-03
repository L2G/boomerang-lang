# Makefile for release 1.0 of the omega system
# DON'T EDIT -- put any locally required changes in Makefile.config.
# See INSTALL for more details.

# Top-level makefile: lists possible targets, call those makefiles, install
#  target

default: oc

all: oc 

BASEDIR =.

oc: always
	cd omega_calc/obj; $(MAKE) oc

tables: 
	cd petit/obj; $(MAKE) .tables

petit: tables always
	cd petit/obj; $(MAKE) petit

libomega.a: always 
	cd omega_lib/obj; $(MAKE) libomega.a

libcode_gen.a: always
	cd code_gen/obj; $(MAKE) libcode_gen.a

libuniform.a: tables always 
	cd uniform/obj; $(MAKE) libuniform.a

install: install_all
depend: depend_all
clean: veryclean_all
veryclean: veryclean_all

SUBMAKE=$(MAKE)

include $(BASEDIR)/Makefile.rules
