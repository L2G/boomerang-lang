####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
#                                                                  #
# Common Makefile infrastructure			           #
####################################################################

TOP = ../..
include $(TOP)/Top.Makefile

all: test

test::
	$(HARMONY) check -module Vcard -test-all

DEMOFILES = a.xml b.xml o.xml

clean::
	rm -rf $(DEMOFILES)

demo:: 
	@$(MAKE) buildharmony
	@if [ -f a.xml ] && [ -f b.xml ]; then $(MAKE) continue-demo; else $(MAKE) init-demo; fi

init-demo:
	-rm -f $(DEMOFILES)
	cp vcard_example_2.xml a.xml
	cp a.xml b.xml
	$(MAKE) continue-demo
	@echo
	@echo Demo initialized: You can now edit a.xml and/or b.xml and 
	@echo then do 'make demo' to synchronize them.

continue-demo:
	$(HARMONY) sync -schema Xml.T \
	   -archive o.xml  -lensar Prelude.id \
	   -replica1 a.xml  -lensr1 Prelude.id \
	   -replica2 b.xml  -lensr2 Prelude.id \
	   -newarchive o.xml -newreplica1 a.xml -newreplica2 b.xml
