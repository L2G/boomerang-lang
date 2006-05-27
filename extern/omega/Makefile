BASEDIR =.

all: 
	$(MAKE) -C ocaml

clean:: 
	$(MAKE) -C ocaml clean; 
	$(MAKE) -C omega_lib/obj veryclean

SUBMAKE=$(MAKE)

include $(BASEDIR)/Makefile.rules

