TOP = ../..
include $(TOP)/Top.Makefile

IN = myfirst.xmi:xml

OUT = output.meta

all: test

test::
	$(HARMONY) get -c $(IN) -o $(OUT) -l XmiUml.l -I $(TOP)/lenses
	cat $(OUT)

clean::
	rm $(OUT)
