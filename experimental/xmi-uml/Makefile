TOP = ../..
include $(TOP)/Top.Makefile

IN = myfirst.xmi:xml

OUT = output.meta

all: testxmi

testxmi:
	$(HARMONY) $(IN) -lensr1 XmiUml.l \
	-lensar XmiUml.l -I $(TOP)/lenses

clean::
	rm $(OUT)
