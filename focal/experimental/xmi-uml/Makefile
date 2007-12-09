TOP = ../..
include $(TOP)/Top.Makefile

ARCH = archive.xmi:xml
IN = example2.xmi:xml
IN2 = example1.xmi:xml

OUT = output.xmi:xml

all: testxmi

get:
	$(HARMONY) $(IN) \
	-lensr1 XmiUml.l -lensar XmiUml.l \
	-schema XmiUml.Abstract

getput:
	$(HARMONY) $(ARCH) $(ARCH) -ar ar.meta \
	-lensr1 XmiUml.l -lensr2 XmiUml.l -lensar Prelude.id \
	-schema XmiUml.Abstract

sync: 
	$(HARMONY) example1.xmi:xml example2.xmi:xml \
	-ar $(ARCH) -newar $(OUT) -newr1 newex1.xmi \
	-lensr1 XmiUml.l -lensr2 XmiUml.l -lensar XmiUml.l \
	-schema XmiUml.Abstract

clean::
	rm -f *$(OUT)* new*.xmi
	cp -f archive.xmi example1.xmi
	cp -f archive.xmi example2.xmi
