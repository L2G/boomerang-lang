include ../Common.mk

test: testget testput

testget: 
	$(HARMONY) \
	     get -concrete vcard.xml -output out.meta -lens Vcard.l \
	     && echo && cat out.meta

testput: 
	$(HARMONY) \
	     put -concrete vcard.xml -abstract newabstract.meta \
	     -output out.tmp:xml -lens Vcard.l \
	     && echo && cat out.tmp



