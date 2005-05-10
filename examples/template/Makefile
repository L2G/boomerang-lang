include ../Common.mk

test: testget testput

testget: 
	$(HARMONY) \
	     get -concrete concrete.xml -output out.tmp:xml -lens Template.l \
	     && echo && cat out.tmp

testput: 
	$(HARMONY) \
	     put -concrete concrete.xml -abstract newabstract.xml \
	     -output out.tmp:xml -lens Template.l \
	     && echo && cat out.tmp



