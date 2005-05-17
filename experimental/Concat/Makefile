include ../Common.mk

test: testget testput

testget: 
	$(HARMONY) \
	     get -concrete concrete.meta -output outget.meta -lens Concat.l \
	     && echo && cat outget.meta

testput: 
	$(HARMONY) \
	     put -concrete concrete.meta -abstract newabstract.meta \
	     -output output.meta -lens Concat.l \
	     && echo && cat output.meta



