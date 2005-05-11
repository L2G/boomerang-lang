all: buildandtest

include ../Common.mk

test: testget 

testget: 
	$(HARMONY) \
	     get -concrete concrete.xml -output out.tmp:meta -lens Safari.l \
	     && echo && cat out.tmp




