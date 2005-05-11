all: buildandtest

include ../Common.mk

include Mozilla.mk

test: testsafget testmozget

testsafget: 
	$(HARMONY) \
	     get -concrete concrete.xml -output out.tmp:meta -lens Safari.l \
	     && echo && cat out.tmp

testmozget: preprocess
	$(HARMONY) \
	     get -concrete $(MOZ_TMP_FILE) -o $(MOZ_OUTPUT_FILE) -l Mozilla_bookmark.l \
	     && echo && cat $(MOZ_OUTPUT_FILE)



