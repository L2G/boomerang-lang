all: buildandtest

include ../Common.mk

include Mozilla.mk

test: testsafari testmozilla


testmozilla: testmozput

testsafari: testsafput

testsafget: 
	$(HARMONY) \
	     get -concrete concrete.xml -output out.tmp:meta -lens Safari.l2 \
	     && echo && cat out.tmp && echo

testsafput: testsafget
	$(HARMONY) \
	     put -concrete concrete.xml -abstract out.tmp:meta -lens Safari.l2 -output getput.xml \
	     && echo && cat getput.xml

testmozput: testmozget
	$(HARMONY) \
	     put -concrete $(MOZ_TMP_FILE) -abstract $(MOZ_OUTPUT_FILE) -l $(MOZILLA_LENS) -o $(MOZ_GETPUT_FILE) \
	     && echo && cat $(MOZ_GETPUT_FILE)
	-tidy -f /dev/null -ashtml $(MOZ_GETPUT_FILE) > $(MOZ_TEST_OUT)

testmozget: preprocess
	$(HARMONY) \
	     get -concrete $(MOZ_TMP_FILE) -o $(MOZ_OUTPUT_FILE) -l $(MOZILLA_LENS) \
	     && echo && cat $(MOZ_OUTPUT_FILE) && echo



