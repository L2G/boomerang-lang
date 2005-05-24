all: buildandtest

include ../Common.mk

include Mozilla.mk

test: testsafari testmozilla


testmozilla: testmozput

testsafari: testsafput

testsafget: 
	$(HARMONY) \
	     get -concrete bigconcrete.xml -output out.tmp:meta -lens Safari.l2 \
	     && echo && cat out.tmp && echo

testsafput: testsafget
	$(HARMONY) \
	     put -concrete bigconcrete.xml -abstract out.tmp:meta -lens Safari.l2 -output getput.xml \
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


sync:
	$(HARMONY) \
	     sync -newreplica2 newr2.xml -newreplica1 newr1.xml -newarchive newar.meta -lensr2 Mozilla_bookmark.l2 -lensr1 Safari.l2 -lensar Prelude.id -schema Schemas.Abstract -replica2 dummy.xml -replica1 bigconcrete.xml -archive arch.meta

	$(HARMONY) \
	     sync -newreplica2 dnewr2.xml -newreplica1 dnewr1.xml -newarchive dnewar.meta -lensr2 Mozilla_bookmark.l2 -lensr1 Safari.l2 -lensar Prelude.id -schema Schemas.Abstract -replica2 bookmarks2.xml -replica1 dummy.xml -archive arch.meta
