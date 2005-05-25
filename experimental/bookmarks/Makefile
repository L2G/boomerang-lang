all: buildandtest

include ../Common.mk

include Mozilla.mk
include IExplorer6.mk

test: testsafari testmozilla testie


testmozilla: testmozput

testsafari: testsafput

testie: testieput

testsafget: bigconcrete.xml safari.fcl plist.fcl schemas.fcl
	$(HARMONY) \
	     get -concrete bigconcrete.xml -output out.tmp:meta -lens Safari.l3 \
	     && echo && cat out.tmp && echo

testsafput: testsafget
	$(HARMONY) \
	     put -concrete bigconcrete.xml -abstract out.tmp:meta -lens Safari.l3 -output getput.xml \
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
	     sync -newreplica2 newr2.xml -newreplica1 newr1.xml -newarchive newar.meta -lensr2 Mozilla_bookmark.l3 -lensr1 Safari.l3 -lensar Prelude.id -schema Schemas.BushAbstract -replica2 dummy.xml -replica1 bigconcrete.xml -archive arch.meta

	$(HARMONY) \
	     sync -newreplica2 dnewr2.xml -newreplica1 dnewr1.xml -newarchive dnewar.meta -lensr2 Mozilla_bookmark.l3 -lensr1 Safari.l3 -lensar Prelude.id -schema Schemas.BushAbstract -replica2 bookmarks.xml -replica1 dummy.xml -archive arch.meta

	$(HARMONY) \
	     sync -newreplica2 mixed2.xml -newreplica1 mixed1.xml -newarchive mixedar.meta -lensr2 Mozilla_bookmark.l3 -lensr1 Safari.l3 -lensar Prelude.id -schema Schemas.BushAbstract -replica2 bookmarks.xml -replica1 bigconcrete.xml -archive arch.meta	

	$(HARMONY) \
	     sync -newreplica2 maximixed2.meta -newreplica1 maximixed1.xml -newarchive maximixedar.meta -lensr2 IExplorer.l3 -lensr1 Safari.l3 -lensar Prelude.id -schema Schemas.BushAbstract -replica2 ie-concrete.meta -replica1 mixed1.xml -archive arch.meta

	$(HARMONY) \
	     sync -newreplica2 megamixed2.xml -newreplica1 megamixed1.xml -newarchive megamixedar.meta -lensr2 Mozilla_bookmark.l3 -lensr1 Safari.l3 -lensar Prelude.id -schema Schemas.BushAbstract -replica2 mixed2.xml -replica1 maximixed1.xml -archive mixedar.meta