include ../Common.mk

###
#  Common Harmony stuff to build ie6util
###
SRC = $(shell pwd)/../../src
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES) ie6util.ml
RESULT = ie6util


SAFARI_PLIST= /Users/nate/Library/Safari/Bookmarks.plist
IN-FOLDER = IE6BMSample
IN-TAR = IE6BMSample.tar.gz
OUT-FOLDER = IE6BMNew
OUT-TAR = IE6BMNew.tar.gz

TRASH := $(TRASH) safari-pre-in.xml safari-in.xml safari-out.xml \
        ie-out.meta arch-in.meta arch-out.meta \
	 $(IN-FOLDER) $(OUT-FOLDER) $(OUT-TAR) ie-in.meta \
	 *.o *.cm* ie6util

test: 
	@echo "Welcome to the Harmony Demo"

clean2: 
	rm -fr $(TRASH)

# Preprocess Safari input
safari-in.meta: $(SAFARI_PLIST)
	plutil -convert xml1 \
		-o safari-pre-in.xml \
		$(SAFARI_PLIST);
	tail +3 safari-pre-in.xml > safari-in.xml;
	$(HARMONY) -I lenses get -l Safari.l3 -c safari-in.xml -o safari-in.meta

# Preprocess IE input
$(IN-FOLDER):
	tar zxvf IE6BMSample.tar.gz

ie-in.meta: native-code $(IN-FOLDER)
	./ie6util get $(IN-FOLDER) ie-pre-in.meta
	$(HARMONY) -I lenses get -l IExplorer.l3 -c ie-pre-in.meta -o ie-in.meta

# Synchronization
sync: safari-in.meta ie-in.meta 
	$(HARMONY) -I lenses sync \
		-schema Schemas.BushAbstract \
		-archive arch-in.meta \
		-replica1 safari-in.meta \
		-replica2 ie-in.meta \
		-lensar Prelude.id -lensr1 Prelude.id -lensr2 Prelude.id \
		-newreplica1 safari-out.meta \
		-newreplica2 ie-out.meta \
		-newarchive arch-out.meta

# Postprocess Safari output
safari-out.xml:
	$(HARMONY) -I lenses put -l Safari.l3 -a safari-out.meta -c safari-in.meta -o safari-out.xml

update-safari: safari-out.xml
	plutil -convert binary1 -o $(SAFARI_PLIST) safari-out.xml

# Postprocess IE output

$(OUT-FOLDER): ie6util ie-out.meta
	$(HARMONY) -I lenses put -l IExplorer.l3 -a ie-out.meta -c ie-in.meta -o ie-out2.meta
	./ie6util put ie-out2.meta $(OUT-FOLDER) 

update-ie: $(OUT-FOLDER)
	tar zcvf IE6BMNew.tar.gz $(OUT-FOLDER)
	scp IE6BMNew.tar.gz jnfoster@seas.upenn.edu:html/tmp/


include $(SRC)/Common.Makefile