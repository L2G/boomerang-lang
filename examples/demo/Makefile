
include ../Common.mk

# HARMONY STUFF
SRC = $(shell pwd)/../../src
RESULT = ie6util
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES) ie6util.ml
IN-FOLDER = ./IE6BMSample
OUT-FOLDER = ./IE6BMNew

SAFARI_PLIST=/Users/nate/Library/Safari/Bookmarks.plist

TRASH := $(TRASH) safari-pre-in.xml safari-in.xml safari-out.xml \
        ie-out.meta arch-in.meta arch-out.meta \
	 $(IN-FOLDER) $(OUT-FOLDER) ie-in.meta \
	 *.o *.cm* ie6util

test: 
	@echo "Welcome to the Harmony Demo"	

all-clean: 
	rm -fr $(TRASH)

# Preprocess Safari input
safari-in.xml: $(SAFARI_PLIST)
	plutil -convert xml1 \
		-o safari-pre-in.xml \
		$(SAFARI_PLIST);
	tail +3 safari-pre-in.xml > safari-in.xml;

# Preprocess IE input
$(IN-FOLDER):
	tar zxvf IE6BMSample.tar.gz

ie-in.meta: native-code $(IN-FOLDER)
	./ie6util get $(IN-FOLDER) ie-in.meta

# Sync
sync: safari-in.xml ie-in.meta 
	$(HARMONY) -I lenses sync \
		-schema Schemas.BushAbstract \
		-archive arch-in.meta \
		-lensar Prelude.id \
		-replica1 safari-in.xml \
		-lensr1 Safari.l3 \
		-replica2 ie-in.meta \
		-lensr2 IExplorer.l3 \
		-newreplica1 safari-out.xml \
		-newreplica2 ie-out.meta \
		-newarchive arch-out.meta

# Postprocess Safari output
update-safari: 
	plutil -convert binary1 -o $(SAFARI_PLIST) safari-out.xml

# Postprocess IE output
update-ie: $(OUT-FOLDER)
	tar zcvf IE6BMNew.tar.gz $(OUT-FOLDER)
	scp IE6BMNew.tar.gz jnfoster@seas.upenn.edu:html/tmp/

$(OUT-FOLDER): ie6util ie-out.meta
	./ie6util put ie-out.meta $(OUT-FOLDER) 

include $(SRC)/Common.Makefile