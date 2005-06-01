include ../Common.mk

###
#  Common Harmony stuff to build ie6util
###
SRC = $(shell pwd)/../../src
CLIBS = $(COMMON_CLIBS)
SOURCES = $(COMMON_SOURCES) ie6util.ml
RESULT = ie6util

###
#  Files 
###
SAFARI_PLIST= /Users/nate/Library/Safari/Bookmarks.plist
IN-FOLDER = IE-in
OUT-FOLDER = IE-out

TRASH := $(TRASH) \
        safari-pre-in.xml safari-in.xml safari-in.meta safari-out.meta safari-out.xml \
        ie-pre-in.meta ie-in.meta ie-out.meta ie-pre-out.meta \
        arch-in.meta arch-out.meta \
	 $(IN-FOLDER) $(OUT-FOLDER) $(OUT-TAR) \
	 *.o *.cm* ie6util ie6util.annot

test: 
	@echo "Welcome to the Harmony Demo"

clean2: 
	rm -fr $(TRASH)

# Preprocess Safari input
safari-in.xml: $(SAFARI_PLIST)
	plutil -convert xml1 -o safari-pre-in.xml $(SAFARI_PLIST)
	tail +3 safari-pre-in.xml > safari-in.xml	

safari-in.meta: safari-in.xml
	$(HARMONY) -I lenses get -l Safari.l3 -c safari-in.xml -o safari-in.meta

# Preprocess IE input
$(IN-FOLDER):
	scp -r harmony@$(STEAK):Favoris .
	mv Favoris $(IN-FOLDER) 
	chmod -R 700 $(IN-FOLDER)

ie-pre-in.meta: native-code $(IN-FOLDER)
	./ie6util get $(IN-FOLDER) ie-pre-in.meta

ie-in.meta: ie-pre-in.meta
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
		-newreplica2 ie-pre-out.meta \
		-newarchive arch-out.meta

sync2-prep: 
	rm -fr $(IN-FOLDER) $(OUT-FOLDER)
	cp arch-out.meta arch-in.meta

# Postprocess Safari output
safari-out.xml: safari-out.meta
	$(HARMONY) -I lenses put -l Safari.l3 -a safari-out.meta -c safari-in.xml -o safari-out.xml

update-safari: safari-out.xml
	plutil -convert binary1 -o $(SAFARI_PLIST) safari-out.xml

# Postprocess IE output

ie-out.meta: ie-pre-out.meta
	$(HARMONY) -I lenses put -l IExplorer.l3 -a ie-pre-out.meta -c ie-pre-in.meta -o ie-out.meta

$(OUT-FOLDER): ie6util ie-out.meta
	./ie6util put ie-out.meta $(OUT-FOLDER) 

update-ie: $(OUT-FOLDER)
	mv $(OUT-FOLDER) Favoris
	scp -r Favoris harmony@$(STEAK):
	rm -fr Favoris

include $(SRC)/Common.Makefile