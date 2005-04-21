OCAMLMAKEFILE = $(SRC)/OCamlMakefile

OCAMLC = ocamlfind ocamlc -dtypes -package "pxp,pxp-engine,pxp-lex-iso88591,netstring,unix,str" 
OCAMLOPT = ocamlfind ocamlopt -dtypes -package "pxp,pxp-engine,pxp-lex-iso88591,netstring,unix,str" 
OCAMLMKTOP = ocamlfind ocamlmktop -dtypes -package "pxp,pxp-engine,pxp-lex-iso88591,netstring,unix,str" 

CONFIG = $(SRC)/config.mk
-include $(CONFIG)

GENERATED_SOURCES = $(SRC)/lib/tidy/tidy.ml $(SRC)/lib/ubase/uprintf.ml 

all: $(CONFIG) $(GENERATED_SOURCES) native-code

$(CONFIG): 
	(cd $(SRC); ./configure)

tags:
	etags $(SOURCES)

all-clean: clean
	rm -f $(CONFIG) $(SRC)/config.log $(SRC)/config.status

#########################
# BASE/COMPILER SOURCES #
#########################

BASE_SOURCE = misc.ml mapplus.ml name.ml v.ml prd.ml lens.ml error.ml

COMPILER_SOURCE = syntax.ml parser.mly lexer.mll library.ml tutil.ml types.ml \
                  checker.ml compiler.ml

OPTOMETRIST_SOURCE = surveyor.ml optometrist.ml

#######
# LIB #
#######

ifeq ($(OCAML_GEQ_308),1)
UBASE_LIB_SOURCES = safelist.ml uprintf_new.ml uprintf.ml util.ml \
                    uarg.ml prefs.ml trace.ml
else
UBASE_LIB_SOURCES = safelist.ml uprintf_old.ml uprintf.ml util.ml \
                    uarg.ml prefs.ml trace.ml
endif

ifeq ($(WITH_TIDY),1)
TIDY_LIB_SOURCES:=tidylib_stub.c tidylib_ocaml.ml with_tidy.ml tidy.ml 
else
TIDY_LIB_SOURCES:=without_tidy.ml tidy.ml
endif

ICALENDAR_LIB_SOURCES := \
  iCalendar_syntax.ml iCalendar_print.ml iCalendar_lextypes.ml \
  iCalendarparse.mly iCalendarlex.mll iCalendar.mli iCalendar.ml

###################
# GENERATED FILES #
###################

$(SRC)/lib/ubase/uprintf.ml: $(CONFIG)
ifeq ($(OCAML_GEQ_308),1)
	@echo "include Uprintf_new" > $(SRC)/lib/ubase/uprintf.ml
else
	@echo "include Uprintf_old" > $(SRC)/lib/ubase/uprintf.ml
endif

$(SRC)/lib/tidy/tidy.ml: $(CONFIG)
ifeq ($(WITH_TIDY),1)
	@echo "include With_tidy" > $(SRC)/lib/tidy/tidy.ml
else
	@echo "include Without_tidy" > $(SRC)/lib/tidy/tidy.ml
endif

###########
# PLUGINS #
###########

PERVASIVES_PLUGIN_SOURCE = pervasives_plugin.ml
EXPERIMENTAL_PLUGIN_SOURCE = experimental_plugin.ml

ALIGN_PLUGIN_SOURCE = lockfile.ml perstrbij.ml align.ml key_factory.ml

SCHEMAS_PLUGIN_SOURCE = schemas.ml

META_PLUGIN_SOURCE = meta_plugin.ml

PLAIN_PLUGIN_SOURCE = plain.ml

STRUCTURED_TEXT_PLUGIN_SOURCE = structured_text_plugin.ml

ifeq ($(WITH_TIDY),1)
COMMON_CLIBS := tidy
else
COMMON_CLIBS :=
endif

XML_PLUGIN_SOURCE = xml.ml xml_plugin.ml

XMLARCHIVE_PLUGIN_SOURCE = xmlarchive.ml

HTML_PLUGIN_SOURCE = html.ml

MOZILLA_PLUGIN_SOURCE = bookmarks.ml mozilla_plugin.ml

SAFARI_PLUGIN_SOURCE = safari_plugin.ml

ICALENDAR_PLUGIN_SOURCE = iCalendar_plugin.ml

PALM_PLUGIN_SOURCE = palm.ml

PALMDATEBOOK_PLUGIN_SOURCE = palmdatebook.ml

PLUGINS_SOURCES = $(PERVASIVES_PLUGIN_SOURCE:%=pervasives/%) \
		  $(EXPERIMENTAL_PLUGIN_SOURCE:%=experimental/%) \
		  $(ALIGN_PLUGIN_SOURCE:%=align/%) \
                  $(SCHEMAS_PLUGIN_SOURCE:%=schemas/%) \
		  $(META_PLUGIN_SOURCE:%=meta/%) \
		  $(PLAIN_PLUGIN_SOURCE:%=plain/%) \
		  $(STRUCTURED_TEXT_PLUGIN_SOURCE:%=structured_text/%) \
		  $(XML_PLUGIN_SOURCE:%=xml/%) \
		  $(XMLARCHIVE_PLUGIN_SOURCE:%=xmlarchive/%) \
		  $(HTML_PLUGIN_SOURCE:%=html/%) \
		  $(MOZILLA_PLUGIN_SOURCE:%=mozilla/%) \
		  $(SAFARI_PLUGIN_SOURCE:%=safari/%) \
		  $(ICALENDAR_PLUGIN_SOURCE:%=iCalendar/%) \
		  $(PALM_PLUGIN_SOURCE:%=palm/%) \
		  $(PALMDATEBOOK_PLUGIN_SOURCE:%=palmdatebook/%) 

##################
# COMMON SOURCES #
##################

COMMON_SOURCES = $(UBASE_LIB_SOURCES:%=$(SRC)/lib/ubase/%) \
		 $(ICALENDAR_LIB_SOURCES:%=$(SRC)/lib/iCalendar/%) \
		 $(BASE_SOURCE:%=$(SRC)/common/base/%) \
		 $(TIDY_LIB_SOURCES:%=$(SRC)/lib/tidy/%) \
                 $(COMPILER_SOURCE:%=$(SRC)/common/compiler/%) \
                 $(OPTOMETRIST_SOURCE:%=$(SRC)/common/optometrist/%) \
		 $(PLUGINS_SOURCES:%=$(SRC)/plugins/%)

include $(SRC)/OCamlMakefile

