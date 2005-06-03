####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Common.Makefile 121 2005-05-05 00:19:32Z bcpierce $

TOP = ..
include $(TOP)/Top.Makefile


######## Needs updated!! ########

SRC = prelude.src
FCL = $(subst .src,.fcl, $(SRC))
TEX = $(subst .src,.tex, $(SRC))

all: $(FCL)

%.fcl : %.src $(SRC2F)
	$(SRC2F) $< $@

%.tex : %.src $(SRC2TEX)
	$(SRC2TEX) $< > $@

temp:
	$(MAKE) -C $(TOOLS)  
	-rm prelude.fcl
	$(MAKE) prelude.fcl

$(SRC2F):
	$(MAKE) -C $(TOOLS)

clean::
	rm -f $(FCL)

