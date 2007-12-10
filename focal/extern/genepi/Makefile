####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################
# $Id$

SUBDIRS = ocaml
SUBDIRSCLEANONLY = src

GENEPIDIR=.
BUILDDIR=$(GENEPIDIR)/build
SRCDIR=$(GENEPIDIR)/src

all:
	$(MAKE) -C $(SRCDIR); \
  if [ $$? -ne 0 ]; then res=1; break; fi; \
  exit $$res

clean:
	rm -fr $(BUILDDIR); \
	$(MAKE) -C ocaml clean || exit $$?