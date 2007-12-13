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

SUBDIRS=ocaml src

clean:
	rm -fr $(BUILDDIR)
	@for i in $(SUBDIRS); do \
	    $(MAKE) -C $$i clean || exit $$?; done
