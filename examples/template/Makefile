####################################################################
# The Harmony Project                                              #
# harmony@lists.seas.upenn.edu                                     #
####################################################################

# $Id: Makefile 121 2005-05-05 00:19:32Z bcpierce $ 

TOP = ../..
include $(TOP)/Top.Makefile

test:: testget testput

testget: 
	$(HARMONY) \
	     get -concrete concrete.xml -output out.tmp:xml -lens Template.l \
	     && echo && cat out.tmp

testput: 
	$(HARMONY) \
	     put -concrete concrete.xml -abstract newabstract.xml \
	     -output out.tmp:xml -lens Template.l \
	     && echo && cat out.tmp



