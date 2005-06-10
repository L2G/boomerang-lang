TOP = ../..
include $(TOP)/Top.Makefile

OCAMLC=ocamlc
OCAMLLEX=ocamllex

all: testget testput vcard2vcf

test:
	@echo "Vcard vcf"

testget: vcard2xml
	$(HARMONY) \
	     get -concrete vcard1.xml -output outget1.meta -lens Lens.l \
	     && echo && cat outget1.meta
	$(HARMONY) \
	     get -concrete vcard2.xml -output outget2.meta -lens Lens.l \
	     && echo && cat outget2.meta

# Synchronization
sync: outget1.meta outget2.meta 
	$(HARMONY) sync \
		-schema Schemas.Vcard \
		-archive arch-in.meta \
		-replica1 outget1.meta \
		-replica2 outget2.meta \
		-lensar Prelude.id -lensr1 Prelude.id -lensr2 Prelude.id \
		-newreplica1 newabstract1.meta \
		-newreplica2 newabstract2.meta \
		-newarchive arch-out.meta

testput:
	$(HARMONY) \
	     put -concrete vcard1.xml -abstract newabstract1.meta \
	     -output output1.xml -lens Lens.l \
	     && echo && cat output1.xml
	$(HARMONY) \
	     put -concrete vcard2.xml -abstract newabstract2.meta \
	     -output output2.xml -lens Lens.l \
	     && echo && cat output2.xml


vcard2xml: vcard2xml.cmo vcard1.vcf vcard2.vcf
	$(OCAMLC) str.cma -o vcard2xml vcard2xml.cmo
	./vcard2xml vcard1.vcf > vcard1.xml
	./vcard2xml vcard2.vcf > vcard2.xml

vcard2vcf: vcard2vcf.cmo output.xml
	$(OCAMLC) str.cma -o vcard2vcf vcard2vcf.cmo
	./vcard2vcf output1.xml > newvcard1.vcf
	./vcard2vcf output2.xml > newvcard2.vcf


clean_all:
	/bin/rm -f *~ *.cmo *.cmi \
	           vcard2vcf vcard2xml newvcard.vcf vcard.xml outget.meta output.xml 

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .mll

.mll.ml:
	${OCAMLLEX} $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLC} -c $<
