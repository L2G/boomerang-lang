TOP = ../..
OCAMLC=ocamlc
OCAMLLEX=ocamllex
include $(TOP)/Top.Makefile

all: xml-vcf

xml-xml: xml-in.meta sync-xml xml-out update-xml

vcf-vcf: vcf-in.meta sync-vcf vcf-out update-vcf

xml-vcf: xml-in.meta vcf-in.meta sync-xml-vcf xml-out1 vcf-out1 update-xml1 update-vcf1

test::
	@echo "Welcome to Vcard demo"

####
## Preprocess Vcard.vcf input
####

vcf-in.meta: vcf-in.xml
	$(HARMONY) \
	     get -concrete vcf-in1.xml -output vcf-in1.meta -lens Lens.vcf \
	     && echo && cat vcf-in1.meta
	$(HARMONY) \
	     get -concrete vcf-in2.xml -output vcf-in2.meta -lens Lens.vcf \
	     && echo && cat vcf-in2.meta

vcf-in.xml: vcf2xml.cmo vcard*.vcf
	$(OCAMLC) str.cma -o vcf2xml vcf2xml.cmo
	./vcf2xml vcard1.vcf > vcf-in1.xml
	./vcf2xml vcard2.vcf > vcf-in2.xml
####
## Preprocess Vcard.xml input
####

xml-in.meta: xml-in.xml
	$(HARMONY) \
	     get -concrete xml-in1.xml -output xml-in1.meta -lens Lens.xml \
	     && echo && cat xml-in1.meta
	$(HARMONY) \
	     get -concrete xml-in2.xml -output xml-in2.meta -lens Lens.xml \
	     && echo && cat xml-in2.meta

xml-in.xml: decomposition.cmo xml2view.cmo vcard*.xml
	$(OCAMLC) -o xml2view decomposition.cmo xml2view.cmo
	./xml2view vcard1.xml > xml-in1.xml
	./xml2view vcard2.xml > xml-in2.xml

####
## Synchronization
####

######
## Sync xml and xml
######
sync-xml: xml-in1.meta xml-in2.meta
	$(HARMONY) sync \
		-schema Schemas.Vcard \
		-archive arch-inXML.meta \
		-replica1 xml-in1.meta \
		-replica2 xml-in2.meta \
		-lensar Prelude.id -lensr1 Prelude.id -lensr2 Prelude.id \
		-newreplica1 xml-out1.meta \
		-newreplica2 xml-out2.meta \
		-newarchive arch-outXML.meta

######
## Sync vcf and vcf
######
sync-vcf: vcf-in1.meta vcf-in2.meta
	$(HARMONY) sync \
		-schema Schemas.Vcard \
		-archive arch-inVCF.meta \
		-replica1 vcf-in1.meta \
		-replica2 vcf-in2.meta \
		-lensar Prelude.id -lensr1 Prelude.id -lensr2 Prelude.id \
		-newreplica1 vcf-out1.meta \
		-newreplica2 vcf-out2.meta \
		-newarchive arch-outVCF.meta

######
## Sync xml and vcf
######
sync-xml-vcf: xml-in1.meta vcf-in1.meta 
	$(HARMONY) sync \
		-schema Schemas.Vcard \
		-archive arch-in.meta \
		-replica1 xml-in1.meta \
		-replica2 vcf-in1.meta \
		-lensar Prelude.id -lensr1 Prelude.id -lensr2 Prelude.id \
		-newreplica1 xml-out1.meta \
		-newreplica2 vcf-out1.meta \
		-newarchive arch-out.meta


sync2-prep:
	cp arch-outXML.meta arch-inXML.meta
	cp arch-outVCF.meta arch-inVCF.meta
	cp arch-out.meta arch-in.meta

####
## Postprocess Vcard.vcf output
####

vcf-out: vcf-out1 vcf-out2

vcf-out1: vcf-out1.meta
	$(HARMONY) \
	     put -concrete vcf-in1.xml -abstract vcf-out1.meta \
	     -output vcf-out1.xml -lens Lens.vcf \
	     && echo && cat vcf-out1.xml

vcf-out2: vcf-out2.meta
	$(HARMONY) \
	     put -concrete vcf-in2.xml -abstract vcf-out2.meta \
	     -output vcf-out2.xml -lens Lens.vcf \
	     && echo && cat vcf-out2.xml

update-vcf: update-vcf1 update-vcf2

update-vcf1: xml2vcf.cmo vcf-out1.xml 
	$(OCAMLC) str.cma -o xml2vcf xml2vcf.cmo
	./xml2vcf vcf-out1.xml > newVcard1.vcf

update-vcf2: xml2vcf.cmo vcf-out2.xml 
	$(OCAMLC) str.cma -o xml2vcf xml2vcf.cmo
	./xml2vcf vcf-out2.xml > newVcard2.vcf


####
## Postprocess Vcard.xml output
####

xml-out: xml-out1 xml-out2

xml-out1: xml-out1.meta 
	$(HARMONY) \
	     put -concrete xml-in1.xml -abstract xml-out1.meta \
	     -output xml-out1.xml -lens Lens.xml \
	     && echo && cat xml-out1.xml

xml-out2: xml-out2.meta
	$(HARMONY) \
	     put -concrete xml-in2.xml -abstract xml-out2.meta \
	     -output xml-out2.xml -lens Lens.xml \
	     && echo && cat xml-out2.xml

update-xml: update-xml1 update-xml2

update-xml1: view2xml.cmo xml-out1.xml 
	$(OCAMLC) -o view2xml view2xml.cmo
	./view2xml xml-out1.xml > newVcard1.xml

update-xml2: view2xml.cmo xml-out2.xml 
	$(OCAMLC) -o view2xml view2xml.cmo
	./view2xml xml-out2.xml > newVcard2.xml

####
## clean
####

clean::
	/bin/rm -f *~ *.cmo *.cmi \
	           vcf2xml xml2vcf xml2view view2xml\
	           vcf-in*.xml vcf-in*.meta vcf-out*.meta vcf-out*.xml newVcard*.vcf \
	           xml-in*.xml xml-in*.meta xml-out*.meta xml-out*.xml newVcard*.xml \
	           arch-*.*

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .mll

.mll.ml:
	${OCAMLLEX} $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLC} -c $<
