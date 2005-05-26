include ../Common.mk
OCAMLC=ocamlc
OCAMLLEX=ocamllex

all: vcard2xml testget testput vcard2vcf

test: testget testput

testget: vcard2xml
	$(HARMONY) \
	     get -concrete vcard.xml -output outget.meta -lens Lens.l \
	     && echo && cat outget.meta

testput:
	$(HARMONY) \
	     put -concrete vcard.xml -abstract newabstract.meta \
	     -output output.xml -lens Lens.l \
	     && echo && cat output.xml

# view: viewxml viewvcf 

# viewxml: vcard.vcf
# 	./vcard2xml vcard.vcf > vcard.xml

# viewvcf: vcard.xml
# 	./vcard2vcf vcard.xml > newvcard.vcf

vcard2xml: vcard2xml.cmo
	$(OCAMLC) str.cma -o vcard2xml vcard2xml.cmo
	./vcard2xml vcard.vcf > vcard.xml

vcard2vcf: vcard2vcf.cmo output.xml
	$(OCAMLC) str.cma -o vcard2vcf vcard2vcf.cmo
	./vcard2vcf output.xml > newvcard.vcf

clean_all:
	/bin/rm -f *~ *.cmo *.cmi vcard2vcf vcard2xml newvcard.vcf vcard.xml outget.meta output.xml 

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .mll

.mll.ml:
	${OCAMLLEX} $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLC} -c $<
