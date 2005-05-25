OCAMLC=ocamlc
OCAMLLEX=ocamllex

all: vcard2xml vcard2vcf

view: viewxml viewvcf 

viewxml: vcard.vcf
	./vcard2xml vcard.vcf > vcard.xml

viewvcf: vcard.xml
	./vcard2vcf vcard.xml > newvcard.vcf

vcard2xml: vcard2xml.cmo
	$(OCAMLC) str.cma -o vcard2xml vcard2xml.cmo

vcard2vcf: vcard2vcf.cmo
	$(OCAMLC) str.cma -o vcard2vcf vcard2vcf.cmo

clean:
	/bin/rm -f *~ *.cmo *.cmi vcard2vcf vcard2xml newvcard.vcf vcard.xml 

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .mll

.mll.ml:
	${OCAMLLEX} $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLC} -c $<
