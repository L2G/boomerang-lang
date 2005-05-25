OCAMLC=ocamlc
OCAMLLEX=ocamllex

all: vcard

vcard: vcard.cmo
	$(OCAMLC) str.cma -o vcard vcard.cmo

clean:
	/bin/rm -f *~ *.cmo *.cmi vcard vcard.ml 

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .mll

.mll.ml:
	${OCAMLLEX} $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLC} -c $<
