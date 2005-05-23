OCAMLC=ocamlc
OCAMLLEX=ocamllex

all: vcard

vcard: decomposition.cmo vcard.cmo
	$(OCAMLC) str.cma -o vcard decomposition.cmo vcard.cmo

clean:
	/bin/rm -f *~ *.cmo *.cmi vcard vcard.ml decomposition.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .mll

.mll.ml:
	${OCAMLLEX} $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLC} -c $<
