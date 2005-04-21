main: parsertypes.mli parsertypes.ml vcardparser.mly vcardlex.mll
	ocamlc  -I ../.. parsertypes.mli
	ocamlc -c -I ../.. parsertypes.ml
	ocamlyacc vcardparser.mly
	ocamllex -ml vcardlex.mll
	ocamlc -I ../.. vcardparser.mli
	ocamlc -c -I ../.. vcardparser.ml
	ocamlc -c -I ../.. vcardlex.ml
	ocamlc -c -I ../.. main.ml
	ocamlc -o convertit -I ../.. -I ../../ubase unix.cma safelist.cmo \
		uprintf.cmo util.cmo uarg.cmo prefs.cmo trace.cmo misc.cmo mapplus.cmo \
		 name.cmo v.cmo parsertypes.cmo vcardparser.cmo vcardlex.cmo main.cmo