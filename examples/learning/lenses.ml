open Struct
open Bsyntax

(* dummy location *)
let i = Info.M "learned"

let mk_id name = (i, name)
let mk_qid ids id = (ids, id)
let mk_qid_of_names names name = (List.map mk_id names, mk_id name)
let mk_unqid_of_name name = mk_qid_of_names [] name

let rec struct_to_lens s =
  match s with
      Structure [] -> struct_to_lens Empty
    | Structure [(_, s)] -> struct_to_lens s
    | Structure ((_, s)::ss) ->
	ECat (i, struct_to_lens s, struct_to_lens (Structure ss))

    | Sequence (_, s) -> EStar (i, struct_to_lens s)

    | Union [] -> failwith "empty Union"
    | Union [(_, s)] -> struct_to_lens s
    | Union ((_, s)::ss) ->
	EUnion (i, struct_to_lens s, struct_to_lens (Union ss))

    | Regex (_, name) -> EVar (i, mk_unqid_of_name name)

    | Constant s -> EString (i, Bstring.t_of_string s)

    | Empty -> EString (i, Bstring.t_of_string "")

    | Void -> failwith "cannot convert void to a lens" (* or, [] *)

let struct_to_module s mod_name l_name =
  let let_binding re_name =
    DLet (i, 
	  Bind (i, 
		mk_id re_name, 
		None (* no sort *), 
		(* ??? will this be okay?

		   if this is a delimiter regexp, this will raise Not_found
		*)
		try Regexp.regexp_to_lens re_name
		with Not_found -> failwith "ask mgree@seas.upenn.edu to fix delimiter regexp management, and send him the data that caused this problem"))
  in
  let token_decls = List.map let_binding (tokens s) in
  let lens_decl = DLet (i,
			Bind (i,
			      mk_id l_name,
			      None (* no sort *),
			      struct_to_lens s))
  in
    Mod (i, mk_id mod_name, [], token_decls @ [lens_decl])
