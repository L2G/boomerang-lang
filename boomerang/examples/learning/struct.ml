open Oracle

(* for re-export *)
type chunk = Oracle.chunk
type chunks = Oracle.chunks

type t =
    (* a structured sequence of fields *)
    Structure of (chunks * t) list
      
    (* an unstructured homogenous list *)
  | Sequence of chunks * t

    (* a union of possible variants *)
  | Union of (chunks * t) list

    (* a regular expression over strings *)
  | Regex of chunks * string

    (* a constant string *)
  | Constant of string

    (* nothing, the empty string, e.g., Structure [] *)
  | Empty

    (* failure, e.g., Union [] *)
  | Void

(* log(|t|), the log of the number of different sorts of type
   constructors *)
let t_card : float = log 7.0

let rec equal s1 s2 =
  let each_equal ss1 ss2 =
    List.for_all2 (fun (_, s1) (_, s2) -> equal s1 s2) ss1 ss2
  in
  match s1, s2 with
      Structure f1, Structure f2 -> each_equal f1 f2
    | Sequence (_, s1), Sequence (_, s2) -> equal s1 s2
    | Union v1, Union v2 -> each_equal v1 v2
    | Regex (_, r1), Regex (_, r2) -> r1 = r2
    | Constant s1, Constant s2 -> s1 = s2
    | Empty, Empty -> true
    | Void, Void -> true
    | _ -> false

(* given equal s1 s2, this combines the chunk information that they share *)
let rec merge s1 s2 =
  match s1, s2 with
      Structure f1, Structure f2 ->
	Structure (List.map2 (fun (cs1, s) (cs2, _) -> (cs1 @ cs2, s)) f1 f2)
    | Sequence (cs1, s), Sequence (cs2, _) -> Sequence (cs1 @ cs2, s)
    | Union v1, Union v2 ->
	Union (List.map2 (fun (vs1, s) (vs2, _) -> (vs1 @ vs2, s)) v1 v2)
    | Regex (cs1, r1), Regex (cs2, _) -> Regex (cs1 @ cs2, r1)
	(* everything else doesn't hold on to chunks *)
    | _ -> s1

let to_string s =
  let rec to_string s indent =
    let ind str = indent ^ str in
      match s with
	| Structure [] -> ind "{}"
	| Structure ss ->
	    let strs =
	      List.map
		(fun (_, s) -> to_string s (indent ^ "  "))
		ss
	    in
	      (ind "{\n") ^ 
		(String.concat ",\n" strs) ^
		"\n" ^ (ind "}")
	| Sequence (_, s) ->
	    (ind "[\n") ^ (to_string s (indent ^ " ")) ^ "]"
	| Union ss ->
	    let strs = 
	      List.map
		(fun (_, s) ->
		   (ind "| ") ^ (to_string s (indent ^ "  ")))
		ss
	    in
	      String.concat "\n" strs
	| Regex (_, re) -> (ind "/") ^ re ^ "/"
	| Constant s -> (ind "'") ^ s ^ "'"
	| Empty -> (ind "Empty")
	| Void -> (ind "Void")
  in
    to_string s ""

let rec tokens s =
  let concat_unique lss =
    let rec merge ls1 ls2 =
      List.fold_left 
	(fun ls2 v ->
	   if List.mem v ls2
	   then ls2
	   else v::ls2)
	ls2 ls1
    in
      List.fold_left merge [] lss
  in
  match s with
      Structure fields ->
	let ts = List.map (fun (_, s) -> tokens s) fields in
	  concat_unique ts
    | Sequence (_, s) -> tokens s
    | Union variants ->
	let ts = List.map (fun (_, s) -> tokens s) variants in
	  concat_unique ts
    | Regex (_, name) -> [name]
    | Constant _
    | Empty
    | Void -> []

let cost s =
  let for_each f css =
    List.fold_left (fun sum (_, sub_s) -> sum +. (f sub_s)) 0.0 css
  in
  let rec cd s = 
    match s with
	Structure fields -> (for_each cd) fields
      | Sequence (_, elt_s) -> 1.0 +. (cd elt_s)
      | Union [] -> 0.0
      | Union [(_, v)] -> cd v
      | Union variants -> 
	  (log (float_of_int (List.length variants))) +. ((for_each cd) variants)
      | Regex (_, re_name) -> float_of_int (Regexp.regexp_length re_name)
      | Constant _ -> 0.0
      | Empty
      | Void -> 0.0 (* to encourage the transformation into these types *)
  in
  let rec ct s =
    t_card +. match s with
	Structure fields -> (for_each ct) fields
      | Sequence (_, elt_s) -> ct elt_s
      | Union variants -> (for_each cd) variants
      | Regex (_, re_name) -> float_of_int (Regexp.regexp_length re_name)
      | Constant s -> float_of_int (String.length s)
      | Empty
      | Void -> 0.0
  in
    (ct s) +. (cd s)

let lowest_cost ss =
  match ss with
      hd::ss ->
	List.fold_left
	  (fun (min_score, min_s) s ->
	     let score = cost s in
	       if score < min_score
	       then (score, s)
	       else (min_score, min_s))
	  (cost hd, hd)
	  ss
    | [] -> failwith "Can't compute lowest cost of empty list -- why is struct rewriting without any rules?"

(**
   convert a prophecy into a structured type
*)
let rec discover cs =
  let discover_each css =
    List.map (fun cs -> cs, discover cs) css
  in
    match (prophesy cs) with
	BaseProphecy (Lex.RegexToken (_, regex)) -> 
	  Regex (cs, regex)

      | EmptyProphecy -> Structure []
	    
      | StructProphecy fields ->
	  Structure (discover_each fields)
	    

      | ListProphecy (pre, body, post) ->
	  let pre_t = discover pre in
	  let body_t = discover body in
	  let post_t = discover post in
	    Structure [(pre, pre_t); 
		       (* we give nothing as the structs for the whole
			  body, since we only have samples of individual
			  elements, not the whole body -- we give those
			  samples as the chunks in the sequence *)
		       ([], Sequence (body, body_t));
		       (post, post_t)]
	      
      | UnionProphecy variants ->
	  Union (discover_each variants)
	    
      | _ -> Constant ""

type rewrite_rule = t -> t

let singleton_struct s =
  match s with
      Structure [(_, sub_s)] -> sub_s
    | _ -> s

let empty_struct s =
  match s with
      Structure [] -> Empty
    | _ -> s

let singleton_union s =
  match s with
      Union [(_, sub_s)] -> sub_s
    | _ -> s

let void_union s =
  match s with
      Union [] -> Void
    | _ -> s

let trivial_fields s =
  match s with
      Structure fields ->
	Structure (List.filter (fun (_, s) -> s <> Empty) fields)
    | _ -> s

let void_variants s =
  match s with
      Union variants ->
	Union (List.filter (fun (_, s) -> s <> Void) variants)
    | _ -> s

let void_fields s =
  match s with
      Structure fields when List.exists (fun (_, s) -> s = Void) fields -> Void
    | _ -> s

let split_suffix (ls : 'a list) : 'a * 'a list=
  let rec last suffix prefix =
    match suffix with
	[] -> raise Not_found
      | [suffix] -> suffix, List.rev prefix
      | v::suffix -> last suffix (v::prefix)
  in
    last ls []

(* given union { struct { ... T }; struct { ... T} ... }, we want to rewrite:

   struct { union { struct { ... }; struct { ... } ... } T }

   this could be more robust, e.g., if T = Constant "foo", then we'd like to rewrite:

   union { struct { ... "Foo" }; "Foo" } as:

   struct { union { struct { ... }; Empty } "Foo" }
*)
let common_variant_suffix s =
  match s with
      Union variants ->
	let suffixes, prefixes = 
	  List.split 
	    (List.map 
	       (fun (_, s) -> 
		  match s with
		      Structure fields -> split_suffix fields
		    | _ -> raise Not_found)
	       variants)
	in
	  begin
	    match List.split suffixes with
		suffix_chunks, hd::suffixes ->
		  if List.for_all (equal hd) suffixes
		  then
		    (* we have to erase all of the chunks in each prefix
		       struct, since there's no (convenient) way to remove the
		       suffix *)
		    let variants = 
		      List.map 
			(fun prefix -> 
			   Structure (List.map (fun (_, fs) -> [], fs) prefix))
			prefixes 
		    in
		      Structure [([], Union (List.map (fun v -> [], v) variants)); 
				 (List.concat suffix_chunks, hd)]
		  else s
	      | _, [] -> s
	  end
    | _ -> s

let combine_structs s =
  match s with
      Structure fields ->
	let rec combine fields =
	  match fields with
	      [] -> []
	    | (_, Structure fields')::fields -> combine (fields' @ fields)
	    | v::fields -> v::(combine fields)
	in
	  Structure (combine fields)
    | _ -> s

let combine_constants s =
  match s with
      Structure fields ->
	let rec combine fields =
	  match fields with
	      [] -> []
	    | (_, Constant s1)::(_, Constant s2)::fields ->
		combine (([], Constant (s1 ^ s2))::fields)
	    | v::fields -> v::(combine fields)
	in
	  Structure (combine fields)
    | _ -> s

let data_independent_rules : rewrite_rule list = 
  [singleton_struct; empty_struct;
   singleton_union; void_union;
   trivial_fields; void_variants; void_fields;
   common_variant_suffix;
   combine_structs;
   combine_constants]

let strings_for_chunk c =
  List.map 
    (function 
       | Lex.RegexToken (s, _) -> s
       | Lex.MetaToken _ -> failwith "No metatokens should exist at this stage") 
    c

let constant_strings s =
  match s with
      Regex (c::cs, _) ->
	let str = strings_for_chunk c in
	  if List.for_all (fun c -> str = strings_for_chunk c) cs
	  then Constant (String.concat "" str)
	  else s	  
    | _ -> s

let data_dependent_rules : rewrite_rule list = 
  [constant_strings]

let rewrite_step rules s =
  let all = List.map (fun rr -> try rr s with exn -> s) rules in
  let (top_cost, top) = lowest_cost all in
    (* if there's any _real_ improvement... *)
    if top_cost < (cost s)
    then top
    else s

let rec refine_for step s =
  let refine_each ss =
    List.map (fun (cs, s) -> (cs, refine_for step s)) ss
  in
  let s' = 
    match s with
	Structure fields -> Structure (refine_each fields)
      | Sequence (cs, sub_s) -> Sequence (cs, refine_for step sub_s)
      | Union variants -> Union (refine_each variants)
      | Regex _
      | Constant _ 
      | Empty
      | Void -> s
  in
    step s'

let refine s =
  let structure = rewrite_step data_independent_rules in
  let data = rewrite_step data_dependent_rules in
    refine_for structure
      (refine_for data
	 (refine_for structure s))

(* let _ =  *)
(*   let s = Structure [([], Constant "foo")] in *)
(*   let s' = refine s in *)
(*     print_endline ("TR: " ^ (to_string s) ^ " to " ^ (to_string s')); *)
(*     print_endline ((string_of_float (cost s)) ^ ", " ^ (string_of_float (cost s'))) *)
