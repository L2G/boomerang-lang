(** a chunked input.  note that Lex.parse_chunk : Lex.chunk -> ... ->
    Oracle.chunk, effectively
*)
type chunk = Lex.token list

(** a list of chunks: a sample set *)
type chunks = chunk list

type histograms = Hist.histograms

type prophecy = 
    BaseProphecy of Lex.token
      (*              fields *)
  | EmptyProphecy
  | StructProphecy of chunks list
      (*          preamble * entries * postamble *)
  | ListProphecy of chunks * chunks * chunks
      (*             variants *)
  | UnionProphecy of chunks list

let string_of_list (ls : 'a list) (to_string : 'a -> string) l r sep =
  let ss = List.map to_string ls in
    l ^ (String.concat sep ss) ^ r

let string_of_chunk (c : chunk) : string =
  string_of_list c Lex.string_of_token "[" "]" "; "

let string_of_chunks (cs : chunks) : string =
  string_of_list cs string_of_chunk "{" "}" ", "

let string_of_chunks_list (ls : chunks list) : string =
  string_of_list ls string_of_chunks "(" ")" ", "

let string_of_prophecy p =
  match p with
      BaseProphecy t -> "BaseProphecy(" ^ (Lex.string_of_token t) ^ ")"
    | StructProphecy fields -> 
	"StructProphecy" ^ (string_of_chunks_list fields)
    | ListProphecy (preamble, entries, postamble) ->
	"ListProphecy" ^ (string_of_chunks_list [preamble;entries;postamble])
    | UnionProphecy variants -> 
	"UnionProphecy" ^ (string_of_chunks_list variants)

(* parameters of the algorithm 

   TODO bundle these up
   TODO finish correspondence with kfisher about mass/coverage
*)
let cluster_tolerance = 0.01
let mass_tolerance = 0.01
let coverage_tolerance = 0.98

(* state for passes *)
type state = { mutable histograms : histograms;
	       mutable groups : histograms list }

let empty_state () = { histograms = []; groups = [] }

type prophesy_pass = chunks -> state -> prophecy option

(** picks a value out of a list of lists, if it can *)
let pick_value (ls : ('a list) list) : 'a option =
  List.fold_left 
    (fun found ls ->
       if found = None
       then match ls with
	   [] -> None
	 | v::_ -> Some v
       else found)
    None ls

(** checks to see if every token in a list is equal to a given token *)
let all_equal t ls = List.for_all (fun t' -> Lex.token_equal t t') ls

exception All_chunks_empty

let all_chunks_equal cs =
  match pick_value cs with
      Some t ->
	if List.for_all (all_equal t) cs
	then Some t
	else None
    | None -> raise All_chunks_empty

(** this prophesy pass corresponds to the first strategy of the oracle
    in Fisher et al: 

    if each chunk contains the same regextoken, we issue a
    baseprophecy for that token.

    if each chunk contains the same metatoken, then we prophesy a
    struct with three fields: the left delimiter, the sub chunks, and
    the right delimiter (rewriting will make this prettier)
*)
let check_for_identical_chunks (cs : chunks) _ : prophecy option =
  try match all_chunks_equal cs with
      Some (Lex.RegexToken(_, name)) -> Some (BaseProphecy(Lex.RegexToken("", name)))
    | Some (Lex.MetaToken(l, _, r, name)) ->
	(* we prophesy a struct with three fields: the left delimiter,
	   the sub chunks, and the right delimiter

	   the two delimiters are represented by a single chunk each,
	   while the subchunks are taken out of each metatoken
	*)
	let left = [Lex.RegexToken(l, name)] in
	let right = [Lex.RegexToken(r, name)] in

	(* pull out the subchunks... *)
	let extract_sub_chunks chunk =
	  List.map 
	    (function Lex.MetaToken(_, sub_chunks, _, _) -> sub_chunks) 
	    chunk
	in
	let sub_chunks = List.concat (List.map extract_sub_chunks cs) in
	  Some (StructProphecy ([[left]; sub_chunks; [right]]))
    | None -> None
  with All_chunks_empty -> Some EmptyProphecy

(** this pass never produces prophecies, but prepares the histograms
    for later passes *)
let compute_histograms (cs : chunks) (s : state) =
  s.histograms <- Hist.make cs;
  None

let fits_in (h : Hist.t) (group : histograms) : bool =
  List.exists 
    (fun (_, h') -> 
       (Hist.relative_entropy h h') < cluster_tolerance)
    group

let merge_groups (g : histograms list) : histograms = List.concat g

let add_to_groups (s : state) ((t, h) : Lex.token * Hist.t) : unit =
  let matching, nonmatching = List.partition (fits_in h) s.groups in
  let merged = (t, h)::(merge_groups matching) in
    s.groups <- merged::nonmatching

(** another computational pass, that groups histograms by
    (thresholded) relative entropy *)
let group_histograms _ (s : state) =
  List.iter (add_to_groups s) s.histograms;
  None

(** this is a debug pass, just so we can see the resulting grouping *)
let show_groups _ s =
  print_newline ();
  print_endline "Grouped histograms: ";
  List.iter
    (fun hs -> 
       print_endline "BEGIN GROUP";
       List.iter (fun (t, h) ->
		    print_string "  ";
		    print_string (Lex.string_of_token t);
		    print_string ": ";
		    print_endline (Hist.to_string h))
	 hs;
       print_endline "END GROUP";)
    s.groups;
  None

let rec list_remove p ls =
  match ls with
      [] -> None, []
    | v::ls when p v -> Some v, ls
    | v::ls ->
	let result, ls = list_remove p ls in
	  result, v::ls	  

type 'a ordering = Lex.token list * 'a

let list_group (eq : 'a -> 'b -> bool) (join : 'a -> 'b option -> 'b) (ls1 : 'a list) : 'b list =
  List.fold_left
    (fun ls2 v1 ->
       let v2, ls2 = list_remove (eq v1) ls2 in
	 (join v1 v2)::ls2)
    []
    ls1

let group_tagged_list eq ls =
  list_group 
    eq 
    (fun (t, x) v ->
       match v with
	   None -> (t, [x])
	 | Some (_, xs) -> (t, x::xs))
    ls

(* computes the ordering of the tokens in the given chunk *)
let find_ordering (tokens : Lex.token list) (chunk : chunk) : chunk ordering =
  let rec find tokens chunk =
    match chunk with
	[] -> []
      | t::chunk ->
	  let matching t' = Lex.token_equal t t' in
	    match list_remove matching tokens with
		Some t', tokens -> t::(find tokens chunk)
	      | None, tokens -> find tokens chunk
  in
    (find tokens chunk), chunk

(* given the ordering [t1 t2 t3], we go through each chunk and
   prophesy the following fields:

   [everything up to t1; t1;
    everything up to t2; t2;
    everything up to t3; t3;
    everything after t3]
*)
let split_chunk_at_tokens (ordering : Lex.token list) (chunk : chunk) : chunks =
  let rec split (chunk : chunk) (ordering : Lex.token list) (current : chunk) : chunks =
    match chunk, ordering with
	[], []  -> [List.rev current]
	  
      (* this should never happen: chunks are split according to the
	 order that was found by find_ordering...how can we not be split
	 by an ordering that we found earlier? *)
      | [], _ -> failwith "chunk didn't match ordering in split_chunk_at_tokens"
	  
      (* tail of the list -- "everything after t3" *)
      | chunk, [] -> [(List.rev current) @ chunk]
	  
      (* we've matched a token in the ordering -- that's its own field *)
      | t::chunk, t'::ordering when Lex.token_equal t t' -> 
	  (List.rev current)::[t]::(split chunk ordering [])
	  
      | t::chunk, ordering -> split chunk ordering (t::current)
  in
    split chunk ordering []

(* given a list of split chunks by field like so:
   
   [[[everything up to t1]; [t1];
     [everything up to t2]; [t2];
     ...];
    [[everything up to t1]; [t1];
     [everything up to t2]; [t2];
     ...];
    ...]

   we want to produce the field list:

   [[[everything up to t1]; [everything up to t1]; ...];
    [[t1]; [t1]; ...];
    ...]
*)
let rec make_fields (structs : chunks list) num_fields : chunks list =
  if num_fields = 0
  then []
  else
    begin
      match structs with
	  []::_ -> []
	| _ -> let field : chunks = List.map List.hd structs in
	    field::(make_fields (List.map List.tl structs) (num_fields - 1))
    end

(** 
    this pass corresponds with the 3rd step of Fisher et al.:

    here we seek to find structs or unions based on certain patterns
    of residual mass and coverage
*)
let find_structs (cs : chunks) (s : state) =
  let min_rm_for_group g =
    let tagged_g = List.map (fun (t, h) -> Hist.residual_mass h, (t, h)) g in
      try List.fold_left 
	(fun min_rm (rm, _) -> min min_rm rm)
	(fst (List.hd tagged_g))
	(List.tl tagged_g)
      with exn -> failwith "Found an empty group"
  in

  (* order groups by minimum residual mass *)
  let gs_with_min_rm : (int * histograms) list = 
    List.map 
      (fun g -> min_rm_for_group g, g)
      s.groups 
  in
  let gs_by_min_rm = 
    List.sort
      (fun (rm1, _) (rm2, _) -> compare rm1 rm2)
      gs_with_min_rm
  in

  (* a group indicates a struct if all of its histograms' residual
     masses are below the mass_tolerance and coverages are above the
     coverage tolerance *)
  let struct_group (rm, g) =
    List.for_all
      (fun (t, h) ->
	 let size = float_of_int (Hist.size h) in
	 let rm = (float_of_int (Hist.residual_mass h)) /. size in
	 let cov = (float_of_int (Hist.coverage h)) /. size in
	   (rm < mass_tolerance) && (cov > coverage_tolerance))
      g
  in
    try 
      let _, g = List.find struct_group gs_by_min_rm in

      (* the group of histograms g is a good candidate for a struct.
	 each histogram in g is for some specific token, so we can use
	 the list of tokens as a guide for splitting up the struct
      *)
      let tokens = List.map (fun (t, _) -> t) g in
	(* given the list of tokens, we need to find the orderings of
	   those tokens in the list of chunks cs 

	   if there's only one ordering, we can prophecy a struct
	   if there are many orderings, we prophecy a union, one for each
	*)
      let all_orderings = List.map (find_ordering tokens) cs in
      let orderings = 
	group_tagged_list
	  (fun (ts, _) (ts', _) -> List.for_all2 Lex.token_equal ts ts')
	  all_orderings
      in
	match orderings with
	    [(ordering, cs)] -> 
	      let chunks_by_field = List.map (split_chunk_at_tokens ordering) cs in
		(* each token makes two fields: itself and the stuff
		   before it; there's also a final field of leftovers *)
	      let num_tokens = (2 * (List.length ordering)) + 1 in
	      let fields = make_fields chunks_by_field num_tokens in
		Some (StructProphecy fields)
	  | [] -> failwith "no orderings...how did i get here?"
	  | _ -> 
	      Some (UnionProphecy (List.map (fun (_, chunks) -> chunks) orderings))
    with Not_found -> None

let extract_sequence_containing tokens chunk =
  let rec extract tokens c current =
    match tokens, c with
	[], c -> (List.rev current), c
      | tokens, [] -> 
	  [], chunk (* we don't reverse current, we just return our original argument *)
      | t::tokens, c_t::c when Lex.token_equal t c_t ->
	  extract tokens c (c_t::current)
      | tokens, c_t::c ->
	  extract tokens c (c_t::current)
  in
    extract tokens chunk []

let split_chunk_with_tokens tokens chunk : (chunks * chunks * chunks) =
  let pre, chunk = extract_sequence_containing tokens chunk in
  let rec extract_subs chunk subs =
    match extract_sequence_containing tokens chunk with
	[], post -> subs, post
      |	sub, post -> extract_subs post (sub::subs)
  in
  let subs, post = extract_subs chunk [] in
    ([pre], subs, [post])

(** this is part 4 of Fisher et al's oracle

    we sort the groups in descending order by the max-coverage of the
    histograms in each group.

    we then find the first group that has _any_ histograms satisfying
    the criteria:

    * width(h) > 3
    * coverage(h)/size(h) > coverage_tolerance

    that group's tokens t1 through tn are then used to partition the
    input chunks into three parts:

    * a preamble that contains the first occurence of each token a
    * set of element subsequences, each containing one occurence of
    the identified tokens
    * a postamble, with anything left after all of the subsequences
*)
let find_lists (cs : chunks) (s : state) =
  let max_cov_for_group g =
    let tagged_g = List.map (fun (t, h) -> Hist.coverage h, (t, h)) g in
      List.fold_left
	(fun max_cov (cov, _) -> max max_cov cov)
	0
	tagged_g
  in

  (* order groups by greatest coverage of any histogram in the group *)
  let gs_with_max_cov : (int * histograms) list = 
    List.map 
      (fun g -> max_cov_for_group g, g)
      s.groups 
  in
  let gs_by_max_cov= 
    List.sort
      (fun (cov1, _) (cov2, _) -> 
	 (* we want descending, not ascending *)
	 compare cov2 cov1)
      gs_with_max_cov
  in

  (* the criterion for being a list: width > 3 and coverage/size >
     coverage_tolerance *)
  let list_histogram (_, h) = 
    let width = Hist.width h in
    let cov = float_of_int (Hist.coverage h) in
    let size = float_of_int (Hist.size h) in
      width > 3 && (cov /. size) > coverage_tolerance
  in
    (* note the first empty argument, to throw away the max_cov from
       sorting *)
  let list_group (_, g) = 
    List.exists list_histogram g
  in
    try 
      (* try to find a candidate group *)
      let _, g = List.find list_group gs_by_max_cov in
	
      (* we've got one, so now we need to pull out its tokens and
	 split the list into a pre-amble, subsequences, and post-amble
      *)
      let tokens = List.map (fun (t, _) -> t) g in

      let split_append3 ls =
	List.fold_right
	  (fun (v1, v2, v3) (ls1, ls2, ls3) -> 
	     (v1 @ ls1, v2 @ ls2, v3 @ ls3))
	  ls
	  ([], [], [])
      in
	(* we map each chunk to a triple:

	   ([pre], subs, [post])

	   where pre is its preamble, post is its postamble, and subs
	   are the subsequence chunks *)
      let split_chunks = List.map (split_chunk_with_tokens tokens) cs in
	(* we merge the triples into a triple of lists:

	   (preambles, subs, postambles) *)
      let pres, subs, posts = split_append3 split_chunks in

	(* and now we prophesy a list *)
	Some (ListProphecy (pres, subs, posts))
    with Not_found -> None

(** this corresponds to part 5 of Fisher et al's oracle

    as a last ditch measure, if we can't predict something based on
    the structure of the data, we'll predict a union, using the first
    token to partition the input set
*)
let split_on_first_token (cs : chunks) _ =
  let cs, empties = List.partition (function | [] -> false | _::_ -> true) cs in

  (* a tag is the first token of a chunk *)
  let tags = List.map (fun c -> (List.hd c, c)) cs in

  (* now we group the tags *)
  let tagged_cs =
    group_tagged_list
      (fun (t, _) (t', _) -> Lex.token_equal t t')
      tags
  in

  (* once grouped, we can drop the tag, leaving us with a chunks list,
     for which we prophecy a union *)
  let cs_by_tag = List.map snd tagged_cs in
  let variants =
    if empties = []
    then cs_by_tag
    else cs_by_tag @ [empties]
  in
    Some (UnionProphecy variants)

let prophesy_passes : prophesy_pass list = 
  [check_for_identical_chunks; (* part 1 *)
   compute_histograms; (* part 2 *)
   group_histograms;
(* show_groups; *)
   find_structs; (* part 3 *)
   find_lists; (* part 4 *)
   split_on_first_token (* part 5 *)
  ]

let prophesy (cs : chunks) : prophecy =
  let state = empty_state () in
  let rec run passes =
    match passes with
	[] -> failwith "nothing prophesied!"
      | p::passes ->
	  match p cs state with
	      Some prophecy -> prophecy
	    | None -> run passes
  in
    run prophesy_passes
