(* Feature Tree Types
 * types.ml - manipulate feature tree types
 * Still in a rather fragile state. 
 * Complaints/Bugs to Nate <jnfoster@cis.upenn.edu>
 *)

open Syntax
   
let bogusI = Error.bogusInfo 

(* JNF's debugging flags for this module. displays LOTS of information *)
let debug_flag = false
let debug_view v = if debug_flag then V.format v 
let debug s = if debug_flag then (Format.printf "%s\n" s; Format.print_flush ())
  
(* helper: map f everywhere on a type - not currently used *)
let rec map_typ f t = 
  match t with
      TCat(cs,i)   -> f (TCat ((List.map (map_typ f) cs),i))
    | TUnion(us,i) -> f (TCat ((List.map (map_typ f) us),i))
    | _         -> f t

(* a (somewhat arbitrary) order on types: used in t2nf *)
let rec cmp_typ t1 t2 = 
  let eq = 0 in    (* symbolic comparisons *)
  let lt = -1 in
  let gt = 1 in

  (* helper for comparing lists of things in dictionary order *)
  let rec cmp_lex l1 l2 cmp_f = 
    let rec cmp_aux l1 l2 tie = 
      match (l1,l2) with
	  ([],[])           -> tie
	| (_,[])            -> gt
	| ([],_)            -> lt
	| (h1::t1),(h2::t2) ->
	    if (tie = eq) then cmp_aux t1 t2 (cmp_f h1 h2)
	    else cmp_aux t1 t2 tie
    in
      cmp_aux l1 l2 eq
  in
    match t1,t2 with
      (* TName *)
      | TEmpty(_),_                 -> lt
      | _,TEmpty(_)                 -> gt
      | TEmptyView(_),_             -> lt
      | _,TEmptyView(_)             -> gt
      | TName(n1,_,_),TName(n2,_,_) -> compare n1 n2
      | TName(_,_,_),_              -> lt

      (* TAny *)
      | TAny(f1,_,_),TName(_,_,_)   -> gt
      | TAny(f1,_,_),TAny(f2,_,_)   -> cmp_lex f2 f1 compare
      | TAny(_,_,_),_               -> lt

      (* TAll *)
      | TAll(_,_,_),TName(_,_,_)    -> gt
      | TAll(_,_,_),TAny(_,_,_)     -> gt
      | TAll(f1,_,_),TAll(f2,_,_)   -> cmp_lex f2 f1 compare
      | TAll(_,_,_),_               -> lt
	
      (* TCat *)
      | TCat(_,_),TName(_,_,_)      -> gt
      | TCat(_,_),TAny(_,_,_)       -> gt 
      | TCat(_,_),TAll(_,_,_)       -> gt
      | TCat(cs1,_),TCat(cs2,_)     -> cmp_lex cs1 cs2 cmp_typ 
      | TCat(_,_),_                 -> lt
	  
      (* TUnion *)
      | TUnion(_,_),TName(_,_,_)    -> gt
      | TUnion(_,_),TAny(_,_,_)     -> gt
      | TUnion(_,_),TAll(_,_,_)     -> gt
      | TUnion(_,_),TCat(_,_)       -> gt
      | TUnion(us1,_),TUnion(us2,_) -> cmp_lex us1 us2 cmp_typ 

(* sort a type list *)
let sort_tl tl = List.sort cmp_typ tl 
	  
(* sort a type *)
let rec sort_typ t = match t with
  | TCat(ts,i)   -> TCat (sort_tl (List.map sort_typ ts),i)
  | TUnion(us,i) -> TUnion (sort_tl (List.map sort_typ us),i)
  | _         -> t

(* t2nf: convert a type to normal form *)
(* returns the new type and a list of new bindings *)
let rec t2nf t delta = match t with 
  | TEmpty(_)     -> (t,[])
  | TEmptyView(_) -> (t,[])
  | TAny(_,_,_)   -> (t,[])
  | TAll(_,_,_)   -> (t,[]) (* should maybe think about normalizing empty TAlls as TEmptyViews *)
  | TName(_,_,_)  -> (t,[])

  (* unions:
     - recursively normalize
     - lift nested unions
     - remove TEmpty from unions
  *)
  | TUnion(us,i)  ->       
      let (usnf,newbinds) = 
	List.fold_left 
	  (fun (usnf,newbinds) ui -> 
	     let (uinf,uibinds) = t2nf ui (delta @ newbinds )in
	       (uinf::usnf, uibinds @ newbinds))
	  ([],[])
	  us
      in
      let lift_us = 
	(List.fold_left 
	   (fun acc h ->
	      match h with 
		| TEmpty(_)    -> acc    (* skip TEmpty bits of unions *)
		| TUnion(us,i) -> us@acc (* lift nested unions *)
		| _            -> h::acc)
	   []
	   usnf) 
      in 
      let tres = 
	(match lift_us with	  
	   | []              -> TEmpty(bogusI)
	   | [(TCat ([],_))] -> TEmpty(bogusI)
	   | _               -> sort_typ (TUnion(lift_us,i)))
      in 
	(tres, newbinds)
	  
  (* TCat:
     - recursively normalize cs
     - lift nested cats in cs 
     - distribute nested unions over cats
     - sort result
     - if a name is repeated, then TEmpty
  *)
  | TCat(cs,i)    -> 
      let (csnf,newbinds) = 
	List.fold_left 
	  (fun (csnf,newbinds) ci -> 
	     let (cinf,cibinds) = t2nf ci (delta @ newbinds) in
	       (cinf::csnf, (cibinds @ newbinds)))
	  ([],[])
	  cs
      in
	
      (* temporarily represent the lifted union of cats as a list of lists *)
      let lift_us_rep = 
	List.fold_left
	  (fun acc h ->
	     (* helper functions for readability *)
	     let prepend s a = List.map (fun x -> sort_tl (s::x)) a in
	     let concat cs a = List.map (fun x -> sort_tl (x @ cs)) a in
	       match h with
		 | TCat(cs,_)   -> concat cs acc (* lift nested cats *)
		 | TUnion(us,_) -> 
		     (* distribute unions, lift deep-nested cats *)
		     List.flatten 
		       (List.map 
			  (fun u -> match u with
			     | TCat(cs,_) -> concat cs acc
			     | _          -> prepend u acc) us)
		 | _         -> prepend h acc)
	  [[]]
	  csnf
      in
      let check_repeats t = 
	let rec cr_aux cs info acc = 
	  match cs with
	    | []                                            -> 
		TCat ((List.rev acc),info)
	    | TName(n,_,_)::TName(m,_,_)::rest when (n = m) -> 
		TEmpty(info)
	    | TAll(f,x,i)::TAll(_,y,_)::rest when (x=y)     ->
		cr_aux ((TAll(f,x,i))::rest) info acc
	    | TAll(_,_,_)::TAll(_,_,i)::rest                ->
		raise (Error.Type_error("repeated wildcard ~ in type", i))
	    | h::t                                          -> 
		cr_aux t info (h::acc) 
	in
	  match t with
	      TCat(cs,i) -> cr_aux cs i []
	    | t          -> t
      in
      let tres = 
	match lift_us_rep with	    
	  | [[]]              -> TEmpty(i) (* recognize some empty types *)
	  | [[TEmpty(_)]]     -> TEmpty(i)
	  | [[(TCat ([],_))]] -> TEmpty(i)
	  | [[t]]             -> check_repeats t
	  | [cs]              -> check_repeats (TCat (cs,i))
	  | _                 -> 
	      TUnion (List.map (fun x -> check_repeats (TCat (x,i))) lift_us_rep, i)
      in
	(tres,newbinds)
	  
(************************)
(* TRANSLATION OF TYPES *)
(************************)
	
(* transpt : takes arguments
 *         - ptenv : an environment mapping strings to pretypes
 *         - tenv  : an environment mapping strings to types
 *         - xs    : a list of variables being checked/defined
 *         - pt    : a pretype
 * 
 * returns a pair (tenv',t) consisting of an updated tenv, and a
 * computed type for pt
 *)

(* fresh variable counter *)
let freshid = ref 0

let rec transpt ptenv tenv xs pt = 

  (* helper: conver a state name to a complex state *)
  let state2cstate x = (x,[],[]) in
    
  (* helper: generate fresh variable name *)
  let freshv () = 
    let res = "_X_" ^ (string_of_int !freshid) in
    let _ = freshid := !freshid + 1 in
      res
  in

  (* helper: find a type state equal to TEmpty, or add one if needed *)
  let getempty delta = 
    let rec ges_aux d acc = 
      match d with
	  []           -> 
	    let x = freshv () in
	      (state2cstate x, 
	       List.rev ((x,bogusI,TEmpty(bogusI))::acc))
	| (x,i1,TEmpty i2)::rest -> 
	    (state2cstate x, 
	     (List.rev acc) @ ((x,i1,TEmpty i2)::rest))
	| d::rest      ->
	    ges_aux rest (d::acc)
    in
      ges_aux delta []
  in

  (* helper: convert a pre-type to a cstate, making up a new name if needed *)
  let pt2cstate tenv pt1 = 
    match pt1 with
      |	PTVar(y,i) -> 
	  (* check that y is defined somewhere, and use it *)
	  (match lookup_opt y ptenv with
	     | None -> raise (Error.Type_error ("Type variable " ^ y ^ " is not defined",i))
	     | Some _ -> (tenv,(y,[],[])))
      | _ -> 
	  (* get a fresh name, compile type new definition *)
	  let x = freshv () in
	  let (tenv1,t1) = transpt ptenv tenv (x::xs) pt1 in
	    (add x bogusI t1 tenv1,(x,[],[]))
  in

    (* transpt - main body *)
    match pt with
      | PTEmpty(i)          -> (tenv, TEmpty(i))
      | PTEmptyView(i)      -> (tenv, TEmptyView(i))
      | PTVar(y,i)          -> 
	  if (List.mem y xs) then 
	    raise (Error.Type_error ("Non-contractive use of " ^ y, i))
	  else 
	    (match (lookup_opt y tenv) with
	       | None -> transpt ptenv tenv (y::xs) 
		   (match (Syntax.lookup_opt y ptenv) with
			None -> raise (Error.Type_error ("Type variable " ^ y ^ " is not defined", i))
		      | Some ty -> ty)
	       | Some t -> (tenv,t))
      | PTName (m,n,pt1,i)  -> 
	  let (tenv1,cx) = pt2cstate tenv pt1 in
	  let t1 = TName(n,cx,i) in	    
	  let (tenv2,t2) = 
	    if m then 
	      let (cempty,tenv2) = getempty tenv1 in
		(tenv2,TUnion ([TAll([],cempty,i);t1],i))
	    else 
	      (tenv1,t1) 
	  in
	    (tenv2, t2)	      
      | PTAny (m,f,pt1,i)   -> 
	  let (tenv1,cx) = pt2cstate tenv pt1 in
	  let t1 = TAny(f,cx,i) in
	  let (tenv2,t2) = 
	    if m then 
	      let (cempty,tenv2) = getempty tenv1 in
		(tenv2,TUnion ([TAll([],cempty,i);t1],i))
	    else 
	      (tenv1,t1) 
	  in
	    (tenv2, t2)	      
      | PTAll (m,f,pt1,i)   -> 
	  let (tenv1,cx) = pt2cstate tenv pt1 in
	  let t1 = TAll(f,cx,i) in
	  let (tenv2,t2) = 
	    if m then 
	      let (cempty,tenv2) = getempty tenv1 in
		(tenv2,TUnion ([TAll([],cempty,i);t1],i))
	    else 
	      (tenv1,t1) 
	  in
	    (tenv2, t2)
      | PTCat (pt1,pt2,i)    -> 
	  let (tenv1,t1) = transpt ptenv tenv xs pt1 in
	  let (tenv2,t2) = transpt ptenv tenv1 xs pt2 in
	    (tenv2, TCat ([t1;t2],i))
      | PTUnion (pt1,pt2,i)  -> 
	  let (tenv1,t1) = transpt ptenv tenv xs pt1 in
	  let (tenv2,t2) = transpt ptenv tenv1 xs pt2 in
	    (tenv2, TUnion ([t1;t2],i))
      | PTDiff (pt1,pt2,_)   -> 
	  let (tenv1,t1) = transpt ptenv tenv xs pt1 in
	  let (tenv2,t2) = transpt ptenv tenv1 xs pt2 in
	  (* let _ = (tenv2,diff t1 t2) in *)
	    debug "Diffs unimplemented";
	    assert false (* diff is not finished yet *)

      | PTInter (pt1,pt2,_)  -> 
	  let (tenv1,t1) = transpt ptenv tenv xs pt1 in
	  let (tenv2,t2) = transpt ptenv tenv1 xs pt2 in
	  (* let t2m1 = diff t2 t1 in
	     let _ = (tenv2,diff t1 t2m1) in
	  *)
	    debug "Intersections unimplemented";
	    assert false (* inter depends on diff, which is unimplemented *)
	      
let pctx2ctx ptenv = 
  let f tenv (x,i,pt) =
    let (tenv1,t) = transpt ptenv tenv [x] pt in
      add x i t tenv1
  in
    (List.rev (List.fold_left f [] ptenv))

let normalize_ctx delta =   
  let delta1,newbinds = 
    (List.fold_left 
       (fun (deltaacc,newbinds) (x,i,t) ->
	  let (t1,tbinds) = t2nf t (delta @ newbinds) in
	    ((x,i,t1)::deltaacc, (tbinds @ newbinds)))
       ([],[])
       delta)
  in
    (List.rev delta1) @ (List.rev newbinds)
    
(* more functions *)
let rec project t n delta = 
  debug ("--- PROJECT --- " ^ n ^ " from t=" ^ (Tutil.t2str t));
  let res = 
    match t with
      | TEmpty(_)     -> None
      | TEmptyView(_) -> None
      | TName(m,x,_)  -> if (n=m) then Some x else None
      | TAny(f,x,_)   -> if (List.mem n f) then None else Some x
      | TAll(f,x,_)   -> 
	  if (List.mem n f) then None 
	  else 
	    begin
	      let tx = lookup (Syntax.cs2s x) delta in
		match tx with
		  | TEmpty _ -> None (* check if t == {} *)
		  | _        -> Some x
	    end
      | TUnion(ts,_) -> 	  
	  List.fold_left 
	    (fun xo ti -> match xo with
		 None -> project ti n delta
	       | Some x -> 
		   let to2str t = 
		     match t with
			 None -> "NONE"
		       | Some t -> Tutil.cs2str t
		   in
		   let tin = project ti n delta in
		     (match tin with
			  None -> xo
			| Some _ -> 
			    if tin = xo then xo
			    else
			      (Format.printf 
				 "ERROR: type %s is not projectable on %s because %s <> %s\n%s\n"
				 (Tutil.t2str t)
				 n
				 (to2str tin)
				 (to2str xo)			      
				 (Tutil.defs2str delta);
			       assert false)))
	    None 
	    ts
      | TCat(cs,_) -> 
	  List.fold_left 
	    (fun xo ti -> match xo with
		 None -> project ti n delta
	       | Some _ -> xo)
	    None 
	    cs
  in
  let _ = debug ("res = " ^ (match res with
				 None -> "NONE"
			       | Some x -> Tutil.cs2str x)) in
    res

(* the unfold operator *)
let rec unfold cx delta = match cx with
    (x,[],[]) -> lookup x delta
  | _         -> debug "ERROR: unfold not implemented for complex type states"; assert false
(*
  | (x,y::t,ds) -> 
      let dy = lookup y delta in
      let ux = unfold (x,t,ds) delta in
	diff (TUnion ([ux; dy],bogusI)) (TUnion ([diff ux dy; diff dy ux],bogusI))
  | (x,[],y::t) -> diff (unfold (x,[],t) delta) (lookup y delta)
*) 
     
(* check if v in t *)
let rec member v t delta = 
  let _ = debug ("\n--MEMBER--\nt="
		 ^ (Tutil.t2str t)
		 ^ "\nv="); debug_view v in
  let _ = debug ("\nDELTA =\n" ^ (Tutil.defs2str delta)) in
    match t with		
      | TEmpty(_)    -> 
	  debug "\nin TEmpty\nANSWER to TAny = false";
	  false
      | TEmptyView(_) -> 
	  let _ = debug ("\nin TEmptyView") in
	  let res = V.is_empty v in
	  let _ = debug ("\nANSWER to TEmptyview: " ^ (string_of_bool res)) in
	    res
      | TName(n,x,_) ->
	  debug ("\nin TName for " ^ n);
	  let d = V.dom v in
	  let c = Name.Set.cardinal d in
	  let res = (c = 1) && 
		    (match V.get v n with
		       | None -> false
		       | Some vn -> member vn (unfold x delta) delta)
	  in
	  let _ = debug ("ANSWER to TName: " ^ (Tutil.t2str t) ^ " = " ^ (string_of_bool res)) in
	    res
      | TAny(f,x,_) ->
	  let _ = debug "\nin TAny" in	  
	  let d = V.dom v in
	  let res = 
	    match Name.Set.cardinal d with
	      | 1 -> 
		  let k = Name.Set.choose d in		   
		    (not (List.mem k f)) 
		    && (member 
			  (V.get_required v k) 
			  (unfold x delta) 
			  delta)
	      | _ -> false
	  in
	  let _ = debug ("ANSWER to TAny: " ^ (Tutil.t2str t) ^ " = " ^ (string_of_bool res)) in
	    res
      | TAll(f,x,_) -> 
	  debug "\nin TAll";
	  let ux = unfold x delta in
	  let res = Name.Set.fold 
	      (fun k okSoFar ->
		 okSoFar &&
		 (not (List.mem k f)) &&
		 (member (V.get_required v k) ux delta))	  
	      (V.dom v)
	      true	  
	  in 
	  let _ = debug ("ANSWER to TAll : " ^ (Tutil.t2str t) ^ " = " ^ (string_of_bool res)) in
	    res
      | TUnion (us,_) -> 
	  let _ = debug "\nin TUnion" in
	  let res = 
	    List.fold_left
	      (fun acc ui ->
		 if acc then true 
		 else member v ui delta)
	      false
	      us
	  in
	  let _ = debug ("ANSWER to TUnion: "  ^ (Tutil.t2str t) ^ " = " ^ (string_of_bool res)) in
	    res
      | TCat (cs,_) -> 
	  let _ = debug "\nin TCat" in
	  (* this split does not work in general... 
	     just for testing some easy cases *)
	  let split v t  = 
	    match t with
	      | TEmpty(_)     -> (None, v)
	      | TEmptyView(_) -> (None, v) 
	      | TName(n,x,_) -> 
		  (match V.get v n with
		     | None       -> (None, v)
		     | Some vn    -> (Some (vn,x), V.set v n None))
	      | TAny(f,x,_)  -> 
		  let n = Name.Set.choose (V.dom v) in
		  let vn = V.get_required v n in
		    if (List.mem n f) then (None, v)
		    else (Some (vn,x), V.set v n None)
	      | TAll(f,x,_)  -> (Some (v,x), V.empty)
	      | _            -> 
		  debug ("ERROR: the impossible happened. Non-atomic type in TCat; t=" ^ (Tutil.t2str t));
		  assert false (* can't happen *)
	  in
	  let rec loop cs v = 
	    (** let _ = debug ("\n--LOOP--\n t="
		^ (Tutil.tl2str cs)
	      ^ " v=") in
	      let _ = V.format v in	      
	    **)
	    if (V.is_empty v) then 
	      (* all the cs must be TAlls *)
	      List.fold_left (fun ok h -> match h with TAll _ -> ok | _ -> false) true cs
	    else
	      match cs with
		| []   -> false
		| [ci] -> member v ci delta
		| ci::rest -> 
		    match split v ci with
		      | (None,_)             -> false
		      | (Some (vk,x), vrest) ->
			  let tx = unfold x delta in
			    (member vk tx delta ) && (loop rest vrest)
	  in
	  let res = loop cs v
	  in
	  let _ = debug ("ANSWER to TCat: "  ^ (Tutil.t2str t) ^ " = " ^ (string_of_bool res)) in
	    res

(* Nice interface to the parser/compiler for types *)
let string2abstract_type deltastr = 
  try 
    let lexbuf = Lexing.from_string deltastr in
    let pdefs = Parser.pdefs Lexer.type_token lexbuf in
    let delta1 = pctx2ctx pdefs in
    let delta = normalize_ctx delta1 in
    let ty = 
      match pdefs with 
	  [] -> Syntax.TEmpty Error.bogusInfo
	| ((x,_,_)::_) -> unfold (x,[],[]) delta 
    in
      (delta,ty)	
  with
      e -> 
	Format.printf "ERROR in string2abstract_type for:\n%s\n" deltastr; 
	raise e


(* --- CK added Type Domain stuff from here --- *)

type tdom_atom =
    DAny of Name.Set.t
  | DAll of Name.Set.t
  | DName of name
      
let cmp_tdom_atom a1 a2 =
  let lt = -1 in
  let gt = 1 in
    match a1,a2 with
 	(* DAny *)
	DAny(s1), DAny(s2)   -> Name.Set.compare s1 s2
      | DAny(_), DAll(_)     -> lt
      | DAny(_), DName(_)    -> lt
          (* DAll *)
      | DAll(_), DAny(_)     -> gt
      | DAll(s1), DAll(s2)   -> Name.Set.compare s1 s2 
      | DAll(_), DName(_)    -> lt
          (* DName *)
      | DName(_), DAny(_)    -> gt
      | DName(_), DAll(_)    -> gt
      | DName(n1), DName(n2) -> compare n1 n2
	  
module TDom = Set.Make(struct
	 		 type t = tdom_atom
		 	 let compare = cmp_tdom_atom
		       end)

module TDoms = Set.Make(struct
 			  type t = TDom.t
	 		  let compare = compare
			end)  
  
let atom2str ta = 
  match ta with
      DAny s -> "! "
    | DAll s -> "* "
    | DName n -> n ^ " "

let tdom2str td = 
  "{ " ^ (TDom.fold (fun ta -> fun str -> str ^ (atom2str ta)) td "") ^ "} "

let tdoms2str tds = 
  "{ " ^ (TDoms.fold (fun td -> fun str -> str ^ (tdom2str td)) tds "") ^ "}"

let nfcheck tbase f t = 
  match tbase with
      TCat(_,i) ->
	(match t with
	     TCat(_,_) | TUnion(_,_) -> raise (Error.Type_error("Type not normal formed",i)) 
	   | _ -> f t)
    | TUnion(_,i) -> 
	(match t with 
	     TUnion(_,_) -> raise (Error.Type_error("Type not normal formed",i)) 
	   | _ -> f t)
    | _ -> f t
		    
let rec tdoms t =
  let nameset lst = 
    List.fold_left (fun ns n -> Name.Set.add n ns) Name.Set.empty lst
  in 
  let shallow_union f lst = 
    List.fold_left (fun acc td -> TDoms.union acc (f td)) TDoms.empty lst     
  in
  let deep_union f lst = 
    TDoms.singleton (TDoms.fold (fun acc d -> TDom.union acc d) (shallow_union f lst) TDom.empty)
  in
    match t with
	TUnion(tl,_)  -> shallow_union (nfcheck t tdoms) tl
      | TCat(tl,_)    -> deep_union (nfcheck t tdoms) tl
      | TName(m,x,_)  -> TDoms.singleton (TDom.singleton (DName(m))) 
      | TAny(f,x,_)   -> TDoms.singleton (TDom.singleton (DAny(nameset f))) 
      | TAll(f,x,_)   -> TDoms.singleton (TDom.singleton (DAll(nameset f))) 
      | TEmptyView(_) -> TDoms.singleton (TDom.empty)
      | TEmpty(_)     -> TDoms.empty

let rec vdom_in_tdoms vd tds =
  let name_match n ta =
    match ta with
	DName m -> m=n
      | _ -> false
  in
  let any_match n ta = 
    match ta with
	DAny s -> not (Name.Set.mem n s) 
      | _ -> false
  in
  let all_match n ta = 
    match ta with
	DAll s -> not (Name.Set.mem n s) 
      | _ -> false
  in    
  let tdom_filter keep f n (vd,td) = 
    let (td1,td2) = TDom.partition (f n) td in
      if (TDom.cardinal td1)=1 
      then (Name.Set.remove n vd, 
	    if keep then td else td2) 
      else (vd,td)
  in    
  let is_all ta =
    match ta with
	DAll _ -> true
      | _ -> false
  in 
  let rec vdom_matches_tdom vd td =
    let (vd1,td1) = (Name.Set.fold (tdom_filter false name_match) vd (vd,td)) in
    let (vd2,td2) = (Name.Set.fold (tdom_filter false any_match) vd (vd1,td1)) in
    let (vd3,td3) = (Name.Set.fold (tdom_filter true all_match) vd (vd2,td2)) in
      (Name.Set.is_empty vd3) && ((TDom.is_empty td3) or (TDom.for_all is_all td3))
  in
    TDoms.exists (vdom_matches_tdom vd) tds 

(* --- CK added to here --- *)



(************* DEPRECATED / UNFINISHED STUFF **************************)
(* let setUnion x f =  if (List.mem x f) then f else x::f *)
(* *)
(* (\* symbolic diff on two cstates *\) *)
(* let rec csdiff cx cy = *)
(*   let (x,ix,dx) = cx in *)
(*     match cy with *)
(*       | (z,[],[])    ->  *)
(* 	  if (z = x || List.mem z ix) then [] *)
(* 	  else [(x,ix,setUnion z dx)] *)
(*       | (z,i::is,ds) ->  *)
(* 	  let rest = csdiff cx (z,is,ds) in *)
(* 	    if (z = x || List.mem z ix) then rest *)
(* 	    else (x,ix,setUnion z dx)::rest *)
(*       | (z,[],d::ds) ->  *)
(* 	  let rest = csdiff cx (z,[],ds) in *)
(* 	    if (List.mem d dx) then rest  *)
(* 	    else if (x = d) then cx::rest  *)
(* 	    else (x,setUnion d ix,dx)::rest *)
(* *)
(* (\* the diff operator *\) *)
(* let rec diff t1 t2 =  *)
(*   (\** let _ = debug ("diffing: " ^ Tutil.t2str t1 ^ ", " ^ Tutil.t2str t2) in **\) *)
  
(*   (\* compute normal forms *\) *)
(*   let nt1,nt2 = t2nf t1, t2nf t2 in     *)
    
(*   (\* lift a list of cstates to a list of things generated by f *\) *)
(*   let liftCSUnions cus f =  *)
(*     match cus with *)
(* 	[]  -> [TEmpty(bogusI)] *)
(*       | [z] -> [f z] *)
(*       | _   -> (List.map f cus) *)
(*   in *)
(*   let mkNs f1 f2 x info = *)
(*     List.map  *)
(*       (fun n -> TName(n,x,info)) *)
(*       (List.filter (fun n -> not (List.mem n f1)) f2) *)
(*   in *)

(*   (\* helper: used whenever we have to split a TCat *\) *)
(*   (\* NB: cs1 and cs2 are assumed to be in nf--i.e., consist of TNames, *)
(*      TAny, and at most one TAll in that order! *\) *)
(*   let split_case cs1 cs2 info =  *)
(*     let divide cs =  *)
(*       List.fold_left *)
(* 	(fun (names,anys,alls) h -> *)
(* 	   match h with *)
(* 	       TName(_,_,_) -> (h::names,anys,alls) *)
(* 	     | TAny(_,_,_)  -> (names,h::anys,alls) *)
(* 	     | TAll(_,_,_)  -> (names,anys,h::alls) *)
(* 	     | _          ->  *)
(* 		 (\* can't happen *\) *)
(* 		 raise (Error.Type_error("Type not in normal form: " ^ (Tutil.t2str h), *)
(* 					t2info h)) *)
(* 	) ([],[],[]) cs *)
(*     in *)
(*     let (ns1,ys1,ls1),(ns2,ys2,ls2) = divide cs1, divide cs2 in *)
      
(*     (\* the split operator - iterates all the perfect matchings *\) *)
(*     let rec split cs1 cs2 cover acc =  *)
(*       (\**  *)
(* 	let _ = debug ("--SPLIT--\n cs1=" *)
(* 	^ (Tutil.tl2str cs1) *)
(* 	^ " cs2=" *)
(* 	^ (Tutil.tl2str cs2) *)
(* 	^ " cover=" *)
(* 	^ (if cover = [] then "[]"  *)
(* 	else (List.fold_right  *)
(* 	(fun (n,x) ta -> "(" ^ n ^ ", " ^ (Tutil.cs2str x) ^ ") " ^ ta) *)
(* 	cover "")) *)
(* 	^ " acc=" *)
(* 	^ Tutil.tpl2str acc) in  *)
(*       **\) *)
(*       match cs1,cs2 with *)
(*       | (TName(n,x,i1)::tl1, TName(m,y,i2)::tl2) ->  *)
(* 	  if m = n then split tl1 tl2 cover ((TName(n,x,i1),TName(m,y,i2))::acc) *)
(* 	  else split tl1 tl2 ((n,x)::cover) acc *)
(*       | (TName(n,x,_)::tl1, TAny(f,y,_)::tl2) -> split tl1 cs2 ((n,x)::cover) acc *)
(*       | (TName(n,x,_)::tl1, TAll(f,y,_)::tl2) -> split tl1 cs2 ((n,x)::cover) acc *)
(*       | (TName(n,x,i)::tl1, [])             -> split tl1 [] ((n,x)::cover) acc *)
(*       | [],[]                             -> *)
(* 	  if (cover = []) then acc  *)
(* 	  else  *)
(* 	    (List.map (fun (n,x) -> (TName(n,x,bogusI),TEmpty(bogusI))) cover)  *)
(* 	    @ (List.map (fun (t,_) -> (t,TEmpty(bogusI))) acc) *)
(*       | _ ->  *)
(* 	  raise (Error.Type_error("unimplemented case in split", bogusI)) *)
(*    in  *)
      
(*     let splits = split cs1 cs2 [] [] in       *)
(*     let diff_pairs = List.map (fun (t1,t2) -> (t1,diff t1 t2)) splits in *)

(*     (\** *)
(*       let _ = debug ("splits=" ^ (Tutil.tpl2str splits)) in       *)
(*       let _ = debug ("diff_pairs=" ^ (Tutil.tpl2str diff_pairs)) in *)
(*     **\) *)
(*     (\* ugly hack alert! - we should fold this fold in somewhere else :) *\)  *)
(*     let all_diffs_empty =  *)
(*       List.fold_left (fun a (_,d) ->  *)
(* 		     match d with  *)
(* 		       | TEmpty _ -> a *)
(* 		       | _        -> false) *)
(* 	true splits *)
(*     in *)

(*     (\* helper: append t to all the lists in acc IF acc non-empty *\) *)
(*     let rec app t acc =  *)
(*       match acc with *)
(* 	| []      -> [] *)
(* 	| h::rest -> (h@[t])::(app t rest) *)
(*     in       *)
      
(*     (\* hideous code. JNF takes blame :) *\) *)
(*     let diag_diff_matrix =  *)
(*       snd  *)
(* 	(List.fold_left  *)
(* 	   (fun (ts,acc) (t,d) -> *)
(* 	      let newacc = app t acc in *)
(* 		(ts@[t]), *)
(* 		match d with  *)
(* 		    TEmpty _ -> newacc  *)
(* 		  | _ -> (ts@[d])::newacc) *)
(* 	   ([],[]) *)
(* 	   diff_pairs) *)
(*     in *)
      
(*       (\* need to factor out code from t2nf and here *\) *)
(*       if (all_diffs_empty) then nt1 *)
(*       else 	 *)
(* 	let cs =  *)
(* 	  List.map (fun x -> TCat(x,info)) diag_diff_matrix in *)
(* 	  t2nf (TUnion(cs,info)) *)
(*   in *)
(*     match nt1,nt2 with *)

(*       (\* the "easy" cases *\) *)
(*       | TEmpty(_),_               -> nt1 *)
(*       | _,TEmpty(_)               -> nt1 *)
(*       | TUnion(us,i),_         -> t2nf (TUnion ((List.map (fun ui -> diff ui t2) us),i)) *)
(*       | _,TUnion(us,_)         -> t2nf (List.fold_left (fun t ui -> diff t ui) t1 us) *)
(*       | TCat(_,_),TName(_,_,_) -> nt1 *)
(*       | TCat(_,_),TAny(_,_,_)  -> nt1 *)

(*       | TCat(cs1,i),TAll(f,y,_) -> split_case cs1 [nt2] i *)
(*       | TCat(cs1,i),TCat(cs2,_) -> split_case cs1 cs2 i *)
	    
(*       (\* base cases *\) *)
(*       | TName(n,x,i),TName(m,y,_) ->  *)
(* 	  if (n <> m) then nt1 *)
(* 	  else t2nf (TUnion ((liftCSUnions (csdiff x y) (fun z -> TName(n,z,i))),i)) *)

(*       | TName(n,x,i),TAny(f,y,_)  -> *)
(* 	  if (List.mem n f) then nt1 *)
(* 	  else t2nf(TUnion ((liftCSUnions (csdiff x y) (fun z -> TName(n,z,i))),i)) *)

(*       | TName(n,x,i),TAll(f,y,_)  -> *)
(* 	  if (List.mem n f) then nt1 *)
(* 	  else t2nf (TUnion ((liftCSUnions (csdiff x y) (fun z -> TName(n,z,i))),i)) *)

(*       | TName(n,x,_),TCat(ts,_) -> nt1 *)
	  
(*       | TAny(f,x,i),TName(m,y,_)  ->  *)
(* 	  if (List.mem m f) then nt1 *)
(* 	  else  *)
(* 	    t2nf(TUnion ((TAny(setUnion m f,x,i):: *)
(* 			    (liftCSUnions  *)
(* 			       (csdiff x y)  *)
(* 			       (fun z -> TName(m,z,i)))),i)) *)

(*       | TAny(f,x,i),TAny(g,y,_)   -> *)
(* 	  t2nf (TUnion (( *)
(* 		  (mkNs f g x i) @ *)
(* 			  (liftCSUnions  *)
(* 			     (csdiff x y)  *)
(* 			     (fun z -> TAny(f@g,z,i))) *)
(* 			),i)) *)
	    
(*       | TAny(f,x,i),TAll(g,y,_)   -> *)
(* 	  t2nf(TUnion (( *)
(* 		 (mkNs f g x i) @ *)
(* 		 (liftCSUnions  *)
(* 		    (csdiff x y)  *)
(* 		    (fun z -> TAny(f@g,z,i))) *)
(* 	    ),i)) *)

(*       | TAny(f,x,_),TCat(ts,_)     -> nt1 *)

(*       | TAll(f,x,i),TName(m,y,_)  -> *)
(* 	  if (List.mem m f) then nt1 *)
(* 	  else *)
(* 	    t2nf ( *)
(* 	      TUnion (( *)
(* 		(liftCSUnions (csdiff x y) (fun z -> (TName(m,z,i)))) *)
(* 		@[(\* emptyViewType; *\)TAny(setUnion m f,x,i); TCat ([TAny(f,x,i); TAny(f,x,i); TAll(f,x,i)],i)] *)
(* 	      ),i)) *)

(*       | TAll(f,x,i),TAny(g,y,_)   -> *)
(* 	  t2nf ( *)
(* 	    TUnion(( *)
(* 	      (liftCSUnions (csdiff x y) (fun z -> (TAny(f,z,i)))) *)
(* 	      @ (mkNs f g x i) *)
(* 	      @ [(\* emptyViewType; *\) TCat([TAny(f,x,i); TAll(f,x,i)],i)]	  	   *)
(* 	    ),i)) *)

(*       | TAll(f,x,i),TAll(g,y,_)   -> *)
(* 	  t2nf ( *)
(* 	    TCat((TAny(f,x,i)::[TUnion(( *)
(* 			       (liftCSUnions (csdiff x y) (fun z -> (TAny(f,z,i)))) *)
(* 			       @ (mkNs f g x i)),i)]),i) *)
(* 	  ) *)
(*       | TAll(f,x,i),TCat(cs2,_) -> split_case [nt1] cs2 i *)



(* insert CK's algorithm here *)
(* let is_empty g x = false *)





