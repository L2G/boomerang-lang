(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* type.ml - representation and functions on types       *)
(*                                                       *)
(*********************************************************)
(* $Id: type.ml,v 1.1 2005/04/11 18:24:48 jnfoster Exp $ *)

open Pretty
    
let sprintf = Printf.sprintf

(* TYPES *)
type t = TT of pt | NT of pt 
and pt = 
    Empty 
  | Var of Syntax.qid * Syntax.qid list (* FIXME: do we need the naming context here ? Seems so *) 
  | App of pt * pt 
  | Fun of (pt -> pt)
  | Name of string * pt
  | Star of (string list) * pt
  | Bang of (string list) * pt
  | Cat of pt list 
  | Union of pt list 

(* types *)
let rec string_of_t = function 
    TT pt -> string_of_pt pt
  | NT pt -> sprintf "~(%s)" (string_of_pt pt)

and string_of_pt = function
  | Empty -> "empty"
  | Var (x,_) -> Syntax.string_of_qid x
  | App(pt1,pt2) -> sprintf "(%s %s)" (string_of_pt pt1) (string_of_pt pt2)
  | Fun f -> "<type fun>"
  | Name(n,pt) -> sprintf "%s = %s" n (string_of_pt pt)
  | Bang(f,pt)  -> 
      sprintf "!%s = %s"
	(if f = [] then "" else "\\" ^ braces (concat ", " f))
	(string_of_pt pt)
  
  | Star(f,pt)  ->
     sprintf "!%s = %s"
	(if f = [] then "" else "\\" ^ braces (concat ", " f))
	(string_of_pt pt)
  | Cat(cs)   -> concat "." (Safelist.map string_of_pt cs)
  | Union(ts)  -> braces (concat " | " (Safelist.map string_of_pt ts))


(* STUBS *)
let eval_pt = fun x -> x
      
(* a (somewhat arbitrary) order on types: used in t2nf *)
let eq = 0     (* symbolic comparisons *)
let lt = -1
let gt = 1
  
(* helper for comparing lists of things in dictionary order *)
let rec cmp_lex l1 l2 cmp_f =
  let rec cmp_lex_aux l1 l2 tie =
    match (l1,l2) with
	([],[])           -> tie
      | (_,[])            -> gt
      | ([],_)            -> lt
      | (h1::t1),(h2::t2) ->
	  if (tie = eq) then cmp_lex_aux t1 t2 (cmp_f h1 h2)
	  else cmp_lex_aux t1 t2 tie
  in
    cmp_lex_aux l1 l2 eq
  
let rec cmp_ptyp t1 t2 =
  match t1,t2 with      
      (* Fun *)
      Fun(_),Fun(_)         -> eq
    | Fun(_),_              -> lt
    | _,Fun(_)              -> gt
	
    (* | Var(x,xctx),Var(y,yctx) -> SHORT CIRCUIT SAME VARIABLE, SAME CONTEXT? *)
    | Var(x,_),_              -> cmp_ptyp (eval_pt t1) t2
    | _,Var(x,_)              -> cmp_ptyp t1 (eval_pt t2)
	
    (* App *)
    | App(pt1,pt2),_         -> cmp_ptyp (eval_pt pt1) t2
    | _,App(pt1,pt2)         -> cmp_ptyp t1 (eval_pt pt2)
	
    (* Empty *)
    | Empty,Empty           -> eq
    | Empty,_               -> lt
    | _,Empty               -> gt
	
    (* Name *)
    | Name(n1,_),Name(n2,_) -> compare n1 n2
    | Name(_,_),_           -> lt
    | _,Name(_,_)           -> gt
	
    (* Bang *)
    | Bang(f1,_),Bang(f2,_) -> cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare
    | Bang(_,_),_           -> lt
    | _,Bang(_,_)           -> gt
	
    (* Star *)
    | Star(f1,_),Star(f2,_) -> cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare
    | Star(_,_),_           -> lt
    | _,Star(_,_)           -> gt
	
    (* Cat *)
    | Cat(cs1),Cat(cs2)    -> cmp_lex (Safelist.sort cmp_ptyp cs1) (Safelist.sort cmp_ptyp cs2) cmp_ptyp
    | Cat(_),_             -> lt
    | _,Cat(_)             -> gt
	
    (* Union *)           
    | Union(us1),Union(us2) -> cmp_lex (Safelist.sort cmp_ptyp us1) (Safelist.sort cmp_ptyp us2) cmp_ptyp

let cmp_typ t1 t2 = match t1,t2 with
    NT(_),TT(_)      -> lt
  | TT(_), NT(_)     -> gt	
  | NT(pt1),NT(pt2)  -> cmp_ptyp pt1 pt2
  | TT(pt1), TT(pt2) -> cmp_ptyp pt1 pt2	  
      
(* sort a type list *)
let sort_ptyp pt = pt (* STUB *)
let sort_ptyp_list tl = Safelist.sort cmp_ptyp tl
	  
(*** UTILITIES ***)

(* pt2nf/t2nf: convert a (pre)type to normal form *)
let rec t2nf t0 = match t0 with
      NT pt -> NT (pt2nf pt)
    | TT pt -> TT (pt2nf pt)
and pt2nf pt0 = match pt0 with      
  | Empty      -> pt0
  | Fun(_)     -> pt0
  | Var(_,_)   -> pt2nf (eval_pt pt0)
  | App(_,_)   -> pt2nf (eval_pt pt0)
  | Name(n,pt1) -> Name(n, pt2nf pt1)
  | Bang(es,pt1)-> Bang(Safelist.sort compare es, pt2nf pt1)
  | Star(es,pt1)-> Star(Safelist.sort compare es, pt2nf pt1)
      
  (* unions:
     - recursively normalize
     - lift nested unions
     - remove obviously Empty types from unions
  *)
  | Union(us)  ->
      let us_nf = Safelist.map pt2nf us in
      let lifted_us = Safelist.fold_left
	(fun acc hpt ->
	   match hpt with
	       Empty     -> acc    (* skip TEmpty bits of unions *)
	     | Union(us) -> us@acc (* lift nested unions *)
	     | _         -> hpt::acc)
	[]
	us_nf
      in
	begin
	  match lifted_us with
	    | []           -> Empty
	    | [(Cat [])]   -> Empty
	    | [(Union [])] -> Empty
	    | _            -> sort_ptyp (Union(lifted_us))
	end
	  
  (* TCat:
     - recursively normalize cs
     - lift nested cats in cs
     - distribute nested unions over cats
     - sort result
     - if a name is repeated, then TEmpty
  *)
  | Cat(cs)    ->
      let cs_nf = Safelist.map pt2nf cs in	
	
      (* temporarily represent the lifted union of cats as a list of lists *)
      let lift_us_rep = Safelist.fold_left
	(fun acc h ->
	   (* helper functions for readability *)
	   let prepend s a = Safelist.map (fun x -> sort_ptyp_list (s::x)) a in
	   let concat cs a = Safelist.map (fun x -> sort_ptyp_list (x @ cs)) a in
	     match h with
	       | Cat(cs)   -> concat cs acc (* lift nested cats *)
	       | Union(us) ->
		   (* distribute unions, lift deep-nested cats *)
		   Safelist.flatten
		     (Safelist.map
			(fun u -> match u with
			   | Cat(cs) -> concat cs acc			     
			   | _          -> prepend u acc) us)
	       | _         -> prepend h acc)
	[[]]
	cs_nf
      in
      let check_repeats t =
	let rec cr_aux cs acc =
	  (* this used to be more complicated; keeping the structure for now *)
	  match cs with
	    | []                                       -> Cat (Safelist.rev acc)
	    | Name(n,_)::Name(m,_)::rest when (n = m)  -> Empty
	    | h::t                                     -> cr_aux t (h::acc)
	in
	  match t with
	      Cat(cs) -> cr_aux cs []
	    | t       -> t
		
      in		  
	begin
	  match lift_us_rep with
	    | [[]]          -> Empty (* recognize some obviously empty types *)
	    | [[Empty]]     -> Empty
	    | [[(Cat [])]]  -> Empty
	    | [[t]]         -> check_repeats t
	    | [cs]          -> check_repeats (Cat cs)
	    | _             -> Union (Safelist.map (fun x -> check_repeats (Cat x)) lift_us_rep)
	end
	    
let rec project t0 n = 
  match t0 with 
      TT pt -> 
	begin match project_pt pt n with 
	    None -> None
	  | Some ptn -> Some (TT (ptn))
	end
    | _ -> assert false (* what would this mean? *)
and project_pt pt0 n : pt option =
  let _ = debug (Format.sprintf "--- PROJECT_PT --- %s from t=%s\n" n (string_of_pt pt0)) in
    match pt0 with
      | Empty        -> None
      | Name(m,pt)   -> if (n=m) then Some pt else None
      | Bang(f,pt)   -> if (Safelist.mem n f) then None else Some pt
      | Star(f,pt)   -> if (Safelist.mem n f) then None else Some pt
       | Union(pts)  -> Safelist.fold_left 
 	   (fun xo pti -> 
	      match xo with 
 		  None -> project_pt pti n 
		| Some x -> begin match project_pt pti n with
		      None -> xo
		    | Some ptin ->
			if ptin = x then xo
			else (Format.printf
				"ERROR: type is not projectable because %s <> %s \n"
				(string_of_pt ptin)
				(string_of_pt x);
			      assert false)
		  end)
	     None
	     pts
       | Cat(pts) -> Safelist.fold_left
	   (fun xo pti -> match xo with
		None -> project_pt pti n
	      | Some _ -> assert (project_pt pti n = None); xo)
	     None
	     pts
       | Var(q,nctx) -> project_pt (eval_pt pt0) n
       | App(pt1, pt2) -> project_pt (eval_pt pt0) n
       | Fun(_) -> assert false
	   
let rec member v t = match t with 
    TT pt -> member_pt v pt
  | NT pt -> not (member_pt v pt)
and member_pt v pt0 = match pt0 with
  | Empty      -> false
  | Name(n,pt) ->
      let d = V.dom v in
        begin match Name.Set.cardinal d with
          | 1 ->
       	      let k = Name.Set.choose d in
       	      let vk = V.get_required v k in
       		(k = n) && (member_pt vk pt)
          | _ -> false
        end

  | Bang(f,pt) ->
      let d = V.dom v in
        begin match Name.Set.cardinal d with
          | 1 ->
       	      let k = Name.Set.choose d in
       	      let vk = V.get_required v k in
       		(not (List.mem k f)) && 
       		  (member_pt vk pt)
          | _ -> false
        end
  | Star(f,pt) -> Name.Set.fold
      (fun k okSoFar ->
         okSoFar && (not (List.mem k f)) &&
           (member_pt (V.get_required v k) pt))
        (V.dom v)
        true
  | Union (us) -> List.fold_left
      (fun acc ui -> 
         if acc then true
         else member_pt v ui)
        false
        us
  | Cat (cs) ->
      let rec split v pt =
	match pt with
	  | Empty      -> (None, v)
	  | Name(n,pt) ->
	      (match V.get v n with
	         | None       -> (None, v)
	         | Some vn    -> (Some (vn,pt), V.set v n None))
	  | Bang(f,pt)  ->
	      let n = Name.Set.choose (V.dom v) in
	      let vn = V.get_required v n in
	        if (List.mem n f) then (None, v)
	        else (Some (vn,pt), V.set v n None)
	  | Star(f,pt)  -> (Some (v,pt), V.empty)
	  | Var(q,ctx)    -> split v (eval_pt pt)
	      
	  | App(q,ctx)    -> split v (eval_pt pt)
	  | _            -> debug (sprintf "ERROR: the impossible happened. Non-atomic type in TCat; t=%s" (string_of_pt pt));
	      assert false (* can't happen *)
      in
      let rec loop cs v =
	if (V.is_empty v) then
	  (* all the cs must be Stars *)
	  List.fold_left (fun ok h -> match h with Star _ -> ok | _ -> false) true cs
	else
	  match cs with
	    | []   -> false
	    | [ci] -> member_pt v ci
	    | ci::rest ->
		match split v ci with
		  | (None,_)             -> false
		  | (Some (vk,pt), vrest) -> (member_pt vk pt) && (loop rest vrest)
      in
	loop cs v
  | Var(_,_) -> member_pt v (eval_pt pt0)
  | App(_,_) -> member_pt v (eval_pt pt0)
  | Fun(_)   -> assert false

(* type domains *)
type tdom_atom =
    DAny of Name.Set.t
  | DAll of Name.Set.t
  | DName of string
      
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
      Cat(_) ->
	(match t with
	     Cat(_) | Union(_) -> raise (Error.Run_error("Type is not in normal form")) 
	   | _ -> f t)
    | Union(_) -> 
	(match t with 
	     Union(_) -> raise (Error.Run_error("Type is not in normal form")) 
	   | _ -> f t)
    | _ -> f t
		    
let rec tdoms t =
  match t with 
      TT pt -> tdoms_pt pt
    | _     -> assert false
and tdoms_pt pt = 
  let nameset lst = 
    List.fold_left (fun ns n -> Name.Set.add n ns) Name.Set.empty lst
  in 
  let shallow_union f lst = 
    List.fold_left (fun acc td -> TDoms.union acc (f td)) TDoms.empty lst     
  in
  let deep_union f lst = 
    TDoms.singleton (TDoms.fold (fun acc d -> TDom.union acc d) (shallow_union f lst) TDom.empty)
  in
    match pt with
	Union(tl)  -> shallow_union (nfcheck pt tdoms_pt) tl
      | Cat(tl)    -> deep_union (nfcheck pt tdoms_pt) tl
      | Name(m,x)  -> TDoms.singleton (TDom.singleton (DName(m))) 
      | Bang(f,x)  -> TDoms.singleton (TDom.singleton (DAny(nameset f))) 
      | Star(f,x)  -> TDoms.singleton (TDom.singleton (DAll(nameset f))) 
      | Empty      -> TDoms.empty
      | Var(_) | App(_) -> tdoms_pt (eval_pt pt)
      | Fun(_) -> assert false

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
