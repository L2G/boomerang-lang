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

let failAt i mesg = 
  Printf.eprintf "Fatal error at %s: %s" (Info.string_of_t i) mesg; 
  exit 1

(* TYPES *)
type i = Info.t
type t = TT of pt | NT of pt 
and pt = 
    Empty of i
  | Var of i * Syntax.qid * thunk
  | App of i * pt * pt * thunk
  | Fun of i * (pt -> pt)
  | Name of i * string * pt
  | Star of i * (string list) * pt
  | Bang of i * (string list) * pt
  | Cat of i * pt list 
  | Union of i * pt list 
and thunk = unit -> pt

let rec info_of_t = function
    TT pt -> info_of_pt pt
  | NT pt -> info_of_pt pt
and info_of_pt = function
    Empty(i)     -> i
  | Var(i,_,_)   -> i
  | App(i,_,_,_) -> i
  | Fun(i,_)     -> i
  | Name(i,_,_)  -> i
  | Star(i,_,_)  -> i
  | Bang(i,_,_)  -> i
  | Cat(i,_)   -> i
  | Union(i,_) -> i

(* types *)
let rec string_of_t = function 
    TT pt -> string_of_pt pt
  | NT pt -> sprintf "~(%s)" (string_of_pt pt)

and string_of_pt = function
  | Empty(_) -> "empty"
  | Var (_,x,_) -> Syntax.string_of_qid x
  | App(_,pt1,pt2,_) -> sprintf "(%s %s)" (string_of_pt pt1) (string_of_pt pt2)
  | Fun (_,_) -> "<type fun>"
  | Name(_,n,pt) -> sprintf "%s = %s" n (string_of_pt pt)
  | Bang(_,f,pt)  -> 
      sprintf "!%s = %s"
	(if f = [] then "" else "\\" ^ braces (concat_list ", " f))
	(string_of_pt pt)  
  | Star(_,f,pt)  ->
     sprintf "*%s = %s"
	(if f = [] then "" else "\\" ^ braces (concat_list ", " f))
	(string_of_pt pt)
  | Cat(_,cs)   -> curlybraces (concat_list ", " (Safelist.map string_of_pt cs))
  | Union(_,ts)  -> braces (concat_list " | " (Safelist.map string_of_pt ts))

(* STUBS *)
let eval_pt pt = match pt with 
    Var(_,_,thk) -> thk () 
  | App(_,_,_,thk) -> thk ()
  | _            -> pt
      
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

(* a VERY conservative approximation of equality *)
(* assumes that types are in normal form *)
let rec eq_ptyp pt1 pt2 = match pt1,pt2 with
    Var(_,x1,_),Var(_,x2,_) -> Syntax.qid_compare x1 x2 = 0
  | Name(_,n1,ptn1),Name(_,n2,ptn2) -> (n1=n2) && (eq_ptyp ptn1 ptn2)
  | Star(_,f1,ptx1),Star(_,f2,ptx2) -> (f1=f2) && (eq_ptyp ptx1 ptx2)
  | Bang(_,f1,ptx1),Bang(_,f2,ptx2) -> (f1=f2) && (eq_ptyp ptx1 ptx2)
  | Cat(_,cs1), Cat(_,cs2)            ->       
      if (Safelist.length cs1 <> Safelist.length cs2) then false
      else Safelist.fold_left 
	(fun ok (ci1,ci2) -> ok && (eq_ptyp ci1 ci2))
	true
	(Safelist.combine cs1 cs2)
  | Union(_,us1), Cat(_,us2) ->       
      if (Safelist.length us1 <> Safelist.length us2) then false
      else Safelist.fold_left 
	(fun ok (ui1,ui2) -> ok && (eq_ptyp ui1 ui2))
	true
	(Safelist.combine us1 us2)
  | App(_,pt11,pt12,_),App(_,pt21,pt22,_) -> eq_ptyp pt11 pt21 && eq_ptyp pt21 pt22      
  | _ -> false 
    
let rec cmp_typ t1 t2 = match t1,t2 with
    NT(_),TT(_)      -> lt
  | TT(_), NT(_)     -> gt	
  | NT(pt1),NT(pt2)  -> cmp_ptyp pt1 pt2
  | TT(pt1), TT(pt2) -> cmp_ptyp pt1 pt2	  
and cmp_ptyp t1 t2 =
  match t1,t2 with      
      (* Fun *)
      Fun(_),Fun(_)             -> eq
    | Fun(_),_                  -> lt
    | _,Fun(_)                  -> gt
	
    (* | Var(x,xctx),Var(y,yctx) -> SHORT CIRCUIT SAME VARIABLE, SAME CONTEXT? *)
    | Var(_,x,_),_              -> cmp_ptyp (eval_pt t1) t2
    | _,Var(_,x,_)              -> cmp_ptyp t1 (eval_pt t2)

    (* App *)
    | App(_,pt1,pt2,_),_        -> cmp_ptyp (eval_pt pt1) t2
    | _,App(_,pt1,pt2,_)        -> cmp_ptyp t1 (eval_pt pt2)
	
    (* Empty *)
    | Empty(_),Empty(_)         -> eq
    | Empty(_),_                -> lt
    | _,Empty(_)                -> gt
	
    (* Name *)
    | Name(_,n1,_),Name(_,n2,_) -> compare n1 n2
    | Name(_,_,_),_             -> lt
    | _,Name(_,_,_)             -> gt
	
    (* Bang *)
    | Bang(_,f1,_),Bang(_,f2,_) -> cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare
    | Bang(_,_,_),_             -> lt
    | _,Bang(_,_,_)             -> gt
	
    (* Star *)
    | Star(_,f1,_),Star(_,f2,_) -> cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare
    | Star(_,_,_),_             -> lt
    | _,Star(_,_,_)             -> gt
	
    (* Cat *)
    | Cat(_,cs1),Cat(_,cs2)    -> cmp_lex (Safelist.sort cmp_ptyp cs1) (Safelist.sort cmp_ptyp cs2) cmp_ptyp
    | Cat(_),_                 -> lt
    | _,Cat(_)                 -> gt
	
    (* Union *)           
    | Union(_,us1),Union(_,us2) -> cmp_lex (Safelist.sort cmp_ptyp us1) (Safelist.sort cmp_ptyp us2) cmp_ptyp

      
(* sort a type list *)
let sort_ptyp pt = pt (* STUB *)
let sort_ptyp_list tl = Safelist.sort cmp_ptyp tl
	  
(*** UTILITIES ***)
(* pt2nf/t2nf: convert a (pre)type to normal form *)
let rec t2nf t0 = match t0 with
      NT pt -> NT (pt2nf pt)
    | TT pt -> TT (pt2nf pt)
and pt2nf pt0 = pt2nf_aux true pt0
and pt2nf_aux dethunk pt0 = 
  let res = match pt0 with      
  | Empty(_)       -> pt0
  | Fun(_)         -> pt0
  | Var(_,_,_)     -> if dethunk then pt2nf_aux true (eval_pt pt0) else pt0
  | App(_,_,_,_)   -> pt2nf_aux dethunk (eval_pt pt0) 
  | Name(i,n,pt1)  -> Name(i, n, pt2nf_aux false pt1)
  | Bang(i,es,pt1) -> Bang(i, Safelist.sort compare es, pt2nf_aux false pt1)
  | Star(i,es,pt1) -> Star(i, Safelist.sort compare es, pt2nf_aux false pt1)      
      
  (* unions:
     - recursively normalize
     - lift nested unions
     - remove obviously Empty types from unions
  *)
  | Union(i,us)  ->
      let us_nf = Safelist.map (pt2nf_aux dethunk) us in
      let lifted_us = Safelist.fold_left
	(fun acc hpt ->
	   match hpt with
	       Empty(_)    -> acc    (* skip TEmpty bits of unions *)
	     | Union(_,us) -> us@acc (* lift nested unions *)
	     | _           -> hpt::acc)
	[]
	us_nf
      in
	begin
	  match lifted_us with
	    | []           -> Empty(i)
	    | [(Cat(_,[]))]   -> Empty(i)
	    | [(Union(_,[]))] -> Empty(i)
	    | _            -> sort_ptyp (Union(i,lifted_us))
	end
  (* TCat:
     - recursively normalize cs
     - lift nested cats in cs
     - distribute nested unions over cats
     - sort result
     - if a name is repeated, then TEmpty
  *)
  | Cat(i,cs)    ->
      let cs_nf = Safelist.map (pt2nf_aux dethunk) cs in		
      (* temporarily represent the lifted union of cats as a list of lists *)
      let lift_us_rep = Safelist.fold_left
	(fun acc h ->
	   (* helper functions for readability *)
	   let prepend s a = Safelist.map (fun x -> sort_ptyp_list (s::x)) a in
	   let concat cs a = Safelist.map (fun x -> sort_ptyp_list (x @ cs)) a in
	     match h with
	       | Cat(_,cs)   -> concat cs acc (* lift nested cats *)
	       | Union(_,us) ->
		   (* distribute unions, lift deep-nested cats *)
		   Safelist.flatten
		     (Safelist.map
			(fun u -> match u with
			   | Cat(_,cs) -> concat cs acc			     
			   | _         -> prepend u acc) us)
	       | _         -> prepend h acc)
	[[]]
	cs_nf
      in
      let check_repeats t =
	let rec cr_aux cs acc =
	  (* this used to be more complicated; keeping the structure for now *)
	  match cs with
	    | []                                           -> Cat (i,Safelist.rev acc)
	    | Name(_,n,_)::Name(_,m,_)::rest when (n = m)  -> Empty(i)
	    | h::t                                         -> cr_aux t (h::acc)
	in
	  match t with
	      Cat(_,cs) -> cr_aux cs []
	    | t         -> t
		
      in		  
	begin
	  match lift_us_rep with
	    | [[]]          -> Empty(i) (* recognize some obviously empty types *)
	    | [[Empty(_)]]     -> Empty(i)
	    | [[(Cat(_,[]))]]  -> Empty(i)
	    | [[t]]         -> check_repeats t
	    | [cs]          -> check_repeats (Cat(i,cs))
	    | _             -> Union (i, Safelist.map (fun x -> check_repeats (Cat(i,x))) lift_us_rep)
	end
  in
  let _ = debug (sprintf "p2nf_aux %s %s ---> %s" (string_of_bool dethunk) (string_of_pt pt0) (string_of_pt res)) in
    res

(* (\* a conservative approximation of CED *\) *)
(* module type SMap = Mapplus.Make( *)
(*   struct *)
(*     type t = string *)
(*     let compare = compare *)
(*     let to_string = (fun s -> s) *)
(*   end) *)

(* let rec is_consistent t0 = match t2nf t0 with *)
(*     TT pt -> is_consistent_pt pt *)
(*   | NT pt -> false *)
(* and is_consistent_pt pt0 =  *)
(*   match pt0 with *)
(*       Empty(_) -> true *)
(*     | Fun(_) -> true ? *)
(*     | App(_) | Var(_) -> is_consistent_pt (eval_pt pt0) *)
(*     | Name(_) | Star(_) | Bang(_) -> true *)
(*     | Union(_,us) -> true STUB *)
(*     | Cat(_,cs) ->  *)
(* 	let kids_map = SMap.Map.empty in *)
	  
let rec project t0 n = match t0 with 
    TT pt -> 
      begin match project_pt pt n with 
	  None -> None
	| Some ptn -> Some (TT (ptn))
      end
  | _ -> failAt (info_of_t t0) "can't project on a negative type"
and project_pt pt0 n =
  let _ = debug (Format.sprintf "--- PROJECT_PT --- %s from t=%s\n" n (string_of_pt pt0)) in
    match pt0 with
      | Empty(_)        -> None
      | Name(_,m,pt)   -> if (n=m) then Some pt else None
      | Bang(_,f,pt)   -> if (Safelist.mem n f) then None else Some pt
      | Star(_,f,pt)   -> if (Safelist.mem n f) then None else Some pt
      | Union(_,pts)  -> Safelist.fold_left 
 	  (fun xo pti -> 
	     match xo with 
 		 None -> project_pt pti n 
	       | Some x -> 
		   begin match project_pt pti n with
		       None -> xo
		     | Some ptin ->
			 if eq_ptyp ptin x then xo
			 else  failAt (info_of_pt pt0)
			   (sprintf "Fatal error: %s is not projectable on %s; %s <> %s"
			      (string_of_pt pt0) n
			      (string_of_pt pti)
			      (string_of_pt x))
		   end)
	    None
	    pts
      | Cat(i,pts) -> Safelist.fold_left
	  (fun xo pti -> match xo with
	       None   -> project_pt pti n
	     | Some x -> 
		 begin
		   match project_pt pti n with 
		       None -> xo
		     | Some ptin -> 
			 if eq_ptyp ptin x then xo
			 else failAt (info_of_pt pt0)
			   (sprintf "Fatal error: %s is not projectable on %s; %s <> %s"
			      (string_of_pt pt0) n
			      (string_of_pt pti)
			      (string_of_pt x))
		 end)
	    None
	    pts
      | Var(_,q,thk)        -> project_pt (eval_pt pt0) n
      | App(_,pt1, pt2,thk) -> project_pt (eval_pt pt0) n
      | Fun(_,_)            -> assert false
	  
let rec member v t = match t2nf t with 
    TT pt -> member_pt v pt
  | NT pt -> not (member_pt v pt)
and member_pt v pt0 = 
  (* sanity check for projectability *)
  let _ = V.fold (fun k _ () -> let _ = project_pt pt0 k in ()) v () in
  
  let res = match pt0 with
    Empty(i)     -> false
  | Fun(_,_)     -> false
  | Var(_,_,_)   -> member_pt v (eval_pt pt0)
  | App(_,_,_,_) -> member_pt v (eval_pt pt0)
  | Name(_,n,pt) ->
      let d = V.dom v in
        begin match Name.Set.cardinal d with
          | 1 ->
       	      let k = Name.Set.choose d in
       	      let vk = V.get_required v k in
       		(k = n) && (member_pt vk pt)
          | _ -> false
        end
  | Bang(_,f,pt) ->
      let d = V.dom v in
        begin match Name.Set.cardinal d with
          | 1 ->
       	      let k = Name.Set.choose d in
       	      let vk = V.get_required v k in
       		(not (List.mem k f)) && 
       		  (member_pt vk pt)
          | _ -> false
        end
  | Star(_,f,pt) -> Name.Set.fold
      (fun k okSoFar ->
         okSoFar && (not (List.mem k f)) &&
           (member_pt (V.get_required v k) pt))
        (V.dom v)
        true
  | Union (_,us) -> List.fold_left
      (fun acc ui -> 
         if acc then true
         else member_pt v ui)
        false
        us
  | Cat (_,cs) ->
      let rec split v pt =
	match pt with
	  | Empty(_)   -> (None, v)
	  | Name(_,n,pt) ->
	      (match V.get v n with
	         | None       -> (None, v)
	         | Some vn    -> (Some (vn,pt), V.set v n None))
	  | Bang(_,f,pt)  ->
	      let n = Name.Set.choose (V.dom v) in
	      let vn = V.get_required v n in
	        if (List.mem n f) then (None, v)
	        else (Some (vn,pt), V.set v n None)
	  | Star(_,f,pt)  -> (Some (v,pt), V.empty)
	  | _ -> 
	      failAt (info_of_pt pt)
		(sprintf "non-atomic type in concatenation: %s"
		   (string_of_pt pt))
      in
      let rec loop cs v =
	let lres = 
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
	let _ = debug (sprintf "loop %s %s --> %s" (brackets (concat_list ", " (Safelist.map string_of_pt cs))) (V.string_of_t v) (string_of_bool lres)) in 
	  lres
      in
	loop cs v
  in
  let _ = debug (sprintf "member_pt(%s, %s) --> %s" (V.string_of_t v) (string_of_pt pt0) (string_of_bool res)) in
    res
      
(* type domains *)
type tdom_atom =
    DAny of Name.Set.t
  | DAll of Name.Set.t
  | DName of string
      
let cmp_tdom_atom a1 a2 = match a1,a2 with
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
	  
module TDom = 
  Set.Make(struct
	     type t = tdom_atom
	     let compare = cmp_tdom_atom
	   end)

module TDoms = 
  Set.Make(struct
 	     type t = TDom.t
	     let compare = compare
	   end)  
  
let atom2str ta = match ta with
    DAny s -> "!"
  | DAll s -> "*"
  | DName n -> n

let concat_tdom sep td = concat TDom.fold sep atom2str td
let tdom2str td = curlybraces (concat_tdom " " td)

let concat_tdoms sep tds = concat TDoms.fold sep tdom2str tds
let tdoms2str tds = curlybraces (concat_tdoms " " tds)

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
	Union(_,tl)  -> shallow_union (nfcheck pt tdoms_pt) tl
      | Cat(_,tl)    -> deep_union (nfcheck pt tdoms_pt) tl
      | Name(_,m,x)  -> TDoms.singleton (TDom.singleton (DName(m))) 
      | Bang(_,f,x)  -> TDoms.singleton (TDom.singleton (DAny(nameset f))) 
      | Star(_,f,x)  -> TDoms.singleton (TDom.singleton (DAll(nameset f))) 
      | Empty(_)     -> TDoms.empty
      | Var(_) | App(_) -> tdoms_pt (eval_pt pt)
      | Fun(i,_) -> failAt i "can't compute tdom of a type function"
	  
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
