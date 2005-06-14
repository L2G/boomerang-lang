(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* type.ml - representation and functions on types       *)
(*********************************************************)
(* $Id$ *)

open Value

(* imports *)
let sprintf = Printf.sprintf
let debug = Trace.debug "type" 
let fatal_error msg = raise (Error.Harmony_error (fun () -> Format.printf "Fatal error in type : %s" msg))

(* re-export [Value.ty] and [Value.string_of_ty] in this name space *)
type t = Value.ty
let string_of_t = Value.string_of_ty

(* constants *)
let mk_nil i = Value.Cat(i,[Value.Atom(i,V.nil_tag,Value.Cat(i,[]))])
let mk_cons i h t = Value.Cat(i, [Value.Atom(i, V.hd_tag, h); 
				  Value.Atom(i, V.tl_tag, t)])

let force t0 = match t0 with 
    Var(_,_,f) 
  | App(_,_,_,f) -> Value.get_type (Info.M "forcing type expression") (f ())
  | _            -> t0
          
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

(* compare on types *)
let rec cmp_type t1 t2 = match t1,t2 with      
    (* t1,t2 have same shape *)
    Empty(_),Empty(_)         
  | Any(_),Any(_)             -> eq
  | Var(_,x,_),Var(_,y,_)     -> compare x y
  | App(_,v11,v12,_),App(_,v21,v22,_) ->
      if compare v11 v21 = eq then eq 
      else compare v12 v22 
  | Atom(_,n1,t1),Atom(_,n2,t2) -> 
      if (compare n1 n2 = eq) then eq 
      else cmp_type t1 t2
  | Bang(_,f1,t1),Bang(_,f2,t2) 
  | Star(_,f1,t1),Star(_,f2,t2) ->
      if (cmp_lex 
	    (Safelist.sort compare f1) 
	    (Safelist.sort compare f2) 
	    compare) = eq 
      then eq
      else cmp_type t1 t2
  | Cat(_,ts1),Cat(_,ts2)    	
  | Union(_,ts1),Union(_,ts2) ->
      cmp_lex 
	(Safelist.sort cmp_type ts1) 
	(Safelist.sort cmp_type ts2) 
	cmp_type

    (* t1,t2 have different shapes *)
    | Empty(_),_                -> lt
    | _,Empty(_)                -> gt
    | Any(_),_                  -> lt
    | _,Any(_)                  -> gt
    | Var(_),_                  -> lt
    | _,Var(_)                  -> gt
    | App(_),_                  -> lt
    | _,App(_)                  -> gt
    | Atom(_,_,_),_             -> lt
    | _,Atom(_,_,_)             -> gt
    | Bang(_,_,_),_             -> lt
    | _,Bang(_,_,_)             -> gt
    | Star(_,_,_),_             -> lt
    | _,Star(_,_,_)             -> gt
    | Cat(_),_                  -> lt
    | _,Cat(_)                  -> gt
      
(* sort a type *)
let rec sort_type t = match t with
    Atom(i,n,t) -> Atom(i,n,sort_type t)
  | Bang(i,f,t) -> Bang(i,f,sort_type t)
  | Star(i,f,t) -> Star(i,f,sort_type t)
  | Cat(i,ts)   -> Cat(i,sort_type_list ts)
  | Union(i,ts) -> Union(i,sort_type_list ts)
  | _           -> t
and sort_type_list ts = Safelist.sort cmp_type (Safelist.map sort_type ts)
	  
(* normalizer *)
let t2nf t0 = 
  let rec t2nf_aux depth t0 = 
    match sort_type t0 with
	Empty(_) | Any(_) -> t0
      | Var(_,_,_)     -> if (depth < 1) then t2nf_aux depth (force t0) else t0
      | App(_,_,_,_)   -> if (depth < 1) then t2nf_aux depth (force t0) else t0
      | Atom(i,n,t)  -> Atom(i, n, t2nf_aux (depth + 1) t)
      | Bang(i,f,t) -> Bang(i,f,t2nf_aux (depth + 1) t)
      | Star(i,f,t) -> Star(i,f,t2nf_aux (depth + 1) t)
	  
      (* Union:
	 - recursively normalize
	 - lift nested unions
	 - remove obviously Empty types from unions
      *)
      | Union(i,ts)  ->
	  let ts_nf = Safelist.map (t2nf_aux depth) ts in
	  let lifted_ts = Safelist.fold_left
	    (fun acc ti ->
	       match ti with
		   Empty(_)    -> acc    (* skip TEmpty bits of unions *)
		 | Union(_,us) -> us@acc (* lift nested unions *)
		 | _           -> ti::acc)
	    []
	    ts_nf
	  in begin match lifted_ts with
	      []              
	    | [Union(_,[])] -> Empty(i)
	    | _             -> Union(i,lifted_ts)
	    end
	       
      (* Cat:
	 - recursively normalize cs
	 - lift nested cats in cs
	 - distribute nested unions over cats
	 - sort result
	 - if a name is repeated, then TEmpty
      *)
      | Cat(i,ts)    ->
	  (* temporarily represent the lifted union of cats as a list of lists *)
	  let ts_nf = Safelist.map (t2nf_aux depth) ts in	  	  
	  let lift_ts_rep = Safelist.fold_left
	    (fun acc ti ->
	       (* helper functions for readability *)
	       let prepend s a = Safelist.map (fun x -> sort_type_list (s::x)) a in
	       let concat cs a = Safelist.map (fun x -> sort_type_list (x @ cs)) a in
		 match ti with
	             Cat(_,cs) -> concat cs acc (* lift nested cats *)
		   | Union(_,us) ->
		       (* distribute unions, lift deep-nested cats *)
		       Safelist.flatten 
			 (Safelist.map
			    (function
			       | Cat(_,ts'') -> concat ts'' acc
			       | ui         -> prepend ui acc) us)
		   | _         -> prepend ti acc)
	    [[]]
	    ts_nf
	  in begin match lift_ts_rep with
	      [[Empty(_)]] -> Empty(i)
	    | [[ti]]       -> ti
	    | [cs]         -> Cat(i,cs)
	    | _            -> Union(i, Safelist.map (fun ri -> Cat(i,ri)) lift_ts_rep)	      
	    end
  in     
  let res = t2nf_aux 0 t0 in
    debug (fun () ->
    	     Printf.eprintf "t2nf %s = %s\n%!"	       
    	       (string_of_t t0)
	       (string_of_t res))
    ;
    res
(* a VERY conservative approximation of equality *)
(* assumes that types are in normal form *)
let rec equal t1 t2 = match t1,t2 with
    Empty(_),Empty(_) 
  | Any(_),Any(_)                   -> true
  | Var(_,x1,_),Var(_,x2,_)         -> Syntax.qid_compare x1 x2 = 0
  | Atom(_,n1,tn1),Atom(_,n2,tn2)   -> (n1=n2) && (equal tn1 tn2)
  | Star(_,f1,tx1),Star(_,f2,tx2)   -> (f1=f2) && (equal tx1 tx2)
  | Bang(_,f1,tx1),Bang(_,f2,tx2)   -> (f1=f2) && (equal tx1 tx2)
  | Cat(_,ts1), Cat(_,ts2) ->
      (Safelist.fold_left
	 (fun ok (ti1,ti2) -> ok && (equal ti1 ti2))
	 (Safelist.length ts1 = Safelist.length ts2)
	 (Safelist.combine ts1 ts2))	
  | Union(_,ts1), Cat(_,ts2) ->
      (Safelist.fold_left
	 (fun ok (ti1,ti2) -> ok && (equal ti1 ti2))
	 (Safelist.length ts1 = Safelist.length ts2)
	 (Safelist.combine ts1 ts2))
  |_ -> false
      
let rec project t0 n = 
  let t0_nf = t2nf t0 in
    match t2nf t0 with
	Empty(_)        -> None
      | Any(i)          -> Some (Any(i))
      | Atom(_,m,t)   -> if (n=m) then Some t else None
      | Bang(_,f,t)   -> if (Safelist.mem n f) then None else Some t
      | Star(_,f,t)   -> if (Safelist.mem n f) then None else Some t
      | Union(i,ts)  -> 
	  Safelist.fold_left
 	    (fun xo ti ->
	       match xo with
 		   None -> project ti n
		 | Some x -> begin
		     match project ti n with
			 None -> xo
		       | Some tin ->
			   if equal tin x then xo
			   else fatal_error
			     (sprintf "%s is not projectable on %s; %s <> %s"
				(string_of_t t0)
				n
				(string_of_t ti)
				(string_of_t x))
		   end)
	    None
	    ts
      | Cat(i,ts) -> Safelist.fold_left
	  (fun xo ti -> match xo with
	       None   -> project ti n
	     | Some x -> begin
		 match project ti n with
		     None -> xo
		   | Some tin ->
		       if equal tin x then xo
		       else fatal_error
			 (sprintf "%s is not projectable on %s; %s <> %s"
			    (string_of_t t0) n
			    (string_of_t ti)
			    (string_of_t x))
	       end)
	    None
	    ts
      | Var(_) | App(_) -> project (force t0_nf) n
	  (* debug (fun () ->                                                   *)
	  (*     Printf.eprintf "project type %s on %s = %s\n%!"                *)
	  (*       (string_of_t t0)                                             *)
	  (*       n                                                            *)
	  (*       (match res with None -> "NONE" | Some t -> (string_of_t t))) *)
	  
let rec member v t0 = 
  let t0_nf = t2nf t0 in
(*     debug (fun () -> *)
(*     	     Printf.eprintf "checking member %s in %s\n%!" *)
(*     	       (V.string_of_t v) *)
(*     	       (string_of_t t0_nf)) *)
(*     ; *)
  match t0_nf with    
    Empty(i) -> false
  | Any(_)   -> true
  | Var(_) | App(_) -> member v (force t0_nf)
  | Atom(_,n,t) ->
      let d = V.dom v in
        begin match Name.Set.cardinal d with
          | 1 ->
       	      let k = Name.Set.choose d in
       	      let vk = V.get_required v k in
       		(k = n) && (member vk t)
          | _ -> false
        end
  | Bang(_,f,t) ->
      let d = V.dom v in
        begin match Name.Set.cardinal d with
          | 1 ->
       	      let k = Name.Set.choose d in
       	      let vk = V.get_required v k in
       		(not (Safelist.mem k f)) &&
       		  (member vk t)
          | _ -> false
        end
  | Star(_,f,t) -> 
      Name.Set.fold
	(fun k okSoFar ->
           okSoFar 
	   && (not (Safelist.mem k f)) 
	   && (member (V.get_required v k) t))
        (V.dom v)
        true
  | Union (_,ts) -> 
      Safelist.fold_left
	(fun mem ti -> mem or (member v ti))
        false
        ts
  | Cat (_,ts) ->
      let rec split v t1 = match t1 with
	  Empty(_)    -> (None, v)
	| Any(i)      -> (Some (v,t1), V.empty)
	| Atom(_,n,t) -> begin
	    match V.get v n with
	      | None       -> (None, v)
	      | Some vn    -> (Some (vn,t), V.set v n None)
	  end
	| Bang(_,f,t)  ->
	    let n = Name.Set.choose (V.dom v) in
	    let vn = V.get_required v n in
	      if (List.mem n f) then (None, v)
	      else (Some (vn,t), V.set v n None)
	| Star(_,f,t)  -> (Some (v,t1), V.empty)
	| _ ->
	    fatal_error
	      (sprintf "non-atomic type in concatenation: %s"
		 (string_of_t t1))
      in
      let rec loop ts v =
	(* debug (fun () -> Printf.eprintf "loop {%s} %s\n%!"
	   (Misc.concat_list "," (Safelist.map string_of_t ts))
	   (V.string_of_t v)); *)
	(* sanity check for projectability;
	   FIXME: should really just do this once during normalization *)
	let _ = V.fold (fun k _ () -> let _ = project t0 k in ()) v () in
	  if (V.is_empty v) then
	    (* either ts = [] or all ts are Stars *)
	    Safelist.fold_left (fun ok h -> match h with Star _ -> ok | _ -> false) true ts
	  else match ts with
	    | []   -> false
	    | [ti] -> member v ti
	    | ti::rest ->
		match split v ti with
		    (None,_)             -> false
		  | (Some (vk,t), vrest) -> (member vk t) && (loop rest vrest)
      in
	loop ts v
	  (*   debug (fun () ->  *)
	  (* 	   Printf.eprintf "member %s %s = %b\n%!" *)
	  (* 	     (V.string_of_t v) *)
	  (* 	     (string_of_t t0) *)
	  (* 	     res) *)
	  
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

let concat_tdom sep td = Misc.concat TDom.fold sep atom2str td
let tdom2str td = Misc.curlybraces (concat_tdom " " td)
  
let concat_tdoms sep tds = Misc.concat TDoms.fold sep tdom2str tds
let tdoms2str tds = Misc.curlybraces (concat_tdoms " " tds)

let nfcheck tbase f t =match tbase with
    Cat(i,_) ->
      (match t with
	   Cat(_) | Union(_) -> fatal_error (sprintf "%s: Type %s is not in normal form" (string_of_t tbase) (Info.string_of_t i))
	 | _ -> f t)
  | Union(i,_) ->
      (match t with
	   Union(_) -> fatal_error (sprintf "%s: Type %s is not in normal form" (string_of_t tbase) (Info.string_of_t i))
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
  in match t2nf t with
      Union(_,tl)  -> shallow_union (nfcheck t tdoms) tl
    | Cat(_,tl)    -> deep_union (nfcheck t tdoms) tl
    | Atom(_,m,x)  -> TDoms.singleton (TDom.singleton (DName(m)))
    | Bang(_,f,x)  -> TDoms.singleton (TDom.singleton (DAny(nameset f)))
    | Star(_,f,x)  -> TDoms.singleton (TDom.singleton (DAll(nameset f)))
    | Empty(_)     -> TDoms.empty
    | Any(_)       -> TDoms.singleton (TDom.singleton (DAll(Name.Set.empty)))
    | Var(_,_,thk) | App(_,_,_,thk) -> tdoms (Value.get_type (Info.M "computing type domain") (thk ()))
	  
let rec vdom_in_tdoms vd tds  = 
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
