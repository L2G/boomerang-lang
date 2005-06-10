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
let fatal_error msg = raise (Error.Fatal_error (sprintf "Fatal error in type : %s" msg))

(* re-export [Value.ty] and [Value.string_of_ty] in our name space *)
type t = Value.ty
let string_of_t = Value.string_of_ty

(* constants *)
let mk_nil i = Value.Cat(i,[Value.Atom(i,V.nil_tag,Value.Cat(i,[]))])
let mk_cons i h t = Value.Cat(i, [Value.Atom(i, V.hd_tag, h); 
				  Value.Atom(i, V.tl_tag, t)])

let force t0 = match t0 with 
    Var(_,_,f) | App(_,_,_,f) -> Value.get_type (Info.M "forcing type expression") (f ())
  | _                         -> t0

(* (\* a (somewhat arbitrary) order on types: used in t2nf *\) *)
(* let eq = 0     (\* symbolic comparisons *\) *)
(* let lt = -1 *)
(* let gt = 1 *)
  
(* (\* helper for comparing lists of things in dictionary order *\) *)
(* let rec cmp_lex l1 l2 cmp_f = *)
(*   let rec cmp_lex_aux l1 l2 tie = *)
(*     match (l1,l2) with *)
(* 	([],[])           -> tie *)
(*       | (_,[])            -> gt *)
(*       | ([],_)            -> lt *)
(*       | (h1::t1),(h2::t2) -> *)
(* 	  if (tie = eq) then cmp_lex_aux t1 t2 (cmp_f h1 h2) *)
(* 	  else cmp_lex_aux t1 t2 tie *)
(*   in *)
(*     cmp_lex_aux l1 l2 eq *)

(* a VERY conservative approximation of equality *)
(* assumes that types are in normal form *)
let rec eq t1 t2 = match t1,t2 with
    Empty(_),Empty(_)               -> true
  | Any(_),Any(_)                   -> true
  | Var(_,x1,_),Var(_,x2,_)         -> Syntax.qid_compare x1 x2 = 0
  | Atom(_,n1,t1),Atom(_,n2,t2) -> (n1=n2) && (eq t1 t2)
  | Star(_,f1,t1),Star(_,f2,t2) -> (f1=f2) && (eq t1 t2)
  | Bang(_,f1,t1),Bang(_,f2,t2) -> (f1=f2) && (eq t1 t2)
  | Cat(_,ts1), Cat(_,ts2)          ->
      if (Safelist.length ts1 <> Safelist.length ts2) then false
      else Safelist.fold_left
	(fun ok (ti1,ti2) -> ok && (eq ti1 ti2))
	true
	(Safelist.combine ts1 ts2)
  | Union(_,ts1), Cat(_,ts2)        ->
      if (Safelist.length ts1 <> Safelist.length ts2) then false
      else Safelist.fold_left
	(fun ok (ti1,ti2) -> ok && (eq ti1 ti2))
	true
	(Safelist.combine ts1 ts2)
  | App(_),App(_) -> false
  | _             -> false
    
(* let rec cmp_t t1 t2 = match t1,t2 with *)
(*     NT(_),TT(_)      -> lt *)
(*   | TT(_), NT(_)     -> gt	 *)
(*   | NT(pt1),NT(pt2) -> cmp_pt pt1 pt2 *)
(*   | TT(pt1),TT(pt2) -> cmp_pt pt1 pt2 *)
(* and cmp_pt (_,_,itr1) (_,_,itr2) = cmp_it !itr1 !itr2 *)
(* and cmp_it t1 t2 = *)
(*   match t1,t2 with       *)
(*       Empty(_),Empty(_)         -> eq *)
(*     | Empty(_),_                -> lt *)
(*     | _,Empty(_)                -> gt *)
(*     | Any(_),Any(_)             -> eq *)
(*     | Any(_),_                  -> lt *)
(*     | _,Any(_)                  -> gt *)
(*     | Fun(_),Fun(_)             -> eq *)
(*     | Fun(_),_                  -> lt *)
(*     | _,Fun(_)                  -> gt *)
(*     | Var(_,x,_),Var(_,y,_)     -> compare x y (\* FIXME: broken? *\) *)
(*     | Var(_),_                  -> lt *)
(*     | _,Var(_)                  -> gt *)
(*     | App(_,it11,it12,_),App(_,it21,it22,_) ->  *)
(* 	let cmp1 = cmp_it it11 it21 in *)
(* 	  if cmp1 <> eq then cmp1 *)
(* 	  else cmp_it it12 it22  *)
(*     | App(_),_                  -> lt *)
(*     | _,App(_)                  -> gt     *)
(*     | Name(_,n1,_),Name(_,n2,_) -> compare n1 n2 *)
(*     | Name(_,_,_),_             -> lt *)
(*     | _,Name(_,_,_)             -> gt *)
(*     | Bang(_,f1,_),Bang(_,f2,_) ->  *)
(* 	cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare *)
(*     | Bang(_,_,_),_             -> lt *)
(*     | _,Bang(_,_,_)             -> gt *)
(*     | Star(_,f1,_),Star(_,f2,_) ->  *)
(* 	cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare *)
(*     | Star(_,_,_),_             -> lt *)
(*     | _,Star(_,_,_)             -> gt *)
(*     | Cat(_,cs1),Cat(_,cs2)    ->  *)
(* 	cmp_lex (Safelist.sort cmp_pt cs1) (Safelist.sort cmp_pt cs2) cmp_pt *)
(*     | Cat(_),_                 -> lt *)
(*     | _,Cat(_)                 -> gt *)
(*     | Union(_,us1),Union(_,us2) ->  *)
(* 	cmp_lex (Safelist.sort cmp_pt us1) (Safelist.sort cmp_pt us2) cmp_pt *)
(*     | _,Singleton(_,_) -> lt (\* FIXME: we should find what to do *\) *)
(*     | Singleton(_,_),_ -> gt (\* FIXME: we should find what to do *\) *)
      
(* (\* sort a type list *\) *)
(* let sort_it it = it (\* STUB *\) *)
(* let sort_pt_list ptl = Safelist.sort cmp_pt ptl *)
	  
(* (\*** UTILITIES ***\) *)
(* (\* pt2nf/t2nf: convert a (pre)type to normal form *\) *)
(* let rec t2nf t0 = match t0 with *)
(*     NT pt -> NT (pt2nf true pt) *)
(*   | TT pt -> TT (pt2nf true pt) *)
	  
(* and pt2nf force ((normalized,memo,itr) as pt0) =  *)
(*   let _ =  *)
(*     match force,!itr with *)
(* 	false, Var(_) -> () *)
(*       | _ ->  *)
(* 	  if not !normalized then *)
(* 	    let it_nf = it2nf force !itr in *)
(* 	      normalized := true; *)
(* 	      itr := it_nf *)
(*   in *)
(*     pt0 *)
      
(* and it2nf force it0 =  *)
(*   let res = match it0 with       *)
(*       Empty(_) | Any(_) | Fun(_) | Singleton(_) -> it0 *)
(*     | Var(_,_,_)     -> if force then it2nf force (eval_it it0) else it0 *)
(*     | App(_,_,_,_)   -> it2nf force (eval_it it0)  *)
(*     | Name(i,n,pt1)  -> Name(i, n, pt2nf false pt1) *)
(*     | Bang(i,es,pt1) -> Bang(i, Safelist.sort compare es, pt2nf false pt1) *)
(*     | Star(i,es,pt1) -> Star(i, Safelist.sort compare es, pt2nf false pt1)     	 *)
(*   (\* unions: *)
(*      - recursively normalize *)
(*      - lift nested unions *)
(*      - remove obviously Empty types from unions *)
(*   *\) *)
(*   | Union(i,us)  -> *)
(*       let us_nf = Safelist.map (pt2nf force) us in *)
(*       let lifted_us = Safelist.fold_left *)
(* 	(fun acc h_pt -> *)
(* 	     match it_of_pt h_pt with *)
(* 		 Empty(_)    -> acc    (\* skip TEmpty bits of unions *\) *)
(* 	       | Union(_,us) -> us@acc (\* lift nested unions *\) *)
(* 	       | _           -> h_pt::acc) *)
(* 	[] *)
(* 	us_nf *)
(*       in *)
(* 	begin *)
(* 	  match lifted_us with *)
(* 	    | []           -> Empty(i) *)
(* 	    | [h_pt] ->  *)
(* 		begin  *)
(* 		  match it_of_pt h_pt with *)
(* 		      (Union(_,[])) -> Empty(i) *)
(* 		    | _ -> sort_it (Union(i, lifted_us)) *)
(* 		end *)
(* 	    | _ -> sort_it (Union(i,lifted_us)) *)
(* 	end *)
(*   (\* TCat: *)
(*      - recursively normalize cs *)
(*      - lift nested cats in cs *)
(*      - distribute nested unions over cats *)
(*      - sort result *)
(*      - if a name is repeated, then TEmpty *)
(*   *\) *)
(*   | Cat(i,cs)    -> *)
(*       let cs_nf = Safelist.map (pt2nf force) cs in		 *)
(*       (\* temporarily represent the lifted union of cats as a list of lists *\) *)
(*       let lift_us_rep = Safelist.fold_left *)
(* 	(fun acc h_pt -> *)
(* 	   (\* helper functions for readability *\) *)
(* 	   let prepend s a = Safelist.map (fun x -> sort_pt_list (s::x)) a in *)
(* 	   let concat cs a = Safelist.map (fun x -> sort_pt_list (x @ cs)) a in	    *)
(* 	     match it_of_pt h_pt with *)
(* 	        Cat(_,cs)   -> concat cs acc (\* lift nested cats *\) *)
(* 	       | Union(_,us) -> *)
(* 		   (\* distribute unions, lift deep-nested cats *\) *)
(* 		   Safelist.flatten *)
(* 		     (Safelist.map *)
(* 			(fun u_pt -> match it_of_pt u_pt with *)
(* 			   | Cat(_,cs) -> concat cs acc			      *)
(* 			   | _         -> prepend u_pt acc) us) *)
(* 	       | _         -> prepend h_pt acc) *)
(* 	[[]] *)
(* 	cs_nf *)
(*       in *)
(*       let check_repeats it1 = *)
(* 	let rec cr_aux cs acc = *)
(* 	  match cs with *)
(* 	    | []                                           -> Cat (i,Safelist.rev acc) *)
(* 	    | [h]                                          -> cr_aux [] (h::acc) *)
(* 	    | h1::h2::t ->  *)
(* 		let it1 = it_of_pt h1 in *)
(* 		let it2 = it_of_pt h2 in  *)
(* 		  begin *)
(* 		    match it1,it2 with  *)
(* 			Name(_,n,_), Name(_,m,_)  *)
(* 			  when (n = m) -> Empty(i) *)
(* 		      | _ -> cr_aux (h2::t) (h1::acc)                                     *)
(* 		  end *)
(* 	in *)
(* 	  match it1 with *)
(* 	      Cat(_,cs) -> cr_aux cs [] *)
(* 	    | _ -> it1		 *)
(*       in		   *)
(* 	begin *)
(* 	  match lift_us_rep with *)
(* 	      [[h_pt]] ->  *)
(* 		begin  *)
(* 		  match it_of_pt h_pt with  *)
(* 		      Empty(_) -> Empty(i) *)
(* 		    | _ -> check_repeats (it_of_pt h_pt) *)
(* 		end *)
(* 	    | [cs] -> check_repeats (Cat(i,cs)) *)
(* 	    | _    -> Union (i, Safelist.map  *)
(* 			       (fun x ->  *)
(* 				  let it = check_repeats (Cat(i,x)) in *)
(* 				    (\* FIXME: hack! *\)  *)
(* 				    (ref true, V.Hash.create 1, ref it)) *)
(* 			       lift_us_rep) *)
(* 	end *)
(*   in *)
(*     res *)

let rec project t0 n = 
  (*   debug (fun () -> Printf.eprintf "project type %s on %s" (string_of_t t0) n); *)
  match t0 with
      Empty(_)        -> None
    | Any(i)          -> Some (Any(i))
    | Atom(_,m,t)   -> if (n=m) then Some t else None
    | Bang(_,f,t)   -> if (Safelist.mem n f) then None else Some t
    | Star(_,f,t)   -> if (Safelist.mem n f) then None else Some t
    | Union(_,ts)  -> 
	Safelist.fold_left
 	  (fun xo ti ->
	     match xo with
 		 None -> project ti n
	       | Some x -> begin
		   match project ti n with
		       None -> xo
		     | Some tin ->
			 if eq tin x then xo
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
		     if eq tin x then xo
		     else fatal_error
		       (sprintf "%s is not projectable on %s; %s <> %s"
			  (string_of_t t0) n
			  (string_of_t ti)
			  (string_of_t x))
	     end)
	  None
	  ts
    | Var(_) | App(_) -> 
	project (force t0) n
	
let rec member v t0 = 
  let result = match t0 with
      Empty(i) -> false
    | Any(_)   -> true
    | Var(_) | App(_) -> member v (force t0)
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
	  debug (fun () -> Printf.eprintf "loop {%s} %s\n%!"
		   (Misc.concat_list "," (Safelist.map Type.string_of_t ts))
		   (V.string_of_t v));
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
  in
    debug (fun () -> 
	     Printf.eprintf "member %s %s = %b\n%!"
	       (V.string_of_t v)
	       (Type.string_of_t t0)
	       result);
    result

(* type domains *)
type tdom_atom =
    DAny of Name.Set.t
  | DAll of Name.Set.t
  | DName of string
      
(* let cmp_tdom_atom a1 a2 = match a1,a2 with *)
(*     (\* DAny *\) *)
(*     DAny(s1), DAny(s2)   -> Name.Set.compare s1 s2 *)
(*   | DAny(_), DAll(_)     -> lt *)
(*   | DAny(_), DName(_)    -> lt *)
(*     (\* DAll *\) *)
(*   | DAll(_), DAny(_)     -> gt *)
(*   | DAll(s1), DAll(s2)   -> Name.Set.compare s1 s2  *)
(*   | DAll(_), DName(_)    -> lt *)
(*     (\* DName *\) *)
(*   | DName(_), DAny(_)    -> gt *)
(*   | DName(_), DAll(_)    -> gt *)
(*   | DName(n1), DName(n2) -> compare n1 n2 *)
	  
module TDom = 
  Set.Make(struct
	     type t = tdom_atom
	     let compare = compare (* cmp_tdom_atom *)
	   end)

module TDoms = 
  Set.Make(struct
 	     type t = TDom.t
	     let compare = compare
	   end)  
  
(* let atom2str ta = match ta with *)
(*     DAny s -> "!" *)
(*   | DAll s -> "*" *)
(*   | DName n -> n *)

(* let concat_tdom sep td = Misc.concat TDom.fold sep atom2str td *)
(* let tdom2str td = Misc.curlybraces (concat_tdom " " td) *)

(* let concat_tdoms sep tds = Misc.concat TDoms.fold sep tdom2str tds *)
(* let tdoms2str tds = Misc.curlybraces (concat_tdoms " " tds) *)

(* let nfcheck tbase f t =  *)
(*   match tbase with *)
(*       Cat(_) -> *)
(* 	(match it_of_pt t with *)
(* 	     Cat(_) | Union(_) -> raise (Error.Fatal_error("Type is not in normal form"))  *)
(* 	   | _ -> f t) *)
(*     | Union(_) ->  *)
(* 	(match it_of_pt t with  *)
(* 	     Union(_) -> raise (Error.Fatal_error("Type is not in normal form"))  *)
(* 	   | _ -> f t) *)
(*     | _ -> f t *)
	
let rec tdoms t = TDoms.empty
(*   match t with  *)
(*       TT pt -> tdoms_pt pt *)
(*     | _     ->  *)
(* 	fatal_error  *)
(* 	  (fun () ->  *)
(* 	     sprintf "cannot calculate type domain for negative type %s" *)
(* 	       (string_of_t t)) *)

(* and tdoms_pt (_,_,itr) = tdoms_it !itr *)
(* and tdoms_it it =  *)
(*   let nameset lst =  *)
(*     List.fold_left (fun ns n -> Name.Set.add n ns) Name.Set.empty lst *)
(*   in  *)
(*   let shallow_union f lst =  *)
(*     List.fold_left (fun acc td -> TDoms.union acc (f td)) TDoms.empty lst      *)
(*   in *)
(*   let deep_union f lst =  *)
(*     TDoms.singleton (TDoms.fold (fun acc d -> TDom.union acc d) (shallow_union f lst) TDom.empty) *)
(*   in *)
(*     match it with *)
(* 	Union(_,tl)  -> shallow_union (nfcheck it tdoms_pt) tl *)
(*       | Cat(_,tl)    -> deep_union (nfcheck it tdoms_pt) tl *)
(*       | Name(_,m,x)  -> TDoms.singleton (TDom.singleton (DName(m)))  *)
(*       | Bang(_,f,x)  -> TDoms.singleton (TDom.singleton (DAny(nameset f)))  *)
(*       | Star(_,f,x)  -> TDoms.singleton (TDom.singleton (DAll(nameset f)))  *)
(*       | Empty(_)     -> TDoms.empty *)
(*       | Any(_)       -> TDoms.singleton (TDom.singleton (DAll(Name.Set.empty))) *)
(*       | Var(_,_,thk) | App(_,_,_,thk) -> tdoms_pt ((\* FIXME: hack! *\) thk ()) *)
(*       | Fun(i,_) ->  *)
(* 	  fatal_error  *)
(* 	    (fun () ->  *)
(* 	       sprintf "cannot calculate type domain of type operator %s" *)
(* 		 (string_of_it it)) *)
(*       | Singleton(_,v) -> TDoms.singleton ( Name.Set.fold *)
(*                                              (fun k d -> TDom.add (DName k) d) *)
(*                                              (V.dom v) *)
(*                                              TDom.empty) *)
	  
let rec vdom_in_tdoms vd tds = true

(*   let name_match n ta = *)
(*     match ta with *)
(* 	DName m -> m=n *)
(*       | _ -> false *)
(*   in *)
(*   let any_match n ta =  *)
(*     match ta with *)
(* 	DAny s -> not (Name.Set.mem n s)  *)
(*       | _ -> false *)
(*   in *)
(*   let all_match n ta =  *)
(*     match ta with *)
(* 	DAll s -> not (Name.Set.mem n s)  *)
(*       | _ -> false *)
(*   in     *)
(*   let tdom_filter keep f n (vd,td) =  *)
(*     let (td1,td2) = TDom.partition (f n) td in *)
(*       if (TDom.cardinal td1)=1  *)
(*       then (Name.Set.remove n vd,  *)
(* 	    if keep then td else td2)  *)
(*       else (vd,td) *)
(*   in     *)
(*   let is_all ta = *)
(*     match ta with *)
(* 	DAll _ -> true *)
(*       | _ -> false *)
(*   in  *)
(*   let rec vdom_matches_tdom vd td = *)
(*     let (vd1,td1) = (Name.Set.fold (tdom_filter false name_match) vd (vd,td)) in *)
(*     let (vd2,td2) = (Name.Set.fold (tdom_filter false any_match) vd (vd1,td1)) in *)
(*     let (vd3,td3) = (Name.Set.fold (tdom_filter true all_match) vd (vd2,td2)) in *)
(*       (Name.Set.is_empty vd3) && ((TDom.is_empty td3) or (TDom.for_all is_all td3)) *)
(*   in *)
(*     TDoms.exists (vdom_matches_tdom vd) tds  *)
