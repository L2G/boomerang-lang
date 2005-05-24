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
(* low-level compiler debugging *)
let types_debug = Prefs.createBool "debug-types" false 
  "print debugging information about types"
  "print debugging information about types"

let debug s_thnk = 
  if Prefs.read types_debug 
  then 
    begin 
      prerr_string (sprintf "%s\n" (s_thnk ()));
      flush stderr
    end

let failAt i mesg_thk = 
  Printf.eprintf "Fatal error at %s: %s" (Info.string_of_t i) (mesg_thk ()); 
  exit 1

(* TYPES *)
type i = Info.t
type t = TT of pt
	 | NT of pt
and pt = (bool ref) * (bool V.Hash.t) * (it ref)
and it = 
    Empty of i
  | Any of i
  | Var of i * Syntax.qid * thunk
  | App of i * it * it * thunk
  | Fun of i * (it -> it)
  | Name of i * string * pt
  | Star of i * (string list) * pt
  | Bang of i * (string list) * pt
  | Cat of i * pt list 
  | Union of i * pt list 
and thunk = unit -> pt

let it_of_pt = function (_,_,itr) -> !itr

let rec info_of_t = function
    TT pt -> info_of_pt pt
  | NT pt -> info_of_pt pt
and info_of_pt = function (_,_,itr) -> info_of_it !itr
and info_of_it = function
    Empty(i)     -> i
  | Any(i)       -> i
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

and string_of_pt = function (_,_,itr) -> string_of_it !itr
and string_of_it = function
  | Empty(_) -> "empty"
  | Any(_) -> "Any"
  | Var (_,x,_) -> Syntax.string_of_qid x
  | App(_,it1,it2,_) -> sprintf "(%s %s)" (string_of_it it1) (string_of_it it2)
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
let eval_it it  = match it with 
    Var(_,_,thk) -> it_of_pt (thk ())
  | App(_,_,_,thk) -> it_of_pt (thk ())
  | _            -> it

(* CONSTRUCTOR *)
let mk_ptype it : pt = (ref false, V.Hash.create 1, ref it)
     
(* CONSTANTS *)
(* fix to use new representation *)
let nil_it = 
  let empty_view_pt = mk_ptype (Cat(Info.bogus, [])) in
    Name(Info.bogus, V.nil_tag, empty_view_pt)
      
let cons_it h t = 
  let i = Info.merge_inc (info_of_it h) (info_of_it t) in
  let hd_pt = mk_ptype (Name(i, V.hd_tag, mk_ptype h)) in
  let tl_pt = mk_ptype (Name(i, V.tl_tag, mk_ptype t)) in
    Cat(i, [hd_pt; tl_pt])

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
let rec eq_pt pt1 pt2 = eq_it (it_of_pt pt1) (it_of_pt pt2)
and eq_it it1 it2 = match it1,it2 with
    Empty(_),Empty(_) -> true
  | Any(_),Any(_) -> true
  | Var(_,x1,_),Var(_,x2,_) -> (* FIXME: is this safe? *) Syntax.qid_compare x1 x2 = 0
  | Name(_,n1,ptn1),Name(_,n2,ptn2) -> (n1=n2) && (eq_pt ptn1 ptn2)
  | Star(_,f1,ptx1),Star(_,f2,ptx2) -> (f1=f2) && (eq_pt ptx1 ptx2)
  | Bang(_,f1,ptx1),Bang(_,f2,ptx2) -> (f1=f2) && (eq_pt ptx1 ptx2)
  | Cat(_,cs1), Cat(_,cs2) ->
      if (Safelist.length cs1 <> Safelist.length cs2) then false
      else Safelist.fold_left 
	(fun ok (ci1,ci2) -> ok && (eq_pt ci1 ci2))
	true
	(Safelist.combine cs1 cs2)
  | Union(_,us1), Cat(_,us2) ->
      if (Safelist.length us1 <> Safelist.length us2) then false
      else Safelist.fold_left 
	(fun ok (ui1,ui2) -> ok && (eq_pt ui1 ui2))
	true
	(Safelist.combine us1 us2)
  | App(_,it11,it12,_),App(_,it21,it22,_) -> eq_it it11 it21 && eq_it it21 it22      
  | _ -> false 
    
let rec cmp_t t1 t2 = match t1,t2 with
    NT(_),TT(_)      -> lt
  | TT(_), NT(_)     -> gt	
  | NT(pt1),NT(pt2) -> cmp_pt pt1 pt2
  | TT(pt1),TT(pt2) -> cmp_pt pt1 pt2
and cmp_pt (_,_,itr1) (_,_,itr2) = cmp_it !itr1 !itr2
and cmp_it t1 t2 =
  match t1,t2 with      
      Empty(_),Empty(_)         -> eq
    | Empty(_),_                -> lt
    | _,Empty(_)                -> gt
    | Any(_),Any(_)             -> eq
    | Any(_),_                  -> lt
    | _,Any(_)                  -> gt
    | Fun(_),Fun(_)             -> eq
    | Fun(_),_                  -> lt
    | _,Fun(_)                  -> gt
    | Var(_,x,_),Var(_,y,_)     -> compare x y (* FIXME: broken? *)
    | Var(_),_                  -> lt
    | _,Var(_)                  -> gt
    | App(_,it11,it12,_),App(_,it21,it22,_) -> 
	let cmp1 = cmp_it it11 it21 in
	  if cmp1 <> eq then cmp1
	  else cmp_it it12 it22 
    | App(_),_                  -> lt
    | _,App(_)                  -> gt    
    | Name(_,n1,_),Name(_,n2,_) -> compare n1 n2
    | Name(_,_,_),_             -> lt
    | _,Name(_,_,_)             -> gt
    | Bang(_,f1,_),Bang(_,f2,_) -> 
	cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare
    | Bang(_,_,_),_             -> lt
    | _,Bang(_,_,_)             -> gt
    | Star(_,f1,_),Star(_,f2,_) -> 
	cmp_lex (Safelist.sort compare f1) (Safelist.sort compare f2) compare
    | Star(_,_,_),_             -> lt
    | _,Star(_,_,_)             -> gt
    | Cat(_,cs1),Cat(_,cs2)    -> 
	cmp_lex (Safelist.sort cmp_pt cs1) (Safelist.sort cmp_pt cs2) cmp_pt
    | Cat(_),_                 -> lt
    | _,Cat(_)                 -> gt
    | Union(_,us1),Union(_,us2) -> 
	cmp_lex (Safelist.sort cmp_pt us1) (Safelist.sort cmp_pt us2) cmp_pt
      
(* sort a type list *)
let sort_it it = it (* STUB *)
let sort_pt_list ptl = Safelist.sort cmp_pt ptl
	  
(*** UTILITIES ***)
(* pt2nf/t2nf: convert a (pre)type to normal form *)
let rec t2nf t0 = match t0 with
    NT pt -> NT (pt2nf true pt)
  | TT pt -> TT (pt2nf true pt)
	  
and pt2nf force ((normalized,memo,itr) as pt0) = 
  let _ = 
    match force,!itr with
	false, Var(_) -> ()
      | _ -> 
	  if not !normalized then
	    let it_nf = it2nf force !itr in
	      normalized := true;
	      itr := it_nf
  in
    pt0
      
and it2nf force it0 = 
  let res = match it0 with      
      Empty(_) | Any(_) | Fun(_) -> it0
    | Var(_,_,_)     -> if force then it2nf force (eval_it it0) else it0
    | App(_,_,_,_)   -> it2nf force (eval_it it0) 
    | Name(i,n,pt1)  -> Name(i, n, pt2nf false pt1)
    | Bang(i,es,pt1) -> Bang(i, Safelist.sort compare es, pt2nf false pt1)
    | Star(i,es,pt1) -> Star(i, Safelist.sort compare es, pt2nf false pt1)     	
  (* unions:
     - recursively normalize
     - lift nested unions
     - remove obviously Empty types from unions
  *)
  | Union(i,us)  ->
      let us_nf = Safelist.map (pt2nf force) us in
      let lifted_us = Safelist.fold_left
	(fun acc h_pt ->
	     match it_of_pt h_pt with
		 Empty(_)    -> acc    (* skip TEmpty bits of unions *)
	       | Union(_,us) -> us@acc (* lift nested unions *)
	       | _           -> h_pt::acc)
	[]
	us_nf
      in
	begin
	  match lifted_us with
	    | []           -> Empty(i)
	    | [h_pt] -> 
		begin 
		  match it_of_pt h_pt with
		      (Union(_,[])) -> Empty(i)
		    | _ -> sort_it (Union(i, lifted_us))
		end
	    | _ -> sort_it (Union(i,lifted_us))
	end
  (* TCat:
     - recursively normalize cs
     - lift nested cats in cs
     - distribute nested unions over cats
     - sort result
     - if a name is repeated, then TEmpty
  *)
  | Cat(i,cs)    ->
      let cs_nf = Safelist.map (pt2nf force) cs in		
      (* temporarily represent the lifted union of cats as a list of lists *)
      let lift_us_rep = Safelist.fold_left
	(fun acc h_pt ->
	   (* helper functions for readability *)
	   let prepend s a = Safelist.map (fun x -> sort_pt_list (s::x)) a in
	   let concat cs a = Safelist.map (fun x -> sort_pt_list (x @ cs)) a in	   
	     match it_of_pt h_pt with
	        Cat(_,cs)   -> concat cs acc (* lift nested cats *)
	       | Union(_,us) ->
		   (* distribute unions, lift deep-nested cats *)
		   Safelist.flatten
		     (Safelist.map
			(fun u_pt -> match it_of_pt u_pt with
			   | Cat(_,cs) -> concat cs acc			     
			   | _         -> prepend u_pt acc) us)
	       | _         -> prepend h_pt acc)
	[[]]
	cs_nf
      in
      let check_repeats it1 =
	let rec cr_aux cs acc =
	  match cs with
	    | []                                           -> Cat (i,Safelist.rev acc)
	    | [h]                                          -> cr_aux [] (h::acc)
	    | h1::h2::t -> 
		let it1 = it_of_pt h1 in
		let it2 = it_of_pt h2 in 
		  begin
		    match it1,it2 with 
			Name(_,n,_), Name(_,m,_) 
			  when (n = m) -> Empty(i)
		      | _ -> cr_aux (h2::t) (h1::acc)                                    
		  end
	in
	  match it1 with
	      Cat(_,cs) -> cr_aux cs []
	    | _ -> it1		
      in		  
	begin
	  match lift_us_rep with
	      [[h_pt]] -> 
		begin 
		  match it_of_pt h_pt with 
		      Empty(_) -> Empty(i)
		    | _ -> check_repeats (it_of_pt h_pt)
		end
	    | [cs] -> check_repeats (Cat(i,cs))
	    | _    -> Union (i, Safelist.map 
			       (fun x -> 
				  let it = check_repeats (Cat(i,x)) in
				    (* FIXME: hack! *) 
				    (ref true, V.Hash.create 1, ref it))
			       lift_us_rep)
	end
  in
    (* let _ = debug (sprintf "p2nf_aux %s %s ---> %s" (string_of_bool dethunk) (string_of_pt pt0) (string_of_pt res)) in *)
    res

let rec project t0 n = 
  debug (fun () -> sprintf "project type %s on %s" (string_of_t t0) n);
  match t2nf t0 with 
    TT pt -> begin 
      match (project_pt pt n) with
	  Some ptn -> Some (TT ptn)
	| None -> None
    end
  | _ -> failAt (info_of_t t0) (fun () -> "Fatal error: can't project a negative type")
and project_pt ((_,_,itr) as pt0) n = 
  debug (fun () -> sprintf "project pre type %s on %s" (string_of_pt pt0) n);
  project_it !itr n
and project_it it0 n =   
  debug (fun () -> sprintf "project inner type %s on %s" (string_of_it it0) n);
  match it0 with
      Empty(_)        -> None
    | Any(i)          -> Some ((* FIXME: hack!*) (ref true, V.Hash.create 1, ref (Any(i))))
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
			 if eq_pt ptin x then xo
			 else  failAt (info_of_it it0)
			   (fun () -> sprintf "Fatal error: %s is not projectable on %s; %s <> %s"
			      (string_of_it it0) n
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
			 if eq_pt ptin x then xo
			 else failAt (info_of_it it0)
			   (fun () -> sprintf "Fatal error: %s is not projectable on %s; %s <> %s"
			      (string_of_it it0) n
			      (string_of_pt pti)
			      (string_of_pt x))
		 end)
	    None
	    pts
      | Var(_) | App(_) -> project_it (eval_it it0) n
      | Fun(_) -> 
	  failAt (info_of_it it0) 
	    (fun () -> sprintf "Fatal error: cannot project type operator %s (on %s)"
	       (string_of_it it0) n)
	    
	    
let rec member v t0 = match t2nf t0 with 
    TT pt -> member_pt v pt
  | NT pt -> not (member_pt v pt)      
and member_pt v ((_,memo,itr) as pt0) = 
  begin 
    try V.Hash.find memo v
    with Not_found -> 
      let v_in_it = member_it v !itr in
	V.Hash.add memo v v_in_it;
	v_in_it
  end	
	
and member_it v it0 =   
  (* sanity check for projectability; 
     FIXME: should really just do this once during normalization *)
  let _ = V.fold (fun k _ () -> let _ = project_it it0 k in ()) v () in
  let res = match it0 with
      Empty(i)     -> false
    | Any(_)       -> true
    | Fun(i,_)     -> 
	failAt i
	  (fun () -> "Fatal error: cannot check for membership in type operator")
    | Var(_,_,thk)   -> member_pt v ((* FIXME: hack!*) thk ())
    | App(_,_,_,thk) -> member_pt v ((* FIXME: hack!*) thk ())
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
	let rec split v it =
	  match it with
	      Empty(_)   -> (None, v)
	    | Any(i)  -> (Some (v, ((* FIXME: hack!*) ref true, V.Hash.create 1, ref (Any(i)))), V.empty)
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
		failAt (info_of_it it)
		  (fun () -> sprintf "Fatal error: Non-atomic type in concatenation: %s"
		     (string_of_it it))
	in
	let rec loop cs v =
	  let lres = 
	    if (V.is_empty v) then
	      (* either cs = [] or all cs are Stars *)
	      List.fold_left (fun ok h -> match it_of_pt h with Star _ -> ok | _ -> false) true cs
	    else
	      match cs with
		| []   -> false
		| [ci] -> member_pt v ci
		| ci::rest ->
		    match split v (it_of_pt ci) with
		      | (None,_)             -> false
		      | (Some (vk,pt), vrest) -> (member_pt vk pt) && (loop rest vrest)
	  in
	    (* let _ = debug 
	       (sprintf "loop %s %s --> %s" 
	       (brackets (concat_list ", " (Safelist.map string_of_pt cs))) 
	       (V.string_of_t v) 
	       (string_of_bool lres)) 
	       in *)
	    lres
	in
	  loop cs v
  in
    (* let _ = debug 
       (sprintf "member_pt(%s, %s) --> %s" 
       (V.string_of_t v) 
       (string_of_pt pt0) 
       (string_of_bool res)) in *)
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
	(match it_of_pt t with
	     Cat(_) | Union(_) -> raise (Error.Run_error("Type is not in normal form")) 
	   | _ -> f t)
    | Union(_) -> 
	(match it_of_pt t with 
	     Union(_) -> raise (Error.Run_error("Type is not in normal form")) 
	   | _ -> f t)
    | _ -> f t
	
let rec tdoms t =
  match t with 
      TT pt -> tdoms_pt pt
    | _     -> 
	failAt 
	  (info_of_t t) 
	  (fun () -> "Fatal error: cannot calculate type domain for negated type")

and tdoms_pt (_,_,itr) = tdoms_it !itr
and tdoms_it it = 
  let nameset lst = 
    List.fold_left (fun ns n -> Name.Set.add n ns) Name.Set.empty lst
  in 
  let shallow_union f lst = 
    List.fold_left (fun acc td -> TDoms.union acc (f td)) TDoms.empty lst     
  in
  let deep_union f lst = 
    TDoms.singleton (TDoms.fold (fun acc d -> TDom.union acc d) (shallow_union f lst) TDom.empty)
  in
    match it with
	Union(_,tl)  -> shallow_union (nfcheck it tdoms_pt) tl
      | Cat(_,tl)    -> deep_union (nfcheck it tdoms_pt) tl
      | Name(_,m,x)  -> TDoms.singleton (TDom.singleton (DName(m))) 
      | Bang(_,f,x)  -> TDoms.singleton (TDom.singleton (DAny(nameset f))) 
      | Star(_,f,x)  -> TDoms.singleton (TDom.singleton (DAll(nameset f))) 
      | Empty(_)     -> TDoms.empty
      | Any(_)       -> TDoms.singleton (TDom.singleton (DAll(Name.Set.empty)))
      | Var(_,_,thk) | App(_,_,_,thk) -> tdoms_pt ((* FIXME: hack! *) thk ())
      | Fun(i,_) -> failAt i (fun () -> "Fatal error: cannot calculate type domain of a type function")
	  
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
