(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* type.ml - representation and functions on types       *)
(*                                                       *)
(* $Id: type.ml,v 1.1 2005/04/11 18:24:48 jnfoster Exp $ *)
(*                                                       *)
(*********************************************************)

open Pretty
    
(* TYPES *)
type t = TT of pt | NT of pt 
and pt = 
    Empty 
  | Var of Syntax.qid * Syntax.qid list (* FIXME: include a naming context here too ? *) 
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
  | NT pt -> concat "" ["~"; "(" ; string_of_pt pt ; ")"]

and string_of_pt = function
  | Empty -> "empty"
  | Var (x,_) -> Syntax.string_of_qid x
  | App(pt1,pt2) -> concat "" [string_of_pt pt1; " "; string_of_pt pt2]
  | Fun f -> "<type fun>"
  | Name(n,pt) -> concat "" [n; curlybraces (string_of_pt pt)]
  | Bang(f,pt)  ->
      concat ""
    	[ "!"
    	; braces (concat " " f)
    	; curlybraces (string_of_pt pt)]
  | Star(f,pt)  ->
	concat ""
    	  [ "*"
    	  ; braces (concat " " f)
    	  ; curlybraces (string_of_pt pt)]
  | Cat(cs)   -> concat "." (List.map string_of_pt cs)
  | Union(ts)  -> braces (concat " | " (List.map string_of_pt ts))

let member t v = assert false
let project t k = assert false

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
  
let rec cmp_ptyp eval t1 t2 =
  match t1,t2 with      
      (* Fun *)
      Fun(_),Fun(_)         -> eq
    | Fun(_),_              -> lt
    | _,Fun(_)              -> gt
	
    (* | Var(x,xctx),Var(y,yctx) -> SHORT CIRCUIT SAME VARIABLE, SAME CONTEXT? *)
    | Var(x,_),_              -> cmp_ptyp eval (eval t1) t2
    | _,Var(x,_)              -> cmp_ptyp eval t1 (eval t2)
	
    (* App *)
    | App(pt1,pt2),_         -> cmp_ptyp eval (eval pt1) t2
    | _,App(pt1,pt2)         -> cmp_ptyp eval t1 (eval pt2)
	
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
    | Cat(cs1),Cat(cs2)    -> cmp_lex (Safelist.sort (cmp_ptyp eval) cs1) (Safelist.sort (cmp_ptyp eval) cs2) (cmp_ptyp eval)
    | Cat(_),_             -> lt
    | _,Cat(_)             -> gt
	
    (* Union *)           
    | Union(us1),Union(us2) -> cmp_lex (Safelist.sort (cmp_ptyp eval) us1) (Safelist.sort (cmp_ptyp eval) us2) (cmp_ptyp eval)

let cmp_typ eval t1 t2 = match t1,t2 with
    NT(_),TT(_)      -> lt
  | TT(_), NT(_)     -> gt	
  | NT(pt1),NT(pt2)  -> cmp_ptyp eval pt1 pt2
  | TT(pt1), TT(pt2) -> cmp_ptyp eval pt1 pt2	  
      
(* sort a type list *)
let sort_ptyp eval pt = pt (* STUB *)
let sort_ptyp_list eval tl = Safelist.sort (cmp_ptyp eval) tl
	  
(*** UTILITIES ***)

(* pt2nf/t2nf: convert a (pre)type to normal form *)
let rec pt2nf eval pt = match pt with      
  | Empty      -> pt
  | Fun(_)     -> pt
  | Var(_,_)   -> pt2nf eval (eval pt)

  | App(_,_)   -> pt2nf eval (eval pt)
  | Name(n,pt1) -> Name(n, pt2nf eval pt1)
  | Bang(es,pt1)-> Bang(Safelist.sort compare es, pt2nf eval pt1)
  | Star(es,pt1)-> Star(Safelist.sort compare es, pt2nf eval pt1)
      
  (* unions:
     - recursively normalize
     - lift nested unions
     - remove obviously Empty types from unions
  *)
  | Union(us)  ->
      let us_nf = Safelist.map (pt2nf eval) us in
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
	    | _            -> sort_ptyp eval (Union(lifted_us))
	end
	  
  (* TCat:
     - recursively normalize cs
     - lift nested cats in cs
     - distribute nested unions over cats
     - sort result
     - if a name is repeated, then TEmpty
  *)
  | Cat(cs)    ->
      let cs_nf = Safelist.map (pt2nf eval) cs in	
	
      (* temporarily represent the lifted union of cats as a list of lists *)
      let lift_us_rep = Safelist.fold_left
	(fun acc h ->
	   (* helper functions for readability *)
	   let prepend s a = List.map (fun x -> sort_ptyp_list eval (s::x)) a in
	   let concat cs a = List.map (fun x -> sort_ptyp_list eval (x @ cs)) a in
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


let rec t2nf eval t = match t with
      NT pt -> NT (pt2nf eval pt)
    | TT pt -> TT (pt2nf eval pt)
	    
(* let normalize_ctx delta =    *)
(*   let delta1,newbinds =  *)
(*     (List.fold_left  *)
(*        (fun (deltaacc,newbinds) (x,i,t) -> *)
(* 	  let (t1,tbinds) = t2nf t (delta @ newbinds) in *)
(* 	    ((x,i,t1)::deltaacc, (tbinds @ newbinds))) *)
(*        ([],[]) *)
(*        delta) *)
(*   in *)
(*     (List.rev delta1) @ (List.rev newbinds) *)
    
(* (\* more functions *\) *)
(* let rec project t n =  *)
(*   debug ("--- PROJECT --- " ^ n ^ " from t=" ^ (t2str t)); *)
(*   let res =  *)
(*     match t with *)
(*       | TEmpty(_)     -> None *)
(*       | TName(_,m,x)  -> if (n=m) then Some x else None *)
(*       | TBang(_,f,x)   -> if (List.mem n f) then None else Some x *)
(*       | TStar(_,f,x)   ->  *)
(* 	  if (List.mem n f) then None  *)
(* 	  else Some x *)
(*       | TUnion(_,ts) ->  *)
(* 	  List.fold_left  *)
(* 	    (fun xo ti -> match xo with *)
(* 		 None -> project ti n *)
(* 	       | Some x ->  *)
(* 		   let to2str t =  *)
(* 		     match t with *)
(* 			 None -> "NONE" *)
(* 		       | Some t -> cs2str t *)
(* 		   in *)
(* 		   let tin = project ti n in *)
(* 		     (match tin with *)
(* 			  None -> xo *)
(* 			| Some _ ->  *)
(* 			    if tin = xo then xo *)
(* 			    else *)
(* 			      (Format.printf  *)
(* 				 "ERROR: type is not projectable because %s <> %s \n" *)
(* 				 (to2str tin) *)
(* 				 (to2str xo);			        *)
(* 			       assert false))) *)
(* 	    None  *)
(* 	    ts *)
(*       | TCat(_,cs) ->  *)
(* 	  List.fold_left  *)
(* 	    (fun xo ti -> match xo with *)
(* 		 None -> project ti n *)
(* 	       | Some _ -> xo) *)
(* 	    None  *)
(* 	    cs *)
(*   in *)
(*   let _ = debug ("res = " ^ (match res with *)
(* 				 None -> "NONE" *)
(* 			       | Some x -> cs2str x)) in *)
(*     res *)

(* (\* the unfold operator *\) *)
(* let rec unfold cx delta = match cx with *)
(*     (x,[],[]) -> Syntax.lookup x delta *)
(*   | _         -> debug "ERROR: unfold not implemented for complex type states"; assert false *)
(* (\* *)
(*   | (x,y::t,ds) ->  *)
(*       let dy = lookup y delta in *)
(*       let ux = unfold (x,t,ds) delta in *)
(* 	diff (TUnion ([ux; dy],bogusI)) (TUnion ([diff ux dy; diff dy ux],bogusI)) *)
(*   | (x,[],y::t) -> diff (unfold (x,[],t) delta) (lookup y delta) *)
(* *\)  *)
     
(* (\* check if v in t *\) *)
(* let rec member v t delta =  *)
(*   let _ = debug ("\n--MEMBER--\nt=" *)
(* 		 ^ (t2str t) *)
(* 	         ^ "\nv="); debug_view v in *)
(*   let _ = debug ("\nDELTA =\n" ^ (defs2str delta)) in *)
(*     match t with		 *)
(*       | TEmpty(_)    ->  *)
(* 	  debug "\nin TEmpty\nANSWER to TBang = false"; *)
(* 	  false *)
(*       | TName(_,n,x) -> *)
(* 	  debug ("\nin TName for " ^ n); *)
(* 	  let d = V.dom v in *)
(* 	  let c = Name.Set.cardinal d in *)
(* 	  let res = (c = 1) &&  *)
(* 		    (match V.get v n with *)
(* 		       | None -> false *)
(* 		       | Some vn -> member vn (unfold x delta) delta) *)
(* 	  in *)
(* 	  let _ = debug ("ANSWER to TName: " ^ (t2str t) ^ " = " ^ (string_of_bool res)) in *)
(* 	    res *)
(*       | TBang(_,f,x) -> *)
(* 	  let _ = debug "\nin TBang" in	   *)
(* 	  let d = V.dom v in *)
(* 	  let res =  *)
(* 	    match Name.Set.cardinal d with *)
(* 	      | 1 ->  *)
(* 		  let k = Name.Set.choose d in		    *)
(* 		    (not (List.mem k f))  *)
(* 		    && (member  *)
(* 			  (V.get_required v k)  *)
(* 			  (unfold x delta)  *)
(* 			  delta) *)
(* 	      | _ -> false *)
(* 	  in *)
(* 	  let _ = debug ("ANSWER to TBang: " ^ (t2str t) ^ " = " ^ (string_of_bool res)) in *)
(* 	    res *)
(*       | TStar(_,f,x) ->  *)
(* 	  debug "\nin TStar"; *)
(* 	  let ux = unfold x delta in *)
(* 	  let res = Name.Set.fold  *)
(* 	      (fun k okSoFar -> *)
(* 		 okSoFar && *)
(* 		 (not (List.mem k f)) && *)
(* 		 (member (V.get_required v k) ux delta))	   *)
(* 	      (V.dom v) *)
(* 	      true	   *)
(* 	  in  *)
(* 	  let _ = debug ("ANSWER to TStar : " ^ (t2str t) ^ " = " ^ (string_of_bool res)) in *)
(* 	    res *)
(*       | TUnion (_,us) ->  *)
(* 	  let _ = debug "\nin TUnion" in *)
(* 	  let res =  *)
(* 	    List.fold_left *)
(* 	      (fun acc ui -> *)
(* 		 if acc then true  *)
(* 		 else member v ui delta) *)
(* 	      false *)
(* 	      us *)
(* 	  in *)
(* 	  let _ = debug ("ANSWER to TUnion: "  ^ (t2str t) ^ " = " ^ (string_of_bool res)) in *)
(* 	    res *)
(*       | TCat (_,cs) ->  *)
(* 	  let _ = debug "\nin TCat" in *)
(* 	  (\* this split does not work in general...  *)
(* 	     just for testing some easy cases *\) *)
(* 	  let split v t  =  *)
(* 	    match t with *)
(* 	      | TEmpty(_)     -> (None, v) *)
(* 	      | TName(_,n,x) ->  *)
(* 		  (match V.get v n with *)
(* 		     | None       -> (None, v) *)
(* 		     | Some vn    -> (Some (vn,x), V.set v n None)) *)
(* 	      | TBang(_,f,x)  ->  *)
(* 		  let n = Name.Set.choose (V.dom v) in *)
(* 		  let vn = V.get_required v n in *)
(* 		    if (List.mem n f) then (None, v) *)
(* 		    else (Some (vn,x), V.set v n None) *)
(* 	      | TStar(_,f,x)  -> (Some (v,x), V.empty) *)
(* 	      | _            ->  *)
(* 		  debug ("ERROR: the impossible happened. Non-atomic type in TCat; t=" ^ (t2str t)); *)
(* 		  assert false (\* can't happen *\) *)
(* 	  in *)
(* 	  let rec loop cs v =  *)
(* 	    (\** let _ = debug ("\n--LOOP--\n t=" *)
(* 	      ^ (tl2str cs) *)
(* 	      ^ " v=") in *)
(* 	      let _ = V.format v in	       *)
(* 	    **\) *)
(* 	    if (V.is_empty v) then  *)
(* 	      (\* all the cs must be TStars *\) *)
(* 	      List.fold_left (fun ok h -> match h with TStar _ -> ok | _ -> false) true cs *)
(* 	    else *)
(* 	      match cs with *)
(* 		| []   -> false *)
(* 		| [ci] -> member v ci delta *)
(* 		| ci::rest ->  *)
(* 		    match split v ci with *)
(* 		      | (None,_)             -> false *)
(* 		      | (Some (vk,x), vrest) -> *)
(* 			  let tx = unfold x delta in *)
(* 			    (member vk tx delta ) && (loop rest vrest) *)
(* 	  in *)
(* 	  let res = loop cs v *)
(* 	  in *)
(* 	  let _ = debug ("ANSWER to TCat: "  ^ (t2str t) ^ " = " ^ (string_of_bool res)) in *)
(* 	    res *)

(* (\* (\\* Nice interface to the parser/compiler for types *\\) *\) *)
(* (\* let string2abstract_type deltastr =  *\) *)
(* (\*   try  *\) *)
(* (\*     let lexbuf = Lexing.from_string deltastr in *\) *)
(* (\*     let pdefs = Parser.pdefs Lexer.type_token lexbuf in *\) *)
(* (\*     let delta1 = pctx2ctx pdefs in *\) *)
(* (\*     let delta = normalize_ctx delta1 in *\) *)
(* (\*     let ty =  *\) *)
(* (\*       match pdefs with  *\) *)
(* (\* 	  [] -> Syntax.TEmpty Error.bogusInfo *\) *)
(* (\* 	| ((x,_,_)::_) -> unfold (x,[],[]) delta  *\) *)
(* (\*     in *\) *)
(* (\*       (delta,ty)	 *\) *)
(* (\*   with *\) *)
(* (\*       e ->  *\) *)
(* (\* 	Format.printf "ERROR in string2abstract_type for:\n%s\n" deltastr;  *\) *)
(* (\* 	raise e *\) *)

(* (\************* DEPRECATED / UNFINISHED STUFF **************************\) *)
(* (\* let setUnion x f =  if (List.mem x f) then f else x::f *\) *)
(* (\* *\) *)
(* (\* (\\* symbolic diff on two cstates *\\) *\) *)
(* (\* let rec csdiff cx cy = *\) *)
(* (\*   let (x,ix,dx) = cx in *\) *)
(* (\*     match cy with *\) *)
(* (\*       | (z,[],[])    ->  *\) *)
(* (\* 	  if (z = x || List.mem z ix) then [] *\) *)
(* (\* 	  else [(x,ix,setUnion z dx)] *\) *)
(* (\*       | (z,i::is,ds) ->  *\) *)
(* (\* 	  let rest = csdiff cx (z,is,ds) in *\) *)
(* (\* 	    if (z = x || List.mem z ix) then rest *\) *)
(* (\* 	    else (x,ix,setUnion z dx)::rest *\) *)
(* (\*       | (z,[],d::ds) ->  *\) *)
(* (\* 	  let rest = csdiff cx (z,[],ds) in *\) *)
(* (\* 	    if (List.mem d dx) then rest  *\) *)
(* (\* 	    else if (x = d) then cx::rest  *\) *)
(* (\* 	    else (x,setUnion d ix,dx)::rest *\) *)
(* (\* *\) *)
(* (\* (\\* the diff operator *\\) *\) *)
(* (\* let rec diff t1 t2 =  *\) *)
(* (\*   (\\** let _ = debug ("diffing: " ^ t2str t1 ^ ", " ^ t2str t2) in **\\) *\) *)
  
(* (\*   (\\* compute normal forms *\\) *\) *)
(* (\*   let nt1,nt2 = t2nf t1, t2nf t2 in     *\) *)
    
(* (\*   (\\* lift a list of cstates to a list of things generated by f *\\) *\) *)
(* (\*   let liftCSUnions cus f =  *\) *)
(* (\*     match cus with *\) *)
(* (\* 	[]  -> [TEmpty(bogusI)] *\) *)
(* (\*       | [z] -> [f z] *\) *)
(* (\*       | _   -> (List.map f cus) *\) *)
(* (\*   in *\) *)
(* (\*   let mkNs f1 f2 x info = *\) *)
(* (\*     List.map  *\) *)
(* (\*       (fun n -> TName(n,x,info)) *\) *)
(* (\*       (List.filter (fun n -> not (List.mem n f1)) f2) *\) *)
(* (\*   in *\) *)

(* (\*   (\\* helper: used whenever we have to split a TCat *\\) *\) *)
(* (\*   (\\* NB: cs1 and cs2 are assumed to be in nf--i.e., consist of TNames, *\) *)
(* (\*      TBang, and at most one TStar in that order! *\\) *\) *)
(* (\*   let split_case cs1 cs2 info =  *\) *)
(* (\*     let divide cs =  *\) *)
(* (\*       List.fold_left *\) *)
(* (\* 	(fun (names,anys,alls) h -> *\) *)
(* (\* 	   match h with *\) *)
(* (\* 	       TName(_,_,_) -> (h::names,anys,alls) *\) *)
(* (\* 	     | TBang(_,_,_)  -> (names,h::anys,alls) *\) *)
(* (\* 	     | TStar(_,_,_)  -> (names,anys,h::alls) *\) *)
(* (\* 	     | _          ->  *\) *)
(* (\* 		 (\\* can't happen *\\) *\) *)
(* (\* 		 raise (Error.Type_error("Type not in normal form: " ^ (t2str h), *\) *)
(* (\* 					t2info h)) *\) *)
(* (\* 	) ([],[],[]) cs *\) *)
(* (\*     in *\) *)
(* (\*     let (ns1,ys1,ls1),(ns2,ys2,ls2) = divide cs1, divide cs2 in *\) *)
      
(* (\*     (\\* the split operator - iterates all the perfect matchings *\\) *\) *)
(* (\*     let rec split cs1 cs2 cover acc =  *\) *)
(* (\*       (\\**  *\) *)
(* (\* 	let _ = debug ("--SPLIT--\n cs1=" *\) *)
(* (\* 	^ (tl2str cs1) *\) *)
(* (\* 	^ " cs2=" *\) *)
(* (\* 	^ (tl2str cs2) *\) *)
(* (\* 	^ " cover=" *\) *)
(* (\* 	^ (if cover = [] then "[]"  *\) *)
(* (\* 	else (List.fold_right  *\) *)
(* (\* 	(fun (n,x) ta -> "(" ^ n ^ ", " ^ (cs2str x) ^ ") " ^ ta) *\) *)
(* (\* 	cover "")) *\) *)
(* (\* 	^ " acc=" *\) *)
(* (\* 	^ tpl2str acc) in  *\) *)
(* (\*       **\\) *\) *)
(* (\*       match cs1,cs2 with *\) *)
(* (\*       | (TName(n,x,i1)::tl1, TName(m,y,i2)::tl2) ->  *\) *)
(* (\* 	  if m = n then split tl1 tl2 cover ((TName(n,x,i1),TName(m,y,i2))::acc) *\) *)
(* (\* 	  else split tl1 tl2 ((n,x)::cover) acc *\) *)
(* (\*       | (TName(n,x,_)::tl1, TBang(f,y,_)::tl2) -> split tl1 cs2 ((n,x)::cover) acc *\) *)
(* (\*       | (TName(n,x,_)::tl1, TStar(f,y,_)::tl2) -> split tl1 cs2 ((n,x)::cover) acc *\) *)
(* (\*       | (TName(n,x,i)::tl1, [])             -> split tl1 [] ((n,x)::cover) acc *\) *)
(* (\*       | [],[]                             -> *\) *)
(* (\* 	  if (cover = []) then acc  *\) *)
(* (\* 	  else  *\) *)
(* (\* 	    (List.map (fun (n,x) -> (TName(n,x,bogusI),TEmpty(bogusI))) cover)  *\) *)
(* (\* 	    @ (List.map (fun (t,_) -> (t,TEmpty(bogusI))) acc) *\) *)
(* (\*       | _ ->  *\) *)
(* (\* 	  raise (Error.Type_error("unimplemented case in split", bogusI)) *\) *)
(* (\*    in  *\) *)
      
(* (\*     let splits = split cs1 cs2 [] [] in       *\) *)
(* (\*     let diff_pairs = List.map (fun (t1,t2) -> (t1,diff t1 t2)) splits in *\) *)

(* (\*     (\\** *\) *)
(* (\*       let _ = debug ("splits=" ^ (tpl2str splits)) in       *\) *)
(* (\*       let _ = debug ("diff_pairs=" ^ (tpl2str diff_pairs)) in *\) *)
(* (\*     **\\) *\) *)
(* (\*     (\\* ugly hack alert! - we should fold this fold in somewhere else :) *\\)  *\) *)
(* (\*     let all_diffs_empty =  *\) *)
(* (\*       List.fold_left (fun a (_,d) ->  *\) *)
(* (\* 		     match d with  *\) *)
(* (\* 		       | TEmpty _ -> a *\) *)
(* (\* 		       | _        -> false) *\) *)
(* (\* 	true splits *\) *)
(* (\*     in *\) *)

(* (\*     (\\* helper: append t to all the lists in acc IF acc non-empty *\\) *\) *)
(* (\*     let rec app t acc =  *\) *)
(* (\*       match acc with *\) *)
(* (\* 	| []      -> [] *\) *)
(* (\* 	| h::rest -> (h@[t])::(app t rest) *\) *)
(* (\*     in       *\) *)
      
(* (\*     (\\* hideous code. JNF takes blame :) *\\) *\) *)
(* (\*     let diag_diff_matrix =  *\) *)
(* (\*       snd  *\) *)
(* (\* 	(List.fold_left  *\) *)
(* (\* 	   (fun (ts,acc) (t,d) -> *\) *)
(* (\* 	      let newacc = app t acc in *\) *)
(* (\* 		(ts@[t]), *\) *)
(* (\* 		match d with  *\) *)
(* (\* 		    TEmpty _ -> newacc  *\) *)
(* (\* 		  | _ -> (ts@[d])::newacc) *\) *)
(* (\* 	   ([],[]) *\) *)
(* (\* 	   diff_pairs) *\) *)
(* (\*     in *\) *)
      
(* (\*       (\\* need to factor out code from t2nf and here *\\) *\) *)
(* (\*       if (all_diffs_empty) then nt1 *\) *)
(* (\*       else 	 *\) *)
(* (\* 	let cs =  *\) *)
(* (\* 	  List.map (fun x -> TCat(x,info)) diag_diff_matrix in *\) *)
(* (\* 	  t2nf (TUnion(cs,info)) *\) *)
(* (\*   in *\) *)
(* (\*     match nt1,nt2 with *\) *)

(* (\*       (\\* the "easy" cases *\\) *\) *)
(* (\*       | TEmpty(_),_               -> nt1 *\) *)
(* (\*       | _,TEmpty(_)               -> nt1 *\) *)
(* (\*       | TUnion(us,i),_         -> t2nf (TUnion ((List.map (fun ui -> diff ui t2) us),i)) *\) *)
(* (\*       | _,TUnion(us,_)         -> t2nf (List.fold_left (fun t ui -> diff t ui) t1 us) *\) *)
(* (\*       | TCat(_,_),TName(_,_,_) -> nt1 *\) *)
(* (\*       | TCat(_,_),TBang(_,_,_)  -> nt1 *\) *)

(* (\*       | TCat(cs1,i),TStar(f,y,_) -> split_case cs1 [nt2] i *\) *)
(* (\*       | TCat(cs1,i),TCat(cs2,_) -> split_case cs1 cs2 i *\) *)
	    
(* (\*       (\\* base cases *\\) *\) *)
(* (\*       | TName(n,x,i),TName(m,y,_) ->  *\) *)
(* (\* 	  if (n <> m) then nt1 *\) *)
(* (\* 	  else t2nf (TUnion ((liftCSUnions (csdiff x y) (fun z -> TName(n,z,i))),i)) *\) *)

(* (\*       | TName(n,x,i),TBang(f,y,_)  -> *\) *)
(* (\* 	  if (List.mem n f) then nt1 *\) *)
(* (\* 	  else t2nf(TUnion ((liftCSUnions (csdiff x y) (fun z -> TName(n,z,i))),i)) *\) *)

(* (\*       | TName(n,x,i),TStar(f,y,_)  -> *\) *)
(* (\* 	  if (List.mem n f) then nt1 *\) *)
(* (\* 	  else t2nf (TUnion ((liftCSUnions (csdiff x y) (fun z -> TName(n,z,i))),i)) *\) *)

(* (\*       | TName(n,x,_),TCat(ts,_) -> nt1 *\) *)
	  
(* (\*       | TBang(f,x,i),TName(m,y,_)  ->  *\) *)
(* (\* 	  if (List.mem m f) then nt1 *\) *)
(* (\* 	  else  *\) *)
(* (\* 	    t2nf(TUnion ((TBang(setUnion m f,x,i):: *\) *)
(* (\* 			    (liftCSUnions  *\) *)
(* (\* 			       (csdiff x y)  *\) *)
(* (\* 			       (fun z -> TName(m,z,i)))),i)) *\) *)

(* (\*       | TBang(f,x,i),TBang(g,y,_)   -> *\) *)
(* (\* 	  t2nf (TUnion (( *\) *)
(* (\* 		  (mkNs f g x i) @ *\) *)
(* (\* 			  (liftCSUnions  *\) *)
(* (\* 			     (csdiff x y)  *\) *)
(* (\* 			     (fun z -> TBang(f@g,z,i))) *\) *)
(* (\* 			),i)) *\) *)
	    
(* (\*       | TBang(f,x,i),TStar(g,y,_)   -> *\) *)
(* (\* 	  t2nf(TUnion (( *\) *)
(* (\* 		 (mkNs f g x i) @ *\) *)
(* (\* 		 (liftCSUnions  *\) *)
(* (\* 		    (csdiff x y)  *\) *)
(* (\* 		    (fun z -> TBang(f@g,z,i))) *\) *)
(* (\* 	    ),i)) *\) *)

(* (\*       | TBang(f,x,_),TCat(ts,_)     -> nt1 *\) *)

(* (\*       | TStar(f,x,i),TName(m,y,_)  -> *\) *)
(* (\* 	  if (List.mem m f) then nt1 *\) *)
(* (\* 	  else *\) *)
(* (\* 	    t2nf ( *\) *)
(* (\* 	      TUnion (( *\) *)
(* (\* 		(liftCSUnions (csdiff x y) (fun z -> (TName(m,z,i)))) *\) *)
(* (\* 		@[(\\* emptyViewType; *\\)TBang(setUnion m f,x,i); TCat ([TBang(f,x,i); TBang(f,x,i); TStar(f,x,i)],i)] *\) *)
(* (\* 	      ),i)) *\) *)

(* (\*       | TStar(f,x,i),TBang(g,y,_)   -> *\) *)
(* (\* 	  t2nf ( *\) *)
(* (\* 	    TUnion(( *\) *)
(* (\* 	      (liftCSUnions (csdiff x y) (fun z -> (TBang(f,z,i)))) *\) *)
(* (\* 	      @ (mkNs f g x i) *\) *)
(* (\* 	      @ [(\\* emptyViewType; *\\) TCat([TBang(f,x,i); TStar(f,x,i)],i)]	  	   *\) *)
(* (\* 	    ),i)) *\) *)

(* (\*       | TStar(f,x,i),TStar(g,y,_)   -> *\) *)
(* (\* 	  t2nf ( *\) *)
(* (\* 	    TCat((TBang(f,x,i)::[TUnion(( *\) *)
(* (\* 			       (liftCSUnions (csdiff x y) (fun z -> (TBang(f,z,i)))) *\) *)
(* (\* 			       @ (mkNs f g x i)),i)]),i) *\) *)
(* (\* 	  ) *\) *)
(* (\*       | TStar(f,x,i),TCat(cs2,_) -> split_case [nt1] cs2 i *\) *)



(* (\* insert CK's algorithm here *\) *)
(* (\* let is_empty g x = false *\) *)


(* (\* --- CK added from here --- *\) *)

(* (\* *)
(* type tdom_atom = *)
(*     DAny of Name.Set.t *)
(*   | DAll of Name.Set.t *)
(*   | DName of name *)
      
(* let cmp_tdom_atom a1 a2 = *)
(*   let lt = -1 in *)
(*   let gt = 1 in *)
(*     match a1,a2 with *)
(* 	(\* DAny *\) *)
(* 	DAny(s1), DAny(s2)   -> Name.Set.compare s1 s2 *)
(*       | DAny(_), DAll(_)     -> lt *)
(*       | DAny(_), DName(_)    -> lt *)
(*         (\* DAll *\) *)
(*       | DAll(_), DAny(_)     -> gt *)
(*       | DAll(s1), DAll(s2)   -> Name.Set.compare s1 s2  *)
(*       | DAll(_), DName(_)    -> lt *)
(*         (\* DName *\) *)
(*       | DName(_), DAny(_)    -> gt *)
(*       | DName(_), DAll(_)    -> gt *)
(*       | DName(n1), DName(n2) -> compare n1 n2 *)
	  
(* module TDomAtomSet = Set.Make(struct *)
(* 				type t = tdom_atom *)
(* 				let compare = cmp_tdom_atom *)
(* 			      end) *)
  
(* module TDom = Set.Make(struct *)
(* 			 type t = TDomAtomSet.t *)
(* 			 let compare = compare *)
(* 		       end)   *)

(* let nfcheck tbase f t =  *)
(*   match tbase with *)
(*       TCat(_,i) -> *)
(* 	(match t with *)
(* 	     TCat(_,_) | TUnion(_,_) -> raise (Error.Type_error("Type not normal formed",i))  *)
(* 	   | _ -> f t) *)
(*     | TUnion(_,i) ->  *)
(* 	(match t with  *)
(* 	     TUnion(_,_) -> raise (Error.Type_error("Type not normal formed",i))  *)
(* 	   | _ -> f t) *)
(*     | _ -> f t *)
    
  		    
(* let rec tdom t = *)
(*   let nameset lst =  *)
(*     List.fold_left (fun ns n -> Name.Set.add n ns) Name.Set.empty lst *)
(*   in  *)
(*   let shallow_union f lst =  *)
(*     List.fold_left (fun acc td -> TDom.union acc (f td)) TDom.empty lst      *)
(*   in *)
(*   let deep_union f lst =  *)
(*     TDom.singleton (TDom.fold (fun acc d -> TDomAtomSet.union acc d) (shallow_union f lst) TDomAtomSet.empty) *)
(*   in *)
(*     match t with *)
(* 	TUnion(tl,_)  -> shallow_union (nfcheck t tdom) tl *)
(*       | TCat(tl,_)    -> deep_union (nfcheck t tdom) tl *)
(*       | TName(m,x,_)  -> TDom.singleton (TDomAtomSet.singleton (DName(m)))  *)
(*       | TBang(f,x,_)   -> TDom.singleton (TDomAtomSet.singleton (DAny(nameset f)))  *)
(*       | TStar(f,x,_)   -> TDom.singleton (TDomAtomSet.singleton (DAll(nameset f)))  *)
(*       | TEmpty(_)     -> TDom.empty *)
	  
(* (\* It is assumed that precisely one tdom_atom in a tdas matches a *)
(*    given name - is this always true for normalized types? Especially, *)
(*    how do we restrict ![f2] and *[f1] from overlapping? If the *)
(*    assumption is false then these functions must be generalized to *)
(*    somehow find the best global match - this should be possible using *)
(*    the "max matching in bipartite graph" algorithm *\) *)

(* let tdas_match tdas n =  *)
(*   TDomAtomSet.fold  *)
(*     (fun tda ao -> *)
(*        match tda with *)
(* 	   DAny s -> if Name.Set.mem n s then ao else Some tda  *)
(* 	 | DAll s -> if Name.Set.mem n s then ao else Some tda  *)
(* 	 | DName m -> if n=m then Some tda else ao)  *)
(*     tdas  *)
(*     None *)
    
(* let tdas_diff tdas ns =  *)
(*   (Name.Set.fold  *)
(*      (fun n (tdas',ns') ->  *)
(* 	match (tdas_match tdas' n) with *)
(* 	    None -> (tdas',ns') *)
(* 	  | Some a -> (TDomAtomSet.remove a tdas',Name.Set.remove n ns'))  *)
(*      ns  *)
(*      (tdas,ns)) *)
    
(* let tdas_inter tdas ns = *)
(*   (Name.Set.fold  *)
(*      (fun n (tdas',ns') ->  *)
(* 	match (tdas_match tdas' n) with *)
(* 	    None -> (tdas',ns') *)
(* 	  | Some tda -> (TDomAtomSet.add tda tdas',Name.Set.add n ns'))  *)
(*      ns  *)
(*      (TDomAtomSet.empty,Name.Set.empty)) *)
    
(* let tdas_matches_empty tdas =  *)
(*   TDomAtomSet.fold (fun tda b ->  *)
(* 		      match tda with *)
(* 			  DAny _   -> b  *)
(* 			| DAll _   -> false *)
(* 			| DName _  -> false *)
(* 		   ) tdas true *)
    
(* let tdas_matches tdas ns =  *)
(*   let (tdas',rest) = tdas_diff tdas ns in *)
(*     tdas_matches_empty tdas' && Name.Set.is_empty rest  *)
      
(* let maxdom t o a b =  *)
(*   let dom_o = V.dom o in *)
(*   let dom_a = V.dom a in *)
(*   let dom_b = V.dom b in *)
(*   let (unchanged,changed) = Name.Set.partition (fun n -> V.equal_opt (V.get o n) (V.get a n)) dom_a in  *)
(*   let remove = Name.Set.diff (Name.Set.inter dom_a unchanged) dom_b in *)
(*   let add = Name.Set.diff dom_b (Name.Set.union dom_o dom_a) in *)
(*   let diffs = Name.Set.union remove add in  *)
(*   let cmp tdas md =   *)
(*     let tdas' = fst (tdas_diff tdas changed) in *)
(*     let d = Name.Set.union changed (snd (tdas_inter tdas' diffs)) in *)
(*       if  *)
(* 	(tdas_matches tdas d) &&  *)
(* 	(Name.Set.cardinal d > Name.Set.cardinal md) *)
(*       then  *)
(* 	d *)
(*       else *)
(* 	md	   *)
(*   in  *)
(*     TDom.fold cmp (tdom t) Name.Set.empty *)
(* *\) *)
      
(* (\* --- CK added to here --- *\) *)
