(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* type.ml - representation and functions on types       *)
(*********************************************************)
(* $Id$ *)

(* imports *)
let sprintf = Printf.sprintf
let debug = Trace.debug "type" 
let debug_msg msg = debug (fun () -> Printf.eprintf "%s\n%!" msg)

let fatal_error i msg = raise (Error.Harmony_error (fun () -> Format.eprintf "%s: %s" (Info.string_of_t i) msg))

(* symbolic comparisons *)
let eq = 0 
let lt = -1
let gt = 1

(* --------------- "flattened" domains --------------- *)
type fdom =
    Finite of Name.Set.t
  | CoFinite of Name.Set.t      
      
(* --------------- schema representation --------------- *)
type t = 
    Any of Info.t  
  | Atom of Info.t * string * t
  | Cat of Info.t * fdom * t list 
  | Union of Info.t * fdom * t list 
  | Var of Info.t * Syntax.qid * thunk
  | Wild of Info.t * Name.Set.t * int * int option * t
and thunk = unit -> t

(* --------------- accessors --------------- *)

let info_of_t = function
    Var(i,_,_)      -> i
  | Any(i)          -> i
  | Atom(i,_,_)     -> i
  | Wild(i,_,_,_,_) -> i
  | Cat(i,_,_)      -> i
  | Union(i,_,_)    -> i

let rec string_of_t = function
    Any(_)          -> "Any"
  | Var (_,x,thk)   -> Syntax.string_of_qid x
  | Atom(_,n,t)     -> Printf.sprintf "%s = %s" n (string_of_t t)
  | Wild(_,f,l,uo,t) -> 
      Printf.sprintf "*[%n,%s]%s = %s"
        l
        (match uo with None -> "w" | Some u -> string_of_int u)
        (if Name.Set.is_empty f then "" 
         else " \\ " ^ Misc.parens (Misc.concat_list ", " (Safelist.map Misc.whack (Name.Set.elements f))))
	(string_of_t t)
  | Cat(_,_,ts)   -> 
      Misc.curlybraces 
        (Misc.concat_list ", " 
           (Safelist.map string_of_t ts))
  | Union(_,_,ts) -> 
      Misc.parens 
        (Misc.concat_list " | " 
           (Safelist.map string_of_t ts))

let rec fdom_of_t = function
    Any(_)          -> CoFinite(Name.Set.empty)
  | Atom(_,n,_)     -> Finite(Name.Set.singleton n)
  | Cat(_,f,_)      -> f
  | Union(_,f,_)    -> f
  | Var(_,_,thk)    -> fdom_of_t (thk ())
  | Wild(_,f,_,_,_) -> CoFinite(f)

(* -------------- fdom operations --------------*)

let fdom_union fd1 fd2 = match fd1,fd2 with
    Finite(s1),Finite(s2)     -> Finite(Name.Set.union s1 s2)
  | CoFinite(t1),CoFinite(t2) -> CoFinite(Name.Set.inter t1 t2)
  | Finite(s),CoFinite(t)
  | CoFinite(t),Finite(s)     -> CoFinite(Name.Set.diff t s)

(* disjoint union of two flattened domains *)
let fdom_disjoint_union fd1 fd2 = match fd1, fd2 with
    Finite(s1),Finite(s2) -> 
      if Name.Set.is_empty (Name.Set.inter s1 s2) then 
        Some (Finite(Name.Set.union s1 s2))
      else None
  | CoFinite(t1),CoFinite(t2) -> 
      if Name.Set.equal t1 t2 then Some fd1
      else None
  | Finite(s),CoFinite(t) 
  | CoFinite(t),Finite(s) ->  
      if Name.Set.subset s t then 
        Some (CoFinite(Name.Set.diff t s))
      else None

let fdom_remove fd k = match fd with 
    Finite(s) -> Finite(Name.Set.remove k s)
  | CoFinite(t) -> CoFinite(Name.Set.add k t)

let is_finite fd = match fd with Finite(_) -> true | _ -> false

(* -------------- split and project --------------- *)
(* [split t k] returns a pair option. It yields None if the projection
   of t on k is empty. Otherwise, it returns Some p where (fst p) is
   the projection of t on k and (snd p) is the schema representing the
   type that remains after projecting *)
let rec split t0 k = match t0 with 
    Any(_) -> Some(t0,t0)
  | Atom(i,n,t) -> if k = n then Some(t,Cat(i,Finite(Name.Set.empty),[])) else None
  | Cat(i,fd,ts) ->
      let new_fd = fdom_remove fd k in 
      let t0_ko,new_ts_rev = Safelist.fold_left 
        (fun (t0_ko,new_ts_rev) ti -> 
           match split ti k with
               None                   -> t0_ko, ti::new_ts_rev
             | Some (ti_k, ti_res)    -> Some ti_k, ti_res::new_ts_rev)
        (None,[])
        ts in begin match t0_ko with 
            None -> None
          | Some t0_k -> Some (t0_k, Cat(i,new_fd,Safelist.rev new_ts_rev))
        end
  | Union(i,fd,ts) -> 
      let new_fd = fdom_remove fd k in 
      let t0_ko,new_ts_rev = Safelist.fold_left 
        (fun (t0_ko,new_ts_rev) ti -> 
           match split ti k with
               None                -> t0_ko, new_ts_rev
             | Some (ti_k, ti_res) -> Some ti_k, ti_res::new_ts_rev)
        (None,[])
        ts in begin match t0_ko with 
            None -> None
          | Some t0_k -> Some (t0_k, Union(i,new_fd,Safelist.rev new_ts_rev))
        end
  | Var(_,_,thk) -> split (thk ()) k
  | Wild(i,f,l,uo,t) -> 
      if Name.Set.mem k f then None
      else 
        let new_f = Name.Set.remove k f in 
        let residue = match uo with 
            None   -> Wild(i,new_f,l,None,t)
          | Some u -> 
              if u = 1 then Cat(i,Finite(Name.Set.empty),[])
              else Wild(i,new_f,l,Some (u-1),t) in
          Some(t, residue)

let project t0 k = 
  debug_msg (sprintf "project %s on %s\n%!" (string_of_t t0) (Misc.whack k));
  match split t0 k with 
    None       -> None
  | Some(tk,_) -> Some(tk)
      
let rec project_bang t0 f = match t0 with
    Any(_) -> Some t0
  | Atom(_,n,t) -> if Name.Set.mem n f then None else Some t
  | Cat(_,_,ts)
  | Union(_,_,ts) -> Safelist.fold_left
      (fun tko ti -> match tko with None -> project_bang ti f | _ -> tko)
        None 
        ts
  | Var(_,_,thk) -> project_bang (thk ()) f
  | Wild(_,f,_,_,t) -> Some t
          
(* -------------- ordering, equality for types --------------- *)
(* compare on types *)
let rec cmp_t t1 t2 : int = 
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
      cmp_lex_aux l1 l2 eq in      
  let cmp_uppers u1 u2 = match u1,u2 with
      None, None -> eq
    | Some _,None -> lt
    | None, Some _ -> gt
    | Some i1, Some i2 -> compare i1 i2 in
  let rec loop = function 
      []   -> eq
    | h::t -> 
        let cmp = h () in
          if cmp = eq then loop t else cmp in
    begin match t1,t2 with
        Any(_),Any(_) -> eq
      | Atom(_,n1,t1),Atom(_,n2,t2) -> 
          loop [ (fun () -> compare n1 n2)
               ; (fun () -> cmp_t t1 t2)]          
      | Cat(_,_,ts1),Cat(_,_,ts2)
      | Union(_,_,ts1),Union(_,_,ts2) -> cmp_lex ts1 ts2 cmp_t
      | Var(_,x,_),Var(_,y,_) -> Syntax.qid_compare x y
      | Wild(_,f1,l1,u1,t1),Wild(_,f2,l2,u2,t2) ->
          loop [ (fun () -> cmp_uppers u1 u2)
               ; (fun () -> compare l1 l2)
               ; (fun () -> compare f1 f2)
               ; (fun () -> cmp_t t1 t2)]
                          
      (* types with different shapes *)
      | Atom(_),_  -> lt
      | _,Atom(_)  -> gt
      | Wild(_),_  -> lt
      | _,Wild(_)  -> gt
      | Any(_),_   -> lt
      | _,Any(_)   -> gt
      | Var(_),_   -> lt
      | _,Var(_)   -> gt
      | Cat(_),_   -> lt
      | _,Cat(_)   -> gt
    end

(* a *very* conservative syntactic approximation of semantic equality 
   used to test projectability *)
let rec equal t1 t2 = match t1,t2 with
    Any(_),Any(_) -> true
  | Atom(_,n1,tn1),Atom(_,n2,tn2) -> (n1=n2) && (equal tn1 tn2)
  | Cat(_,_,ts1), Cat(_,_,ts2) ->
      if (Safelist.length ts1 = Safelist.length ts2) then 
        (Safelist.fold_left
	   (fun ok (ti1,ti2) -> ok && (equal ti1 ti2))
	   true
	   (Safelist.combine ts1 ts2))
      else
        false
  | Var(_,x1,_),Var(_,x2,_) -> Syntax.qid_compare x1 x2 = 0
  | Wild(_,f1,l1,u1,tx1),Wild(_,f2,l2,u2,tx2) -> 
      ((f1,l1,u1) = (f2,l2,u2) 
          && (equal tx1 tx2))
  | Union(_,_,ts1), Union(_,_,ts2) ->
      if (Safelist.length ts1 = Safelist.length ts2) then
        (Safelist.fold_left
	   (fun ok (ti1,ti2) -> ok && (equal ti1 ti2))
           true
	   (Safelist.combine ts1 ts2))
      else false
  |_ -> false

(* -------------------- constructors --------------- *)
(* we only expose these constructors and not the datatype because we
   check that some syntactic well-formedness conditions obtain: (1)
   concatenations have disjoint domains (2) schemas have the same type
   (according to equal) below every name. *)

(* helpers *)
let check_proj_name n ts = 
  let ok,_ = Safelist.fold_left
    (fun (ok,tno) ti -> match tno with
         None    -> (ok,project ti n)
       | Some tn -> begin match project ti n with
             None -> (ok,tno)
           | Some tn' -> (equal tn tn',tno)
         end)
    (true,None)
    ts in
    ok 

let check_proj_fdom fd ts = match fd with 
    Finite(s) -> 
      debug_msg (sprintf "check_proj_fdom FINITE{%s} [%s]" 
                   (Misc.concat_list ", " (Name.Set.elements s))
                   (Misc.concat_list ", " (Safelist.map string_of_t ts)));
      Name.Set.fold 
      (fun k bado -> match bado with 
           Some n -> bado
         | None -> if check_proj_name k ts then None else Some k)
      s
      None
  | CoFinite(f) -> 
      debug_msg (sprintf "check_proj_fdom COFINITE{%s} [%s]" 
                   (Misc.concat_list ", " (Name.Set.elements f))
                   (Misc.concat_list ", " (Safelist.map string_of_t ts)));
      let ok,_ = Safelist.fold_left
        (fun (ok,tno) ti -> match tno with
             None    -> (ok,project_bang ti f)
           | Some tn -> begin match project_bang ti f with
                 None -> (ok,tno)
               | Some tn' -> (equal tn tn',tno)
             end)
        (true,None)
        ts 
      in
        debug_msg "done";
        if ok then None else Some "!"

let assert_proj i fd ts = 
  let proj_error k = 
    fatal_error 
      i 
      (sprintf "schema is not projectable on %s\n%!" (Misc.whack k)) in        
  let fnames = Safelist.fold_left 
    (fun ns ti -> 
       let fdi = fdom_of_t ti in
         match fdi with 
             Finite(s) -> Name.Set.union ns s
           | _         -> ns)
    Name.Set.empty 
    ts in
    match check_proj_fdom (Finite(fnames)) ts with 
        None -> begin
          if is_finite fd then ()
          else match check_proj_fdom (CoFinite(fnames)) ts with
              None -> ()
            | Some k -> proj_error k 
        end
      | Some k -> proj_error k
          
(* constructors *)
let mk_any i = Any(i)

let mk_atom i n t = Atom(i,n,t)
          
let mk_cat i ts = 
  let sorted_ts = Safelist.sort cmp_t ts in
  let fdo = 
    Safelist.fold_left
      (fun fdo ti -> match fdo with 
           None -> None
         | Some fd -> fdom_disjoint_union fd (fdom_of_t ti))
      (Some (Finite(Name.Set.empty)))
      sorted_ts in    
    match fdo with 
        Some fd -> 
          assert_proj i fd ts;
          Cat(i,fd,sorted_ts)            
      | None -> fatal_error i "domain overlap in type concatenation" 
          
let mk_union i ts = 
  let sorted_ts = Safelist.sort cmp_t ts in
  let fd = Safelist.fold_left
    (fun fd ti -> fdom_union fd (fdom_of_t ti))
    (Finite(Name.Set.empty))
    sorted_ts
  in
    assert_proj i fd ts;
    Union(i,fd,sorted_ts)

let mk_var i q thk = Var(i,q,thk)

let mk_wild i f l u t = Wild(i,f,l,u,t)

(* --------------- constants --------------- *)
let mk_nil i = mk_cat i [mk_atom i V.nil_tag (mk_cat i [])]
let mk_cons i h t = mk_cat i [mk_atom i V.hd_tag h;
                              mk_atom i V.tl_tag t]

(* --------------- check_contractive --------------- *)
let rec is_contractive xs t0 = match t0 with 
    Any(_) | Atom(_) | Wild(_)  -> true
  | Union(_,_,ts) | Cat(_,_,ts) -> Safelist.for_all (is_contractive xs) ts
  | Var(_,x,t)                  -> not (Safelist.exists (Syntax.qid_equal x) xs)
      
(* --------------- member and dom_member ---------------*)
(* [empty_view_member t] returns [true] iff {} is a member of t *)
let rec empty_view_member t0 = 
  let res = 
    match t0 with
        Any(_) -> true
      | Atom(_) -> false
      | Union(_,_,ts) -> Safelist.exists empty_view_member ts
      | Cat(_,_,ts) -> Safelist.for_all empty_view_member ts
      | Var(_,_,thk) -> empty_view_member (thk ())
      | Wild(_,_,l,uo,_) -> (l <= 0) && (match uo with None -> true | Some u -> (u >=0))
  in
    debug_msg (sprintf "empty_view_member %s = %b" (string_of_t t0) res);
    res
        
let rec member_aux v t0 = 
  debug_msg (sprintf "member_aux %s %s\n%!" (V.string_of_t v) (string_of_t t0));
  match t0 with 
    Any(_) -> Some []
  | Atom(_,n,t) -> 
      let d = V.dom v in 
        if (Name.Set.cardinal d = 1) 
          && (Name.Set.choose d = n)
        then Some [(n,t)]
        else None
  | Union(_,_,ts) ->
      let rec loop acc l = 
        match acc with 
            None -> begin match l with
                [] -> None
              | h::t -> loop (member_aux v h) t
            end 
          | Some _ -> acc in 
        loop None ts
  | Cat(_,_,ts) -> 
      (* possible optimization: check if (V.dom v) is a subset of the
         flattened dom, return None if not *)
      let split_reso = 
        Name.Set.fold 
          (fun k acco -> 
             match acco with 
                 None -> None
               | Some (t,ps) -> begin
                   match split t k with 
                       None -> None
                     | Some(tk,tres) -> Some (tres, (k,tk)::ps)
                 end)
          (V.dom v)
          (Some (t0,[])) in
        begin 
          match split_reso with 
              None -> None
            | Some(tres,ps) -> if empty_view_member tres then Some(ps) else None
        end                          

  | Var(_,_,thk) -> member_aux v (thk ())
  | Wild(_,f,l,uo,t) -> 
      let d = V.dom v in
      let c = Name.Set.cardinal d in
        if (l <= c)
          && (match uo with None -> true | Some u -> c <= u)
          && (Name.Set.is_empty (Name.Set.inter f d))
        then 
          Some (Name.Set.fold (fun k ps -> (k,t)::ps) d [] )
        else 
          None
	    
let rec member v t0 = 
  debug 
    (fun () -> Printf.eprintf ">>> member %s %s\n%!" (V.string_of_t v) (string_of_t t0))
  ;
  let res = match member_aux v t0 with 
      None -> 
        debug (fun () -> Printf.eprintf "member_aux returned false\n%!")
        ;
        false 
    | Some ps -> 
        debug (fun () -> Printf.eprintf "member_aux returned: [%s]\n%!"
                 (Misc.concat_list ", " 
                    (Safelist.map (fun (k,tk) -> 
                                     sprintf "(%s,%s)" 
                                       (Misc.whack k)
                                       (string_of_t tk))
                    ps)))
        ;
        Safelist.for_all (fun (k,tk) -> member (V.get_required v k) tk) ps
  in
    debug 
      (fun () -> Printf.eprintf "<<<member %s %s = %b\n%!" 
         (V.string_of_t v) 
         (string_of_t t0) 
         res)
    ;
    res

let dom_member v t0 = match member_aux v t0 with None -> false | Some _ -> true
