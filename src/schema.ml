(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* type.ml - representation and functions on types       *)
(*********************************************************)
(* $Id$ *)

(* imports *)
let (@) = Safelist.append
let sprintf = Printf.sprintf
let debug = Trace.debug "type" 

let fatal_error i msg_thk =
  raise (Error.Harmony_error 
           (fun () -> 
              Format.printf "%s: @[<2>" (Info.string_of_t i);
              msg_thk ();
              Format.printf "@]"))

(* symbolic comparisons *)
let eq = 0 
let lt = -1
let gt = 1

(* --------------- schema representation --------------- *)
type t = 
    Any of Info.t  
  | Atom of Info.t * string * t
  | Cat of Info.t * t list 
  | Union of Info.t * t list 
  | Var of Info.t * Syntax.qid * thunk
(* For Wild: the names are the ones that are excluded, 
 * the int is the number of "!", the bool is the presence of "*" *)
  | Wild of Info.t * Name.Set.t * int * bool * t
and thunk = unit -> t

(* --------------- accessors --------------- *)
let info_of_t = function
    Any(i)          -> i
  | Atom(i,_,_)     -> i
  | Cat(i,_)        -> i
  | Union(i,_)      -> i
  | Var(i,_,_)      -> i
  | Wild(i,_,_,_,_) -> i

(* returns the set of free variables of a type, represented as a
   finite map from variables to their expansions *)
let rec fv = function
    Any(_)                        -> Syntax.QidMap.empty
  | Var(_,q,thk)                  -> Syntax.QidMap.from_list [(q,thk ())]
  | Wild(_,_,_,_,t) | Atom(_,_,t) -> fv t
  | Cat(_,ts) | Union(_,ts)       ->
      Safelist.fold_left 
        (fun acc ti -> 
           Syntax.QidMap.fold 
             (fun q t acc -> Syntax.QidMap.add q t acc) 
             (fv ti) 
             acc)
        Syntax.QidMap.empty 
        ts

let format_t t0 = 
  let rec format_t_aux cat = function
      Any(_)          -> Format.printf "Any"
    | Var (_,x,thk)   -> Format.printf "%s" (Syntax.string_of_qid x)
    | Atom(_,n,t)     -> 
        if not cat then Format.printf "{";
        Format.printf "%s@,=@," (Misc.whack n); 
        format_t_aux false t; 
        if not cat then Format.printf "}";
    | Wild(_,f,l,u,t) ->
        let rec format_n_bangs n = match n with 
            0 -> ()
          | n -> Format.printf "!"; format_n_bangs (n-1) in        
          if not cat then Format.printf "{";
          (match l,u with 
               0,true -> Format.printf "*"
             | n,true -> format_n_bangs n; Format.printf "*"
             | n,false -> format_n_bangs n);
          if not (Name.Set.is_empty f) then 
            begin 
              Format.printf "\\ (@[";
              Misc.format_list ",@ " (fun n -> Format.printf "%s" (Misc.whack n)) (Name.Set.elements f);
              Format.printf "@])"
            end;
          Format.printf "@,=@,";
          format_t_aux false t;
          if not cat then Format.printf "}";
    | Cat(_,ts) -> 
        Format.printf "{@["; 
        Misc.format_list ",@ " (format_t_aux true) ts; 
        Format.printf "@]}"
    | Union(_,ts) -> 
        Format.printf "(@["; 
        Misc.format_list "|@ " (format_t_aux false) ts; 
        Format.printf "@])" in

  (* helper to compute the free variables that appear in t *)
  (* uses a slow fixed-point computation ... should fix later *)
  let rec loop fvs = 
    let union fvs1 fvs2 = Syntax.QidMap.fold Syntax.QidMap.add fvs1 fvs2 in
    let qs = Syntax.QidMap.domain fvs in
    let fvs' = Syntax.QidMap.fold 
      (fun q t acc -> if Syntax.QidSet.mem q qs then acc else union (fv t) acc) 
      fvs 
      fvs in
      if Syntax.QidSet.equal qs (Syntax.QidMap.domain fvs') then fvs
      else loop fvs' in
  let fvs = loop (fv t0) in
    Format.printf "@[";
    format_t_aux false t0;
    if not (Syntax.QidMap.is_empty fvs) then 
      begin 
        Format.printf "@\n  where @[";
        Syntax.QidMap.fold 
          (fun q t () -> 
             Format.printf "%s@ =@ @[" (Syntax.string_of_qid q);
             format_t_aux false t;
             Format.printf "@]@\n")
          fvs
          ();
        Format.printf "@]";
      end;
    Format.printf "@]"
      
(* --------------- "flattened" domains --------------- *)
type fdom =
    Finite of Name.Set.t
  | CoFinite of Name.Set.t

let is_finite fd = match fd with Finite(_) -> true | _ -> false

let fdom_union fd1 fd2 = match fd1,fd2 with
    Finite(s1),Finite(s2)     -> Finite(Name.Set.union s1 s2)
  | CoFinite(t1),CoFinite(t2) -> CoFinite(Name.Set.inter t1 t2)
  | Finite(s),CoFinite(t)
  | CoFinite(t),Finite(s)     -> CoFinite(Name.Set.diff t s)

(* disjoint union of two flattened domains *)
let check_disjoint fd1 fd2 = match fd1, fd2 with
    Finite(s1),Finite(s2) -> 
      let inter = Name.Set.inter s1 s2 in
      if Name.Set.is_empty inter then None
      else Some inter
  | CoFinite(t1),CoFinite(t2) -> 
      if Name.Set.equal t1 t2 then None
      else Some (Name.Set.union (Name.Set.diff t2 t1) (Name.Set.diff t1 t2))
  | Finite(s),CoFinite(t) 
  | CoFinite(t),Finite(s) ->  
      if Name.Set.subset s t then None
      else Some (Name.Set.diff s t)
      
let rec fdom_of_t = function
    Any(_)          -> CoFinite(Name.Set.empty)
  | Atom(_,n,_)     -> Finite(Name.Set.singleton n)
  | Cat(_,ts)       -> 
      Safelist.fold_left 
        fdom_union 
        (Finite(Name.Set.empty)) 
        (Safelist.map fdom_of_t ts)
  | Union(_,ts)     -> 
      Safelist.fold_left 
        fdom_union 
        (Finite(Name.Set.empty)) 
        (Safelist.map fdom_of_t ts)        
  | Var(_,_,thk)    -> fdom_of_t (thk ())
  | Wild(_,f,_,_,_) -> CoFinite(f)

(* -------------- split and project --------------- *)
(* [split t k] returns a pair option. It yields None if the projection
   of t on k is empty. Otherwise, it returns Some p where (fst p) is
   the projection of t on k and (snd p) is the schema representing the
   type that remains after projecting *)
let rec split t0 k = match t0 with 
    Any(_) -> Some(t0,t0)
  | Atom(i,n,t) -> if k = n then Some(t,Cat(i,[])) else None
  | Cat(i,ts) ->
      let t0_ko,new_ts_rev = Safelist.fold_left 
        (fun (t0_ko,new_ts_rev) ti -> 
           match split ti k with
               None                   -> t0_ko, ti::new_ts_rev
             | Some (ti_k, ti_res)    -> Some ti_k, ti_res::new_ts_rev)
        (None,[])
        ts in begin match t0_ko with 
            None -> None
          | Some t0_k -> Some (t0_k, Cat(i,Safelist.rev new_ts_rev))
        end
  | Union(i,ts) -> 
      let t0_ko,new_ts_rev = Safelist.fold_left 
        (fun (t0_ko,new_ts_rev) ti -> 
           match split ti k with
               None                -> t0_ko, new_ts_rev
             | Some (ti_k, ti_res) -> Some ti_k, ti_res::new_ts_rev)
        (None,[])
        ts in begin match t0_ko with 
            None -> None
          | Some t0_k -> Some (t0_k, Union(i,Safelist.rev new_ts_rev))
        end
  | Var(_,_,thk) -> split (thk ()) k
  | Wild(i,f,l,u,t) -> 
      if Name.Set.mem k f then None
      else 
        let new_f = Name.Set.remove k f in 
        let residue = match u with 
            true   -> Wild(i,new_f,l,true,t)
          | false  -> 
              if l = 1 then Cat(i,[])
              else Wild(i,new_f,l-1,false,t) in
          Some(t, residue)

let project t0 k = match split t0 k with 
    None       -> None
  | Some(tk,_) -> Some(tk)
      
let rec project_bang t0 f = match t0 with
    Any(_) -> Some t0
  | Atom(_,n,t) -> if Name.Set.mem n f then None else Some t
  | Cat(_,ts)
  | Union(_,ts) -> Safelist.fold_left
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
      | Cat(_,ts1),Cat(_,ts2)
      | Union(_,ts1),Union(_,ts2) -> cmp_lex ts1 ts2 cmp_t
      | Var(_,x,_),Var(_,y,_) -> Syntax.qid_compare x y
      | Wild(_,f1,l1,u1,t1),Wild(_,f2,l2,u2,t2) ->
          loop [ (fun () -> compare u1 u2)
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
  | Cat(_,ts1), Cat(_,ts2) ->
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
  | Union(_,ts1), Union(_,ts2) ->
      if (Safelist.length ts1 = Safelist.length ts2) then
        (Safelist.fold_left
	   (fun ok (ti1,ti2) -> ok && (equal ti1 ti2))
           true
	   (Safelist.combine ts1 ts2))
      else false
  |_ -> false

(* -------------------- constructors --------------- *)
let mk_any i          = Any(i)
let mk_atom i n t     = Atom(i,n,t)
let mk_cat i ts       = Cat(i,Safelist.sort cmp_t ts)
let mk_union i ts     = Union(i,Safelist.sort cmp_t ts)
let mk_var i q thk    = Var(i,q,thk)
let mk_wild i f l u t = Wild(i,f,l,u,t)

(* is_contractive xs t0 returns [None] if t0 is contractive in [xs]
   and [Some xi] otherwise *)
let rec is_contractive t0 xs = match t0 with
    Any(_) | Atom(_) | Wild(_) -> None
  | Cat(_,ts) | Union(_,ts) -> Safelist.fold_left (fun reso ti -> match reso with Some _ -> reso | None -> is_contractive ti xs) None ts
  | Var(_,q,_) -> if Safelist.exists (Syntax.qid_equal q) xs then Some q else None

(* [proj_all f ts] projects each of the types in [ts] using [f]. If
   all the projections return the same type, it returns [None],
   otherwise it returns a pair option [Some (t1,t2)] where [t1] and
   [t2] are the distinct projections found. *)
let proj_all f ts = 
  match Safelist.fold_left
    (fun acc ti -> match acc with
         (Some _,Some _)  -> acc
       | (None,_)        -> (f ti, None)
       | (Some tn, None) -> 
           begin
             match f ti with
                 None -> acc
               | Some tn' -> if equal tn tn' then acc else (Some tn, Some tn')
           end)
    (None, None)
    ts
  with
      Some t1, Some t2 -> Some (t1,t2)
    | _ -> None
        
let is_proj_aux i fd ts = 
  let fns,cfns = Safelist.fold_left 
    (fun (fns,cfns) t -> 
       match fdom_of_t t with 
           Finite(s) -> (Name.Set.union fns s, cfns)
         | CoFinite(t) -> (cfns, Name.Set.union cfns t))
    (Name.Set.empty, Name.Set.empty)
    ts in
  let finite_proj =
    Name.Set.fold 
      (fun k reso -> match reso with 
           Some _ -> reso
         | None -> match proj_all (fun t -> project t k) ts with
               Some(t1,t2) -> Some(k,t1,t2)
             | None        -> None)
      fns
      None
  in
  let infinite_proj = 
    if is_finite fd then None
    else match proj_all (fun t -> project_bang t cfns) ts with
        Some(t1,t2) -> Some("!",t1,t2)
      | None        -> None
  in
    match finite_proj, infinite_proj with 
        None, None -> None
      | Some r,_ | _,Some r -> Some r

(* [is_projectable fd t0] returns [None] if [t0] is projectable (on all
   names) and [Some(k,t1,t2)] otherwise where [k] is the child that
   the type is not projectable on, and [t1], [t2] are the distinct
   projected types *)
(* precondition: t0 is contractive in all recursive variables (will
   loop otherwise) *)
let rec is_projectable t0 = match t0 with 
    Any(_) | Atom(_) | Wild(_) -> None
  | Var(_,_,thk)               -> is_projectable (thk ())
  | Cat(i,ts) | Union(i,ts)    -> is_proj_aux i (fdom_of_t t0) ts

let rec has_disjoint_cats t0 = match t0 with
    Any(_) | Atom(_) | Wild(_) -> None
  | Var(_,_,thk) -> has_disjoint_cats (thk ())
  | Cat(i,ts) -> 
      let _,reso = Safelist.fold_left
        (fun (fd,reso) ti -> 
           match reso with 
               Some _ -> (fd,reso) 
             | None -> 
                 let ti_fd = fdom_of_t ti in 
                   (fdom_union fd ti_fd, check_disjoint fd ti_fd))
        (Finite(Name.Set.empty), None)
        ts in
        reso      
  | Union(i,ts) -> 
      Safelist.fold_left 
        (fun acco ti -> match acco with Some _ -> acco | None -> has_disjoint_cats ti) 
        None 
        ts
      
let assert_wf t0 xs =
  (match is_contractive t0 xs with 
       None -> ()
     | Some x -> 
        fatal_error
          (info_of_t t0)
          (fun () -> 
             Format.printf "schema variable %s may not be used recursively in@,@["
               (Syntax.string_of_qid x);
             format_t t0;
             Format.printf "@]"))
  ;
  (match has_disjoint_cats t0 with 
       None -> ()
     | Some fs -> 
         fatal_error
           (info_of_t t0)
           (fun () -> 
              Format.printf "schema@ "; 
              format_t t0;
              Format.printf "@ has domain overlap on {%s}"
                (Misc.concat_f_list ", " Misc.whack (Name.Set.elements fs))))
  ;
  (match is_projectable t0 with 
       None -> ()
     | Some(k,t1,t2) -> 
         fatal_error
           (info_of_t t0)
           (fun () -> 
              Format.printf "schema@ ";
              format_t t0;
              Format.printf "@ is not projectable on %s;@ "                 
                (Misc.whack k);
              format_t t1;
              Format.printf "@ <> @ ";
              format_t t2))
    
(* --------------- constants --------------- *)
let mk_nil i = mk_cat i [mk_atom i V.nil_tag (mk_cat i [])]
let mk_cons i h t = mk_cat i [mk_atom i V.hd_tag h;
                              mk_atom i V.tl_tag t]
      
(* --------------- member and dom_member ---------------*)

(* membership test results *)
type membership =
    Member of (Name.t * t) list
  | Failure of V.t * t
	
(* [empty_view_member t] returns [true] iff {} is a member of t *)
let rec empty_view_member t0 = match t0 with
    Any(_) -> true
  | Atom(_) -> false
  | Union(_,ts) -> Safelist.exists empty_view_member ts
  | Cat(_,ts) -> Safelist.for_all empty_view_member ts
  | Var(_,_,thk) -> empty_view_member (thk ())
  | Wild(_,_,l,u,_) -> (l <= 0) && (match u with true -> true | false -> not (l > 0))
        
let rec member_aux v t0 = match t0 with 
    Any(_) -> Member []
  | Atom(_,n,t) -> 
      let d = V.dom v in 
        if (Name.Set.cardinal d = 1) 
          && (Name.Set.choose d = n)
        then Member [(n,t)]
        else Failure (v, t0)
  | Union(_,ts) ->
      let rec loop acc l = match acc with 
        Member _ -> acc
      | Failure _ -> 
	  begin match l with
            [] -> Failure (v, t0)
          | h::t -> loop (member_aux v h) t
	  end in
      loop (Failure (v, t0)) ts
  | Cat(_,ts) ->
      let split_reso = 
        Name.Set.fold 
          (fun k acco -> match acco with 
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
          None -> Failure (v, t0)
        | Some(tres,ps) -> 
	    if empty_view_member tres then 
	      Member(ps) 
	    else 
	      Failure(v,t0)
      end
  | Var(_,_,thk) -> member_aux v (thk ())
  | Wild(_,f,l,uo,t) -> 
      let d = V.dom v in
      let c = Name.Set.cardinal d in
        if (l <= c)
          && (match uo with true -> true | false -> (l = c))
          && (Name.Set.is_empty (Name.Set.inter f d))
        then 
          Member (Name.Set.fold (fun k ps -> (k,t)::ps) d [] )
        else 
          Failure (v,t0)
	    
let rec member v t0 = match member_aux v t0 with 
    Failure _ -> false 
  | Member ps  -> 
      Safelist.for_all 
        (fun (k,tk) -> member (V.get_required v k) tk) 
        ps

let rec pick_bad_subtree v t0 = match member_aux v t0 with 
    Failure (v,t) -> Some (v,t)
  | Member ps -> 
      let rec for_all = function
	  [] -> None
	| (k,tk)::q -> 
	    match pick_bad_subtree (V.get_required v k) tk with
	      Some (v,t) -> Some (v,t)
	    | None -> for_all q		  
      in
      for_all ps

let dom_member v t0 = match member_aux v t0 with Failure _ -> false | Member _ -> true

(* --------------- keyed schemas --------------- *)
let rec is_empty_schema = function
  | Any _ -> false
  | Atom(_,_,t) -> is_empty_schema t
  | Cat(_,ts) -> false
  | Union(_,ts) -> Safelist.for_all is_empty_schema ts
  | Var(_,_,th) -> is_empty_schema (th ())
  | Wild(_,_,i,b,t) -> (i<=0 && not b) || is_empty_schema t

let rec is_keyed_schema = function
  | Any _ -> false
  | Atom(_,_,_) -> true
  | Cat(_,ts) -> (Safelist.fold_right (fun elt acc ->
                                         if is_empty_schema elt then acc
                                         else if is_keyed_schema elt then acc + 1
                                         else acc + 2)) ts 0 = 1
  | Union(_,ts) -> Safelist.for_all is_keyed_schema ts
  | Var(_,_,th) -> is_keyed_schema (th ())
  | Wild(_,_,i,b,_) -> i=1 && not b

let rec mk_spine_cons_from_schema i s1 s2 =
  if not (is_keyed_schema s1) then
    fatal_error i
      (fun () -> 
         Format.printf "non keyed values allowed by schema@,@[";
         format_t s1;
         Format.printf "@]");
  match s1 with
  | Any _ -> assert false
  | Atom(i,s,t) -> mk_cat i [mk_atom i V.hd_tag t;
                             mk_atom i s s2]
  | Cat(i,ts) -> (* we know there is exactly one non-empty keyed schema in ts *)
      let s' =
        let rec loop = function
          | [] -> assert false
          | t :: ts -> if is_keyed_schema t then t else loop ts
        in loop ts
      in mk_spine_cons_from_schema i s' s2
  | Union(i,ts) -> mk_union i (Safelist.map (fun t -> mk_spine_cons_from_schema i t s2) ts)
  | Var(i,b,th) -> mk_spine_cons_from_schema i (th ()) s2
  | Wild(i,e,n,b,t) ->
      assert (n=1 && not b);
      mk_cat i [mk_atom i V.hd_tag t;
                mk_wild i e n b s2]

let mk_spine_cons_from_value i v1 s2 =
  let rec schema_of_tree v =
    mk_cat 
      i
      (V.fold  
         (fun k vk ts -> (mk_atom i k (schema_of_tree vk))::ts)
         v
         [])
  in
  let d = V.dom v1 in
  if Name.Set.cardinal d <> 1 then
    fatal_error i
      (fun () -> 
         Format.printf "This tree should have a single child:";
         V.format_t v1);
  let tl = Name.Set.choose d in
    if (tl = V.tl_tag) || (tl = V.nil_tag) || (tl = V.hd_tag) then
      fatal_error i
        (fun () -> 
           V.format_t v1;
           Format.printf "@ is using a forbidden name as key!");
    mk_cat i [mk_atom i V.hd_tag (schema_of_tree (V.get_required v1 tl));
              mk_atom i tl s2]
