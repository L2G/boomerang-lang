(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* schema.ml - representation and functions on types         *)
(*             based on "A Logic You Can Count On", POPL '04 *)
(*************************************************************)
(* $Id$ *)

(* -------------- imports / abbreviations -------------- *)
let (@) = Safelist.append
let sprintf = Printf.sprintf
let debug = Trace.debug "schema" 
module NS = Name.Set
module NM = Name.Map 
module P = Presburger
module IM = P.IntMap
  
(* --------------- schema representation --------------- *)

(* to handle recursive definitions, we need to be able to represent
 * intersections, differences, and unions of types symbolically. the
 * algorithms in this module build and manipulate an implicit automaton
 * whose states are the symbolic represetations of intersections,
 * differences, and unions of ordinary type variables given here. *)

(* a tvar [x] (i.e., just a string) denotes its static definition in
   delta_tvar below *)
type tvar = NS.elt
    
(* a cvar [(is,ds)] represents the type obtained by intersecting all
   the tvars in [is] and removing all the tvars in [ds] *)
type cvar = NS.t * NS.t

(* a CSet.t is just a finite union of cvars *)
module CSet = Set.Make(
  struct 
    type t = cvar
    let compare (is1,ds1) (is2,ds2) = 
      let cmp = NS.compare is1 is2 in
        if cmp <> 0 then cmp 
        else NS.compare ds1 ds2
  end)
  
(* automaton states either one of several common constants, or a CSet.t *)
type state = True | False | EmptyView | NonEmptyView | CS of CSet.t

(* locations are either a single name, or a cofinite set of names *)
type loc = Single of Name.t | CoFinite of NS.t

(* an element is a pair of a location and a state *)
type elt = loc * state

(* syntactic reprsentations of schemas *)
type syn_t = 
    Var of string
  | Atom of Name.t * syn_t 
  | Wild of Name.Set.t * int * bool * syn_t
  | Cat of syn_t list
  | Union of syn_t list
  | Isect of syn_t list
  | Restrict of syn_t * Name.Set.t * bool
  | Inject of syn_t * syn_t
  | Neg of syn_t

(* the type t of schemas is either a variable given by a state, or a a
   counting formula over a proper basis *)
type t = V of state | F of Presburger.t * elt list * syn_t

(* --------------- conversions / simple operations --------------- *)
let cset_of_cvar cx = CSet.singleton cx
let cset_of_tvar x = cset_of_cvar (NS.singleton x, NS.empty)
let cstate_of_tvar x = CS (cset_of_tvar x)
let is_cvar s = CSet.cardinal s = 1
let is_tvar s = (is_cvar s) && 
  let (is,ds) = CSet.choose s in
    (NS.cardinal is = 1) && (NS.is_empty ds)
let is_var = function V _ -> true | _ -> false

(* --------------- formatting --------------- *)
let string_of_cvar (is,ds) = 
  let ns2str sep s = Misc.concat_list sep (NS.elements s) in    
    sprintf "%s%s" 
      (if NS.is_empty is then "0" else (ns2str "&" is))
      (if NS.is_empty ds then "" else sprintf "-(%s)" (ns2str "|" ds))

let string_of_cset cxs = 
  if CSet.is_empty cxs then "{}" else
    Misc.concat_list "|" (Safelist.map string_of_cvar (CSet.elements cxs))

let string_of_state s0 = match s0 with 
    True         -> "T"
  | False        -> "F"
  | EmptyView    -> "{}"
  | NonEmptyView -> "T-{}"
  | CS s         -> string_of_cset s

let fformat_state fmtr s0 = Format.fprintf fmtr "%s" (string_of_state s0)

let fformat_elt fmtr e0 = match e0 with
    (Single n,x) -> 
      Format.fprintf fmtr "%s[" (Misc.whack n);
      fformat_state fmtr x;
      Format.fprintf fmtr "]"

  | (CoFinite f,x) -> 
      Format.fprintf fmtr "~{%s}[" 
        (Misc.concat_list "," (Safelist.map Misc.whack (NS.elements f)));
      fformat_state fmtr x;
      Format.fprintf fmtr "]"
      
let rec fformat_syn_t fmtr syn_t0 = match syn_t0 with 
    Var(x) -> Format.fprintf fmtr "%s" x
  | Atom(n,t) -> 
      Format.fprintf fmtr "@[{%s=" n;
      fformat_syn_t fmtr t;
      Format.fprintf fmtr "}@]"
  | Wild(f,l,u,t) -> 
      Format.fprintf fmtr "@[{";
      (match l,u with 
           0,true -> Format.printf "*"
         | n,b -> 
             for i=1 to n do Format.printf "!" done; 
             if b then Format.printf "*");
      if not (Name.Set.is_empty f) then
        (Format.printf "\\(@[";
         Misc.fformat_list fmtr ",@ " 
           (Format.fprintf fmtr "%s") 
           (Name.Set.elements f);
         Format.printf "@])");
      Format.printf "=@,";
      fformat_syn_t fmtr t;
      Format.printf "}@]" 
  | Cat(ts)    -> Misc.fformat_list fmtr "+" (fformat_syn_t fmtr) ts
  | Union(ts)  -> Misc.fformat_list fmtr "|" (fformat_syn_t fmtr) ts
  | Isect(ts)  -> Misc.fformat_list fmtr "&" (fformat_syn_t fmtr) ts
  | Neg(t)     -> 
      Format.fprintf fmtr "~"; 
      fformat_syn_t fmtr t
  | Inject(t1,t2) -> 
      Format.fprintf fmtr "Inject(";
      fformat_syn_t fmtr t1;
      Format.fprintf fmtr ",";
      fformat_syn_t fmtr t2;
      Format.fprintf fmtr ")"
  | Restrict(t,ns,b) -> 
      Format.fprintf fmtr "Restrict(";
      fformat_syn_t fmtr t;
      Format.fprintf fmtr ",%s[%s])"
        (if b then "" else "not ")
        (Misc.concat_list "," (Name.Set.elements ns))
          
      
   
let fformat_t fmtr t0 = match t0 with
    V(x) -> fformat_state fmtr x
  | F(p,e,syn) -> fformat_syn_t fmtr syn 
      (* Format.fprintf fmtr "F[@[";
         Presburger.fformat_t fmtr p;
         Format.fprintf fmtr "@]@,.@[";
         Misc.fformat_list fmtr "@,," 
         (fun (d,e) -> Format.fprintf fmtr "n%d=" d; fformat_elt fmtr e) 
         (Misc.map_index_filter (fun n ei -> Some (n,ei)) e);
         Format.fprintf fmtr "@]]" *)
      
let format_elt = fformat_elt Format.std_formatter
let format_t = fformat_t Format.std_formatter

let syn_of_t = function
    V(x) -> Var(string_of_state x) 
  | F(_,_,si) -> si

(* --------------- fresh variables generation ----------------- *)
let fresh_map : ((int NM.t) ref) = ref NM.empty
let fresh x = 
  let n = NM.safe_find x (!fresh_map) 0 in 
    fresh_map := NM.add x (n + 1) (!fresh_map);
    let res = sprintf "_%s_%d" x n in res

(* --------------- maps, sets, environments ------------------ *)
type this_t = t (* hack! *)

module TMapplus = Mapplus.Make(
  struct
    type t = this_t
    let compare = compare
    let to_string t0 = 
      let buf = Buffer.create 20 in 
      let fmtr = Format.formatter_of_buffer buf in 
        fformat_t fmtr t0;
        Format.pp_print_flush fmtr (); 
        Buffer.contents buf
  end)
module TSet = TMapplus.KeySet
module TMap = TMapplus.Map   
      
module TVarEnv = Env.Make(
  struct
    type t = tvar
    let compare = compare
    let to_string x = x
  end)

module CSetEnv = Env.Make(
  struct
    type t = CSet.t
    let compare = CSet.compare
    let to_string = string_of_cset 
  end)

(* we maintain two environments: 
     - delta_tvar maps tvars to ts
     - delta_tvar maps compound type states to ts
   we could (and did, in an earlier version) use a unified
   environment, but lookups are slower when we have a simple type
   variable *)

let delta_tvar : (t TVarEnv.t ref) = ref (TVarEnv.empty ())
let delta : (t CSetEnv.t) ref = ref (CSetEnv.empty ())

let update_tvar x t = delta_tvar := (TVarEnv.update (!delta_tvar) x t)
let update_cset cxs t = delta := (CSetEnv.update (!delta) cxs t)
let update_cvar cx t = update_cset (cset_of_cvar cx) t

(* ------------- simple lookup functions ------------- *)
(* generic helper: 
   lookup a bunch of variables from a set, returning a list option *)
let lookup_vars fld lkup s = fld
  (fun xi a -> match a,lkup xi with
       None,_ | _,None -> None
     | Some a,Some t -> Some(t::a)) 
  s
  (Some []) 

let lookup_tvar x = TVarEnv.lookup (!delta_tvar) x

let lookup_tvars = lookup_vars NS.fold lookup_tvar 

(* note: the other lookup functions, lookup_cvar and lookup_cset, are
   defined below. as they need to compute intersections/differences,
   they must be defined mutually with several other functions *)

(* helper for lookup functions: the same as Misc.map_option, but more
   readable with option arg first! *)
let map_opt o f = match o with
    None -> None
  | Some a -> Some (f a)

let map_opto o f = match o with
    None -> None
  | Some a -> (f a)

(* ----------------- support for recursive definitions --------------------- *)

(* when we compile nested recursive schemas, we sometimes need to
 * delay creating some schemas until coming back to the outermost
 * definition, when all of the schemas are defined 
 *)

(* the marked_tvars contain all the currently-compiled schemas *)
let marked_tvars_cell = ref NM.empty
let mark_tvar x io = marked_tvars_cell := (NM.add x io (!marked_tvars_cell))
let mark_tvars l = Safelist.iter (fun (x,i) -> mark_tvar x (Some i)) l
let marked_tvar xs x = NS.mem x xs
let marked_tvars xs ys = NS.fold 
  (fun yi u -> if marked_tvar xs yi then NS.add yi u else u)
  ys NS.empty  
let marked_cvar xs (is,ds) = NS.union (marked_tvars xs is) (marked_tvars xs ds)  
let marked_cset xs cxs = CSet.fold 
  (fun ysi u -> NS.union (marked_cvar xs ysi) u)
  cxs NS.empty
let marked_ts = Safelist.fold_left
  (fun u ti -> match ti
   with V(CS(s)) -> NS.union (marked_cset (NM.domain (!marked_tvars_cell)) s) u
     | _ -> u)
  NS.empty

(* the delayed_work is a list of thunks of schemas we defer until
   reaching the outermost definition *)
let delayed_work : ((unit -> unit) list ref) = ref []
let add_delayed x f = 
  delayed_work := f::!delayed_work;
  mark_tvar x None

(* finalize: called by outermost recursive schema definition
     - checks contractiveness
     - completes delayed work 
*)
let finalize () = 
  let contractive_error i xs = 
    let c = NS.cardinal xs in
      raise (Error.Harmony_error 
               (fun () -> Format.printf 
                  "schema variable%s%s%s at %s appears in a non-contractive position"
                (if c=1 then " " else "s [")              
                  (Misc.concat_list "," (NS.elements xs))
                  (if c=1 then "" else "]")
                  (Info.string_of_t i))) in

  let check_contractive () = NM.fold
    (fun x io () -> 
       match io,lookup_tvar x with 
           None,_ -> ()
         | Some i, Some (V(CS s)) -> 
             let xs = marked_cset (NM.domain !marked_tvars_cell) s in
               if not (NS.is_empty xs) then contractive_error i xs
         | _ -> ())
    (!marked_tvars_cell) 
    () in

  let cleanup () = 
    delayed_work := [];
    marked_tvars_cell := NM.empty in
    
    (* finalize body *)
    begin 
      try check_contractive () with e -> cleanup (); raise e
    end;
    Safelist.iter (fun f -> f ()) (Safelist.rev !delayed_work);
    cleanup ()
  
(* -------------- constants ----------------- *)
let anyE = (CoFinite NS.empty,True)

(* true, the largest schema *)
let truth,truth_formula,truth_basis = 
  let xtruth = "T" in
  let ftruth = P.mkGe(P.mkVar 0) P.zero in
  let btruth = [anyE] in
  let syn_truth = Wild(Name.Set.empty,0,true,Var(xtruth)) in
    update_tvar xtruth (F(ftruth, btruth,syn_truth));
    (xtruth,ftruth,btruth)
let truth_cs = cset_of_tvar truth

(* empty, the singleton schema containing V.empty *)
let empty,empty_formula,empty_basis = 
  let xempty = "{}" in
  let fempty = P.mkEq(P.mkVar 0) P.zero in
  let bempty = [anyE] in
  let syn_empty = Wild(Name.Set.empty, 0, false, Var(truth)) in
    update_tvar xempty (F(fempty, bempty,syn_empty));
    (xempty,fempty,bempty)
let empty_cs = cset_of_tvar empty

(* non_empty, the schema T-{} *)
let non_empty,non_empty_formula,non_empty_basis = 
  let xnon_empty = "T-{}" in
  let fnon_empty = P.mkGt(P.mkVar 0) P.zero in
  let bnon_empty = [anyE] in
  let syn_non_empty = Wild(Name.Set.empty,1,true,Var(truth)) in
    update_tvar xnon_empty (F(fnon_empty, bnon_empty,syn_non_empty));
    (xnon_empty,fnon_empty,bnon_empty)
let non_empty_cs = cset_of_tvar non_empty
 
(* --- convert a t to a state ----- *)

(* when we create an atom or a wildcard from a subschema t, we need to
   create a basis over t. as elements are defined using states, we need
   to find a state representing t. if we already have a variable x
   representing t, then we use it. otherwise we generate a fresh x and
   bind it to t in delta_tvar *)

(* JNF-CHECK: hosing things by keeping syntax alongside semantics? *)
module THash = Hashtbl.Make(
  struct
    type t = this_t
    let equal = (==)
    let hash o = Hashtbl.hash (Obj.magic o : int)
  end)

let varcache : (state THash.t) = THash.create 17
  
let state_of_t t0 = match t0 with 
    V(x) -> x
  | F(p,e,syn_t) -> 
      try 
        THash.find varcache t0
      with Not_found ->
        let n = fresh "_GEN" in             
        let x = CS(cset_of_tvar n) in
          update_tvar n t0;
          THash.add varcache t0 x;          
          x

(* -------------- easy syntactic ops on cvars and csets ----------------- *)

(* these operations depend on and maintain these invariants. 
 * let (es,ds) be a cvar:
 *    - if T is in es, then es = {T}
 *    - T is not in ds
 *    - es is disjoint from ds 
 *)
      
(* --- negation --- *)
let neg_cvar (es,ds) =  
  let u1 = NS.fold 
    (fun ei u -> 
       if ei = truth then u 
       else CSet.add (NS.singleton truth, NS.singleton ei) 
         u)
    es 
    CSet.empty in
    NS.fold (fun di u -> CSet.add (NS.singleton di, NS.empty) u) ds u1
      
let rec neg_cset cxs = CSet.fold 
  (fun cx cxs_neg -> isect_cset (neg_cvar cx) cxs_neg) 
  cxs
  truth_cs

and neg_state = function
    True -> False
  | False -> True
  | EmptyView -> NonEmptyView
  | NonEmptyView -> EmptyView
  | CS s -> let s = neg_cset s in 
      if CSet.is_empty s then False
      else CS(s)

(* --- intersection --- *)
and isect_cvar (es1,ds1) cxs = 
  CSet.fold 
    (fun (es2,ds2) u -> 
       let es = 
         (* remove T from es if (es1 U es2) <> 0 *)
         if NS.mem truth es1 then es2
         else if NS.mem truth es2 then es1
         else NS.union es1 es2 in
       let ds = NS.union ds1 ds2 in
         (* skip this element if (es & ds <> 0) *)
         if not (NS.is_empty (NS.inter es ds)) then u
         else CSet.add (es, ds) u)
    cxs
    CSet.empty
    
and isect_cset cxs1 cxs2 = CSet.fold 
  (fun cx i -> CSet.union (isect_cvar cx cxs2) i) cxs1 CSet.empty

let isect_state x0 x1 = match x0,x1 with
    True,x | x,True -> x
  | False,_ | _,False -> False
  | EmptyView,NonEmptyView | NonEmptyView,EmptyView -> False
  | EmptyView,s | s,EmptyView -> EmptyView
  | NonEmptyView,x | x,NonEmptyView -> x
  | CS s1, CS s2 -> 
      let s = isect_cset s1 s2 in 
        if CSet.is_empty s then False
        else CS(s)

let isect_elt e1 e2 = match e1,e2 with
    (Single n1,x1), (Single n2,x2) -> 
      if (n1=n2) then 
        let x = isect_state x1 x2 in
          if x = False then None else Some(Single n1, x)
      else None
  | (CoFinite cf1,x1), (CoFinite cf2,x2) -> 
      let x = isect_state x1 x2 in
        if x = False then None else Some(CoFinite (NS.union cf1 cf2), x)
  | (Single n,x1), (CoFinite cf,x2) 
  | (CoFinite cf,x2), (Single n,x1) -> 
      if NS.mem n cf then None
      else let x = isect_state x1 x2 in
        if x = False then None else Some(Single n, x)
          
(* --- union --- *)
let union_cset = CSet.union

let union_state x0 x1 = match x0,x1 with
    True,x | x,True -> True
  | False,x | x,False -> x
  | EmptyView,EmptyView -> EmptyView
  | NonEmptyView, NonEmptyView -> NonEmptyView 
  | EmptyView,NonEmptyView | NonEmptyView,EmptyView -> True
  (* might be able to do better here... *)
  | EmptyView,CS s | CS s,EmptyView       -> CS(union_cset empty_cs s)
  | NonEmptyView,CS s | CS s,NonEmptyView -> CS(union_cset non_empty_cs s)
  | CS s1, CS s2                          -> CS(union_cset s1 s2)
            
(* ----- mutually recursive definitions of lookups/refinements/constructors ------ *)

(* lookup_cvar: if we've already expanded this cvar, return it;
   otherwise, calculate the expansion and add it to delta *)
let rec lookup_cvar cx = match CSetEnv.lookup (!delta) (cset_of_cvar cx) with 
    Some t -> Some t
  | None -> 
      let (is,ds) = cx in
        map_opto (lookup_tvars is)
          (fun tis -> map_opt (lookup_tvars ds)
             (fun tds -> 
                let t = mk_diff (mk_isect tis) (mk_union tds) in 
                  update_cvar cx t;
                  t))
          
and lookup_cvars cxs = lookup_vars CSet.fold lookup_cvar cxs

(* lookup_cset: if we've already expanded this cset, return it;
   otherwise, calculate the expansion and add it to delta *)
and lookup_cset cxs = match CSetEnv.lookup (!delta) cxs with
    Some t -> Some t
  | None -> map_opt
      (lookup_cvars cxs)
        (fun ts -> 
           let t = mk_union ts in 
             update_cset cxs t;
             t)
        
and lookup_cset_required cxs = match lookup_cset cxs with 
    Some t -> t
  | None -> raise 
      (Error.Harmony_error 
         (fun () -> Format.printf 
            "Schema.lookup_cset_required: %s not found"
            (string_of_cset cxs))) 
        
(* expopse the formula and basis of a schema *)
and expose = function
    V(x) -> begin match x with 
        True         -> truth_formula, truth_basis
      | False        -> P.mkNot truth_formula, truth_basis
      | EmptyView    -> empty_formula, empty_basis
      | NonEmptyView -> non_empty_formula, empty_basis
      | CS s         -> expose (lookup_cset_required s)
    end
  | F(pi,ei,_) -> (pi,ei)

(* refine two bases: returns a pair of substitutions for formulas
   written over [e1] and [e2] and a common basis *)
and refine_bases e1 e2 = 
  (* compute 
   *   - new basis by finding non-empty intersecting elements, 
   *   - mappings of old variables to new ones *)
  let _,(vmap1,vmap2),common_basis_rev = Safelist.fold_left 
    (fun ((pos, i), vmaps, acc1) e1i ->
       let ((pos',_),vmaps',acc') = Safelist.fold_left 
         (fun ((pos,j),(vmap1,vmap2),acc2) e2j -> 
            match isect_elt e1i e2j with
                Some e ->
                  let new_vmaps = 
                    IM.add i (pos::IM.safe_find i vmap1 []) vmap1,
                    IM.add j (pos::IM.safe_find j vmap2 []) vmap2 in
                    ((pos+1,j+1),new_vmaps,e::acc2)
              | None -> ((pos,j+1),(vmap1,vmap2),acc2))
         ((pos,0),vmaps,acc1) e2
       in ((pos',i+1),vmaps',acc'))
    ((0,0),(IM.empty,IM.empty),[])
    e1 in
    
  (* mk_sum: make a sum of variables from a list of ints *)
  let mk_sum l = 
    let rec aux acc = function
        [] -> acc
      | sh::st -> aux (P.mkSum (P.mkVar sh) acc) st in
      aux P.zero l in
    
  let p1_subst = Misc.map_index_filter
    (fun i _ -> Some (i, mk_sum (IM.safe_find i vmap1 []))) e1 in
  let p2_subst = Misc.map_index_filter
    (fun j _ -> Some (j, mk_sum (IM.safe_find j vmap2 []))) e2 in
    
    (* refine_bases result: new instantiations, new basis *)
    ((p1_subst, p2_subst), Safelist.rev common_basis_rev) 

(* refine main loop is a fold of refine_bases over the elements of ts.
 *
 * at each step we compute a pair of instantiations and a new common
 * basis.     
 * 
 * rather than instantiate the formulas at each step, we simply
 * update the previously-computed instantiations (hoping that this
 * will be faster than instantiating the whole formula) 
 *)
and refine ts = 
  let ps_is_rev,common_basis = Safelist.fold_left
    (fun (ps_is_rev,e) ti -> 
       let pi,ei = expose ti in
       let ((subst_ps,subst_pi),common_basis) = refine_bases e ei in
       let upd_sub s1 s2 = Safelist.map (fun (n,e) -> (n,P.substitute_exp s1 e)) s2 in
       let new_ps_is_rev = Safelist.map (fun (pj,sj) -> (pj, upd_sub subst_ps sj)) ps_is_rev in
         ((pi,subst_pi)::new_ps_is_rev,common_basis))
    ([],[anyE]) 
    ts in
    
  (* rewrite each formula *)
  let ps = Safelist.rev_map (fun (pi,si) -> Presburger.substitute si pi) ps_is_rev in
    
    (* refine final result *)
    (ps, common_basis) 

and mk_neg t0 = match t0 with 
    V(x) -> V(neg_state x)
  | F(p,e,syn_t) -> F(P.mkNot p,e,Neg(syn_t))
      
and mk_diff t1 t2 = mk_isect [t1; mk_neg t2]
  
(* gen_mk: a generalized constructor for unions, intersections,
 * concatenations
 * mk_ps: a function to make the top-level Presburger formula from a
 *        list of formulas
 * cset_from_vars_opt: an optional function to make a cset if the ts
 *                     are all vars
 * ts: the list of schemas 
 *)
and gen_mk mk_ps mk_syns cset_from_vars_opt ts = 
  (* go () actually computes the type *)
  let go () = 
    let ps,e = refine ts in
    let p = mk_ps ps in
      F(p,e,mk_syns (Safelist.map syn_of_t ts)) in
    
  (* first, calculate the marked vars in ts *)
  let marked_xs = marked_ts ts in
    (* cases 1: if no ts marked *)
    if NS.is_empty marked_xs then     
      match cset_from_vars_opt, (Safelist.for_all is_var ts) with
          (* case 1a: and  all ts are vars and cset_from_vars_opt is Some(f) 
           *   then we make a variable directly *)
          Some f, true -> V (f ts)
          (* case 1b: some ts are not vars and/or cset_from_vars_opt is None 
           *          then we use go () to make the type *)
        | _ -> go ()
            
    (* case 2: if some ts have marked vars, add go to delayed work *)
    else 
      begin let x = fresh "_GEN" in
        add_delayed x (fun () -> update_tvar x (go ()));
        V(cstate_of_tvar x)
      end

and mk_isect = function
    (* knock off a few easy cases *)
    [] -> V(True)
  | [ti] -> ti
  | ts -> 
      gen_mk
        P.mkAnd 
        (fun ss -> Isect(ss))
        (Some (fun vs -> Safelist.fold_left 
                 (fun acc -> function 
                      V(xi) -> isect_state acc xi | 
                          _ -> assert false) True vs)) 
        ts
  
and mk_union = function
    (* knock off a few easy cases *)
    [] -> V(False)
  | [ti] -> ti
  | ts -> 
      gen_mk 
        P.mkOr
        (fun ss -> Union(ss))
        (Some (fun vs -> Safelist.fold_left 
                 (fun acc -> function 
                      V(xi) -> union_state acc xi 
                    | _ -> assert false) False vs))
        ts
        
(* ------------ simple constructors -------------*)
        
let mk_any = V(True)

let mk_var x = V(cstate_of_tvar x)
    
let mk_atom n t = 
  let x = state_of_t t in 
  let syn = Atom(n,syn_of_t t) in
  let (ps,e) = 
    if x = True then
      [P.mkEq (P.mkVar 0) P.one;
       P.mkEq (P.mkVar 1) P.zero],
      [(Single n,True);
       (CoFinite (NS.singleton n), True)]
    else
      [P.mkEq (P.mkVar 0) P.one;
       P.mkEq (P.mkVar 1) P.zero;
       P.mkEq (P.mkVar 2) P.zero],
    [(Single n,x);
     (Single n, neg_state x);
     (CoFinite (NS.singleton n), True)] in
    F(P.mkAnd ps,e,syn) 

let mk_wild f l u t = 
  let x = state_of_t t in   
  let syn = Wild(f,l,u,syn_of_t t) in
  let cof_pos,fin_ps_rev,fin_es_rev = NS.fold 
    (fun ni (pos,ps,es) -> 
       (pos+1,
        (P.mkEq (P.mkVar pos) P.zero)::ps,
        (Single ni,True)::es))
    f
    (0,[],[]) in
  let e = Safelist.rev ((CoFinite f, neg_state x)::(CoFinite f, x)::fin_es_rev) in
  let p = P.mkAnd 
    (Safelist.rev 
       ((P.mkEq (P.mkVar (cof_pos + 1)) P.zero)
        :: (if u then P.mkGe (P.mkVar cof_pos) (P.mkConst l)
            else P.mkEq (P.mkVar cof_pos) (P.mkConst l))
       :: fin_ps_rev)) in
    F(p,e,syn)

let rec get_atom t0 = match t0 with
    V(x) -> begin match x with 
        CS s ->       
          if NS.is_empty (marked_ts [t0]) 
          then get_atom (lookup_cset_required s)
          else None
      | _ -> None
    end
  | F(p,e,_) -> begin
      match e,P.get_atom p with 
          [(Single n,True);(CoFinite f,True)],Some g ->            
            if NS.cardinal f=1 && NS.choose f=n
            then Some(n,g,True,False)
            else None
        | [(Single n,x);(Single m,y);(CoFinite f,ctruth)],Some g -> 
            if n=m &&NS.cardinal f=1 && NS.choose f=n then 
              match neg_state x,y with
                  True,True                    
                | False,False 
                | EmptyView,EmptyView 
                | NonEmptyView,NonEmptyView -> Some(n,g,x,y)
                | CS s1, CS s2              -> if CSet.equal s1 s2 then Some (n,g,x,y) else None
                | _ -> None (* might have a few more cases here *)
            else None
        | _ -> None
    end

(* mk_cat_fast: make a concatenation from a list of atoms *)
let mk_cat_fast atms = 
  if atms = [] then V(EmptyView)
  else 
    let rec aux (pos,fs,pacc,eacc,sacc) = function 
        [] -> 
          Safelist.rev ((P.mkEq (P.mkVar pos) P.zero)::pacc), 
          Safelist.rev ((CoFinite fs,True)::eacc),
          Safelist.rev sacc
      | (n,g,x,y)::rest -> 
          let pos',fs',pacc',eacc' = 
            match x,y with
                True,False -> 
                  (pos+1,
                   NS.add n fs,
                   P.substitute [(0,P.mkVar pos)] g::pacc,
                   (Single n,x)::eacc)
              | _ -> 
                  (pos+2,
                   NS.add n fs,
                   (P.mkEq (P.mkVar (pos+1)) P.zero)
                   ::(P.substitute [0,P.mkVar pos] g)
                   ::pacc,
                   (Single n,y)::(Single n,x)::eacc) in
          let sacc' = Atom(n,Var(string_of_state x))::sacc in
            aux (pos',fs',pacc',eacc',sacc') rest in      
    let p,e,syns = aux (0,NS.empty,[],[],[]) atms in
      F(P.mkAnd p,e,Cat(syns))

(* general mk_cat *)
let rec mk_cat ts = 
  let _,atms,natms = Safelist.fold_left 
    (fun (fs,aacc,naacc) ti -> 
       match get_atom ti with 
           None -> (fs,aacc,ti::naacc)
         | Some a ->
             let (n,_,_,_) = a in
               if not (NS.mem n fs) then (NS.add n fs, a::aacc,naacc) 
               else (fs,aacc,ti::naacc))
    (NS.empty,[],[])
    ts in
    match atms,natms with 
        [],[] -> V(EmptyView)
      | [],_  -> gen_mk P.add (fun ts -> Cat(ts)) None (Safelist.rev natms)
      | _,[]  -> mk_cat_fast (Safelist.rev atms)
      | _     -> gen_mk P.add (fun ts -> Cat(ts)) None (mk_cat_fast atms::(natms))
            
(* list schemas *)
let mk_nil = mk_cat [mk_atom V.nil_tag (mk_cat [])]
let mk_cons h t = mk_cat [mk_atom V.hd_tag h; mk_atom  V.tl_tag t]
  
(* [schema_of_tree v] yields the singleton [Value.ty] containing [v] *)
let rec t_of_tree v = 
  mk_cat
    (V.fold (fun k vk ts -> (mk_atom k (t_of_tree vk))::ts) v [])

(* --------------- operations ----------------------*)

(* --- empty test --- *)

(* cached emptiness results *)
let (empties,non_empties) = (ref TSet.empty, ref TSet.empty)
  
let rec empty_aux (es,nes) t0 = 
  if TSet.mem t0 es then (true,es,nes)
  else if TSet.mem t0 nes then (false,es,nes)
  else 
    let t0_empty,es',nes' = match t0 with
        V(x) -> begin match x with
            True | EmptyView | NonEmptyView -> (false,es,nes)
          | False -> (true,es,nes)
          | CS s  -> empty_aux (TSet.add t0 es,nes) (lookup_cset_required s)
        end 
      | F(p,e,_) -> check_empty (es,nes) (p,e) in
      if t0_empty then (true,TSet.add t0 es',nes')
      else (false,es,TSet.add t0 nes)        

and check_empty (es,nes) (p,e) = 
  (* construct a sum of vars and set of names mentioned in finite
     elts, and all the sub_formulas *)
  let collect es = 
    let rec aux (pos, nmap, facc) = function 
        [] -> (pos, nmap, Safelist.rev facc)
      | (Single n,x)::rest -> 
          let old_sum = NM.safe_find n nmap (P.mkConst 0) in 
          let new_nmap = NM.add n (P.mkSum (P.mkVar pos) old_sum) nmap in
            aux (pos+1, new_nmap, x::facc) rest
      | (CoFinite _,x)::rest -> 
          aux (pos+1, nmap, x::facc) rest in
    let _,nmap,facc = aux (0,NM.empty,[]) es in 
      (nmap,facc) in
    
  let nmap, sub_formulas = collect e in 
    
  (* add constraint that these are feature trees *)
  let ft_constraints = NM.fold 
    (fun _ sum acc -> P.mkLe sum P.one::acc)
    nmap [] in

  (* valuation representing elements that must be zero *)
  let z_val = P.Valuation.create (Safelist.length sub_formulas) 0 true in
    
  (* determine which are empty/non-empty *)
  let rec empties (es,nes) pos = function
      []   -> (es,nes)
    | x::xrest -> 
        let fake_t0 = V(x) in
        let (b,es',nes') = empty_aux (es,nes) fake_t0 in
          if b then begin 
            P.Valuation.zonk z_val pos;
            empties (TSet.add fake_t0 es', nes') (pos+1) xrest
          end
          else 
            empties (es,TSet.add fake_t0 nes') (pos+1) xrest in
    
  let es',nes' = empties (es,nes) 0 sub_formulas in
    (* check satisfiability of formula ft, zero constraints *)
    (not (P.fast_sat (P.mkAnd (p::ft_constraints)) z_val),es',nes')
      
let empty t0 = 
  let t0_empty,new_empties,new_non_empties = 
    empty_aux (!empties,!non_empties) t0 in
    empties := new_empties;
    non_empties := new_non_empties;
    t0_empty

let subschema t0 t1 = empty (mk_diff t0 t1)

let equivalent t0 t1 = subschema t0 t1 && subschema t1 t0
    
(* --- member --- *)
let rec member v t0 = match t0 with
    V(x) -> begin match x with 
        True -> true
      | False -> false
      | EmptyView -> V.is_empty v
      | NonEmptyView -> not (V.is_empty v)
      | CS s -> member v (lookup_cset_required s) 
    end
  | F(p,e,_) -> 
      (* count the basis elts that the elts of v belongs to *)
      (* this code is critical. we use an Array.t for speed *)
      let nbits,nints = Safelist.fold_left 
        (fun (b,i) ei -> 
           if i>0 then (b,succ i)
           else match ei with 
               (Single(_),_) -> (succ b, i)
             | _         -> (b,succ i))
        (0,0) 
        e in
      let counts = P.Valuation.create nbits nints false in
        V.fold
          (fun ni vni () ->
             let rec count_elts pos = function
                 [] -> ()
               | h::t -> 
                   let yes () = P.Valuation.bump counts pos in
                   let no () = count_elts (pos+1) t in                     
                   let on_match = function
                       EmptyView    -> 
                         if V.is_empty vni then yes () 
                         else no ()
                     | True         -> yes ()
                     | False        -> no ()
                     | NonEmptyView -> 
                         if V.is_empty vni then no () 
                         else yes ()
                     | CS s         -> 
                         if member vni (lookup_cset_required s) 
                         then yes ()
                         else no () 
                   in
                     (match h with 
                          Single n,x -> 
                            if (n=ni) then on_match x 
                            else no ()
                        | CoFinite f,x -> 
                            if NS.mem ni f then no () 
                            else on_match x)
             in count_elts 0 e)
          v ();

        (* final result: check if the formula is satisfiable *)
        let res = P.fast_sat p counts in
          (* Format.printf "MEMBER"; Format.print_newline ();
             Format.printf " v : "; V.format_t v; Format.print_newline();
             Format.printf "t0 : "; format_t t0; Format.print_newline();
             Format.printf "  = %b" res;
             Format.print_newline (); *)
          res


(* --- dom_member --- *)            
let rec dom_member ns t0 = 
  let res = match t0 with
    V(x) -> begin match x with 
        True -> true
      | False -> false
      | EmptyView -> NS.is_empty ns
      | NonEmptyView -> not (NS.is_empty ns)
      | CS s -> dom_member ns (lookup_cset_required s)
    end
  | F(p,e,_) -> 
      let (new_vars,nmap,vmap) = NS.fold
        (fun k (new_vars,nmap,vmap) -> 
           let _,new_vars,nmap,vmap = Safelist.fold_left 
             (fun (pos,new_vars,nmap,vmap) ei -> 
                let yes () = 
                  let nmap' = NM.add k (new_vars::(NM.safe_find k nmap [])) nmap in
                  let vmap' = IM.add pos (new_vars::(IM.safe_find pos vmap [])) vmap in
                    (pos+1, new_vars+1, nmap', vmap') in
                let no () = (pos+1, new_vars, nmap, vmap) in
                let on_match = function
                    True | EmptyView | NonEmptyView -> yes ()
                  | False -> no ()
                  | CS s  -> if empty (lookup_cset_required s) then no () else yes () in
                  match ei with
                      (Single n,x) -> if k=n then on_match x else no ()
                    | (CoFinite f,x) -> if NS.mem k f then no () else on_match x)
             (0,new_vars,nmap,vmap) 
             e in
             (* result of Safelist.fold_left *)
             (new_vars,nmap,vmap))
        ns 
        (0,NM.empty,IM.empty)in
      let rec aux_sum acc = function [] -> acc | h::t -> aux_sum (P.mkSum (P.mkVar h) acc) t in
        
      let f1 = NM.fold (fun _ xl acc -> (P.mkEq (aux_sum P.zero xl) P.one)::acc) nmap []in
      let subst = IM.fold (fun x xl acc -> (x+new_vars, aux_sum P.zero xl)::acc) vmap [] in
      let formula = P.mkAnd (P.substitute subst (P.shift_t new_vars p)::f1) in
        P.fast_sat formula (P.Valuation.create ((Safelist.length e) + new_vars) 0 true)
  in
    (* Format.printf "DOM_MEMBER {%s}" (ns2str "," ns);
    format_t t0;
    Format.printf " = %b" res;
    Format.print_newline (); *)
    res

(* --- project --- *)
let rec project k t0 = match t0 with
    V(True) | V(NonEmptyView) -> Some(V(True))
  | V(False) | V(EmptyView)   -> None
  | V(CS(s)) -> project k (lookup_cset_required s)
  | F(p,e,_)   ->           
      let e_length = Safelist.length e in
      let eks = Misc.map_index_filter
        (fun pos ei -> match ei with
             (Single n,x) -> 
               if n=k then 
                 let pi_val = P.Valuation.create e_length 0 true in 
                   P.Valuation.bump pi_val pos;
                   if P.fast_sat p pi_val then
                     match x with 
                         True | EmptyView | NonEmptyView | CS _ -> Some (V x)
                       | False -> None
                   else None
               else None
           | (CoFinite f,x) -> 
               if (not (NS.mem k f)) then
                 let pi_val = P.Valuation.create e_length 0 true in
                   P.Valuation.bump pi_val pos;
                   if P.fast_sat p pi_val then match x with 
                       True | EmptyView | NonEmptyView | CS _ -> Some (V x)
                     | False -> None
                   else None
               else None)
        e in
      let t = mk_union eks in 
        if empty t then None 
        else Some t 
  
let rec project_all t0 = match t0 with 
    V(True) | V(NonEmptyView) -> Some (V(True))
  | V(False) | V(EmptyView) -> Some (V(False))
  | V(CS(s)) -> project_all (lookup_cset_required s)
  | F(p,e,_) -> 
      let _,ok,reso = Safelist.fold_left
        (fun (pos,ok,reso) (_,x) -> 
           if not ok then (pos,ok,reso) 
           else let pi_val = P.Valuation.create (pos+1) 0 true in 
             P.Valuation.bump pi_val pos;             
             if not (P.fast_sat p pi_val) then (pos+1,ok,reso)
             else match reso with 
                 None   -> (pos+1,true,Some x)
               | Some y -> 
                   if equivalent (V(x)) (V(y)) then (pos+1,true,Some y)
                   else (pos+1,false,None))
        (0,true,None)
        e in
        if ok then map_opt reso (fun x -> V(x)) else None

let inject t0 t1 = 
  let syn0,syn1 = syn_of_t t0, syn_of_t t1 in
  let fds_union fds1 fds2 = 
    match fds1,fds2 with
        (true,ns1),(true,ns2) -> 
          (true,NS.union ns1 ns2)
      | (true,ns1),(false,ns2) | (false,ns2),(true,ns1) -> 
          (false,NS.diff ns2 ns1)
      | (false,ns1),(false,ns2) -> 
          (false,NS.inter ns1 ns2) in
  let loc2fds = function 
      Single(n) -> (true,NS.singleton n) 
    | CoFinite(ns) -> (false,ns) in
  let p,e = expose t0 in 
  let x_t1 = state_of_t t1 in
  let x_neg_t1 = neg_state x_t1 in 
  let _,x_count,subst,formulas,basis,covered = Safelist.fold_left
    (fun acc (l,x) -> 
       let pos,x_pos,subst,formulas,basis,covered = acc in
       let pi_val = P.Valuation.create (pos+1) 0 true in 
         P.Valuation.bump pi_val pos;
         if not (P.fast_sat p pi_val) then (pos+1,x_pos,(pos,P.zero)::subst,formulas,basis,covered)
         else 
           let covered' = fds_union covered (loc2fds l) in
             match x_neg_t1 with
                 (* optimization, keeps bases smaller *)
                 False -> 
                   let pos' = pos+1 in 
                   let x_pos' = x_pos+1 in 
                   let subst' = (pos,P.mkVar x_pos)::subst in 
                   let basis' = (l,x_t1)::basis in 
                     (pos',x_pos',subst',formulas,basis',covered')
               | _ -> 
                   let pos',x_pos'=pos+1,x_pos+2 in
                   let subst' = (pos,P.mkVar x_pos)::subst in
                   let formulas' = P.mkEq (P.mkVar (x_pos+1)) P.zero::formulas in
                   let basis' = (l,x_t1)::(l,x_neg_t1)::basis in
                     (pos',x_pos',subst',formulas',basis',covered')) 
    (0,0,[],[],[],(true,NS.empty))
    e in
    
  (* add some zero-d elements to make this a basis *)
  let formulas,basis = match covered with
      (false,ns) -> 
        let _,formulas,basis = NS.fold
          (fun ni (x_pos,formulas,basis) -> 
             (x_pos+1,P.mkEq (P.mkVar x_pos) P.zero::formulas,
              (Single(ni),True)::basis))
          ns
          (x_count,formulas,basis) in 
          formulas,basis
    | (true,ns) -> 
        if NS.is_empty ns then formulas,basis
        else (P.mkEq (P.mkVar x_count) P.zero::formulas, 
              (CoFinite(ns),True)::basis) in
  let res_p = P.mkAnd ((P.substitute subst p)::formulas) in
    F(res_p,basis,Inject(syn0,syn1))
      
let restrict ns t0 = 
  let restrict_aux (p,e) syn = 
    (* first, rewrite p and e so that no ns intersect a CoFinite element of e 
     * we can use refine to do this *)
    let ns_e = NS.fold (fun ni acc -> (Single(ni),True)::acc) ns [CoFinite(ns),True] in
    let (_,subst),e' = refine_bases ns_e e in
    let _,ns_cnstr,neg_ns_cnstr = Safelist.fold_left 
      (fun (pos,ns_cnstr,neg_ns_cnstr) ei -> 
         let eq_z = P.mkEq (P.mkVar pos) P.zero in
           match ei with
               Single(n),_ ->              
                 if NS.mem n ns then (pos+1,ns_cnstr,eq_z::neg_ns_cnstr)
                 else (pos+1,eq_z::ns_cnstr,neg_ns_cnstr)
             | CoFinite(_),_ -> 
                 (* because we refined, we know that ns is excluded
                    from ei *)
                 (pos,eq_z::ns_cnstr,neg_ns_cnstr))
      (0,[],[]) e' in
    let p' = P.substitute subst p in
    let restr_ns = F(P.mkAnd (p'::ns_cnstr),e',Restrict(syn,ns,true)) in
    let restr_neg_ns = F(P.mkAnd (p'::neg_ns_cnstr),e',Restrict(syn,ns,false)) in
      (restr_ns, restr_neg_ns) in
    match t0 with 
        V(x) -> begin match x with 
            False | EmptyView -> (V(x),V(x))
          | _ -> restrict_aux (expose t0) (syn_of_t t0)
        end
      | F(p,e,syn) -> restrict_aux (p,e) syn
               
(* WARNING: this may be hideously slow *)
let mk_spine_cons t0 t1 = 
  let res = 
    (* must have t0 <= ![T] *)
    let skinny = F(P.mkEq (P.mkVar 0) P.one,[anyE],Wild(Name.Set.empty,1,false,Var(truth))) in
      if not (subschema t0 skinny) then 
        raise (Error.Harmony_error
                 (fun () -> 
                    Format.printf "non keyed values allowed by schema@,@[";
                    format_t t0;
                    Format.printf "@]")); 
      let (p,es) = expose t0 in
        
    (* collect up all the elements that can be projected on a single name *)
    let single_es = 
      let all_ns = Misc.map_index_filter (fun n _ -> Some n) es in
      let mk_sum_skip n = 
        let rec aux acc = function 
            [] -> acc 
          | h::t -> if h = n then acc else P.mkSum (P.mkVar h) acc in 
          aux P.zero in
        Misc.map_index_filter
          (fun ni ei ->
             let zeros = P.mkEq (mk_sum_skip ni all_ns) P.zero in
             let one = P.mkEq (P.mkVar ni) P.one in
             let f = P.mkAnd [one;zeros;p] in
               if P.satisfiable f then Some ei else None)
          es in
    let (is_finite,names,ux) = 
      Safelist.fold_left 
        (fun (fin,names,ux) ei -> match fin,ei with
             true,(Single n,x) -> (true,NS.add n names,union_state ux x)
           | true,(CoFinite f,x) -> (false,NS.diff f names,union_state ux x)
           | false,(Single n,x) -> (false,NS.remove n names,union_state ux x)
           | false,(CoFinite f,x) -> (false,NS.inter f names, union_state ux x))
        (true,NS.empty,False)
        single_es in
    let t1' = 
      if is_finite then mk_union 
        (NS.fold 
           (fun n acc -> (mk_atom n t1)::acc)
           names 
           [V(False)])
      else mk_wild names 1 false t1 in
    let t0' = mk_atom V.hd_tag (V(ux)) in
      mk_cat [t1';t0']
  in
    res

(* external exports *)
let update = update_tvar
