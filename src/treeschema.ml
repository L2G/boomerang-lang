(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* schema.ml - representation and functions on types         *)
(*             based on "A Logic You Can Count On", POPL '04 *)
(*************************************************************)
(* $Id$ *)

(* -------------- imports / abbreviations / instantiations -------------- *)
let (@) = Safelist.append
let sprintf = Printf.sprintf
let debug = Trace.debug "schema" 
let fresh = Syntax.fresh

module NS = Name.Set
module NM = Name.Map 
module P = Presburger
module IM = Int.Map
module IS = Int.Set
module ISS = Int.SetSet

(* ---- regular expression module ---- *)
module type SRegExp = sig 
  type t 
  val mk_finite : Name.t list -> t
  val mk_cofinite : Name.t list -> t
  val format_t : t -> unit
  val isect : t -> t -> t option
  val union : t -> t -> t
  val finite_domain : t -> Name.Set.t option
  val cofinite_domain : t -> Name.Set.t option
  val is_singleton : t -> bool
  val is_finite : t -> bool
  val singleton_domain : t -> Name.t option
  val mem : Name.t -> t -> bool
end

module RegExp : SRegExp = struct
  type t = Finite of Name.Set.t | CoFinite of Name.Set.t
  let format_t = function 
      Finite(ns)    -> Util.format "@[{%s}@]" 
        (Misc.concat_list "," (Safelist.map Misc.whack (NS.elements ns)))
    | (CoFinite ns) -> Util.format "@[~{%s}@]" 
        (Misc.concat_list "," (Safelist.map Misc.whack (NS.elements ns)))
  let mk_singleton n = 
    Finite(Name.Set.singleton n)
  let mk_finite l = 
    let ns = Safelist.fold_left
      (fun acc ni -> NS.add ni acc)
      NS.empty l in 
      Finite(ns)
  let mk_cofinite l = 
    let ns = Safelist.fold_left 
      (fun acc ni -> NS.add ni acc) 
      NS.empty l in 
      CoFinite(ns)
  let union r1 r2 = match r1,r2 with
      (Finite ns1),(Finite ns2)     -> Finite(NS.union ns1 ns2)
    | (CoFinite ns1),(CoFinite ns2) -> CoFinite(NS.inter ns1 ns2)
    | (Finite nsf),(CoFinite nscf)
    | (CoFinite nscf),(Finite nsf)  -> CoFinite(NS.diff nscf nsf)
  let isect r1 r2 = match r1,r2 with
      (Finite ns1),(Finite ns2) ->
        let ns = NS.inter ns1 ns2 in 
          if NS.is_empty ns then None
          else Some(Finite ns)
    | (CoFinite ns1),(CoFinite ns2) -> 
        Some(CoFinite (NS.union ns1 ns2))
    | (Finite nsf), (CoFinite nscf)
    | (CoFinite nscf), (Finite nsf) -> 
        let ns = NS.diff nsf nscf in
          if NS.is_empty ns then None
          else Some(Finite ns)
        
  let finite_domain = function
      Finite ns -> Some (ns)
    | _ -> None
  let cofinite_domain = function
      CoFinite ns -> Some ns
    | _ -> None
  let is_singleton = function 
      Finite ns -> NS.cardinal ns = 1 
    | _ -> false
  let is_finite = function
      Finite _ -> true
    | _ -> false
  let singleton_domain = function
      Finite ns -> if NS.cardinal ns = 1 then Some(NS.choose ns) else None
    | _ -> None
  let mem n = function 
      Finite ns   -> NS.mem n ns
    | CoFinite ns -> not (NS.mem n ns)
end 
module R = RegExp

(* --- helper functions ---- *)  
let map_opt o f = match o with
    None -> None
  | Some a -> Some (f a)

let map_opto o f = match o with
    None -> None
  | Some a -> (f a)

let zero_of_var xi = P.mkEq P.zero (P.mkVar xi) 
let one_of_var xi = P.mkEq P.one (P.mkVar xi) 
let sum_of_int_list l = P.mkSum (Safelist.map P.mkVar l)
let sum_of_int_set s = P.mkSum (Int.Set.fold (fun ei a -> P.mkVar ei::a) s [])
let sum_of_int_state_list l = P.mkSum (Safelist.map (fun (xi,_) -> P.mkVar xi) l) 
let ns_of_n_list nl = Safelist.fold_left (fun a ni -> NS.add ni a) NS.empty nl

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

(* a CSet.t is a finite union of cvars *)
module CSet = Set.Make(
  struct 
    type t = cvar
    let compare (is1,ds1) (is2,ds2) = 
      let cmp = NS.compare is1 is2 in
        if cmp <> 0 then cmp 
        else NS.compare ds1 ds2
  end)

(* -------------------------- representation ------------------------- *)
(* automaton states either one of several common constants, or a CSet.t *)
type state = True | False | EmptyView | NonEmptyView | CS of CSet.t

let compare_state s1 s2 = match s1,s2 with
    True,True | False,False | EmptyView,EmptyView | NonEmptyView,NonEmptyView -> 0
  | CS x1, CS x2 -> CSet.compare x1 x2
  | True,_ -> -1
  | _,True -> 1
  | False,_ -> -1
  | _,False -> -1
  | EmptyView,_ -> -1
  | _, EmptyView -> 1
  | NonEmptyView,_ -> -1
  | _,NonEmptyView -> 1

(* sets of states, int * states *)
module StateSet = Set.Make(
  struct
    type t = state
    let compare = compare_state 
  end)
module SS = StateSet

module IntStateSet = Set.Make(
  struct
    type t = int * state
    let compare (i1,s1) (i2,s2) = 
      let cmp1 = compare i1 i2 in 
        if cmp1 <> 0 then cmp1 
        else compare_state s1 s2
  end)
module IStS = IntStateSet      

(* an element is a pair of a location and a state *)
type elt = R.t * state

(* the datatype for schemas is either a variable given by a state, or
   a counting formula over a proper basis *)
type u = V of state | F of (Presburger.t * elt list) 

(* syntactic reprsentations of schemas--used for pretty-printing*)
type syn_t = 
    Var of state 
    | AtomCats of bool * Name.Set.t * syn_t 
    | AtomAlts of Name.Set.t * syn_t 
    | Wild of Name.Set.t * int * bool * syn_t
    | Cat of syn_t list
    | Union of syn_t list
    | Isect of syn_t list
    | Neg of syn_t
    | Restrict of syn_t * Name.Set.t * bool
    | Inject of syn_t * syn_t
    | InjectMap of syn_t * (syn_t Name.Map.t)
        
(* counterexamples returned from member tests *)
type counterexample = 
    Domain of Name.Set.t 
  | Child of Name.t * Tree.t
                          
(* result of a single step of a member test *)
type member_step = 
    StepYes 
  | StepNo of counterexample 
  | StepMaybe of Name.t * (Tree.t -> member_step)
  
(* final result of a member test *)
type member_result = Yes | No of counterexample

type t = { def : u; 
           hash : int; 
           syn : syn_t; 
           first_step : Name.Set.t -> member_step; 
           member : Tree.t -> member_result }

let hash t0 = t0.hash

let t_hash_mix = ref 0

let compare_t t0 t1 = match t0.def,t1.def with
    V(s1),V(s2)     -> compare s1 s2
  | F(f1,es1),F(f2,es2) -> 
      let cmp1 = P.compare_t f1 f2 in 
        if cmp1 <> 0 then cmp1
        else Misc.dict_cmp compare es1 es2
  | V(_),F(_)       -> -1
  | F(_),V(_)       -> 1

(* --------------- conversions / simple operations --------------- *)
let cset_of_cvar cx = CSet.singleton cx
let cset_of_tvar x = cset_of_cvar (NS.singleton x, NS.empty)
let cstate_of_tvar x = CS (cset_of_tvar x)
let is_cvar s = CSet.cardinal s = 1
let is_tvar s = (is_cvar s) && 
  let (is,ds) = CSet.choose s in
    (NS.cardinal is = 1) && (NS.is_empty ds)
let is_var t0 = match t0.def with V _ -> true | _ -> false

let syn_of_t t0 = t0.syn

(* ----- primitive type environment ------ *)
module TVarEnv = Env.Make(
  struct
    type t = tvar
    let compare = Pervasives.compare
    let to_string x = x
  end)

let delta_tvar : (t TVarEnv.t ref) = ref (TVarEnv.empty ())
let update_tvar x t = delta_tvar := (TVarEnv.update (!delta_tvar) x t)
let lookup_tvar x = TVarEnv.lookup (!delta_tvar) x
let lookup_tvar_required x = match lookup_tvar x with 
    Some t -> t
  | None -> raise (Error.Harmony_error 
                     (fun () -> Util.format 
                        "Treeschema.lookup_tvar_required: %s not found" x))

(* --------------- formatting --------------- *)
let string_of_cvar (is,ds) = 
  let ns2str sep s = Misc.concat_list sep (NS.elements s) in    
    sprintf "%s%s" 
      (if NS.is_empty is then "0" else 
         if NS.cardinal is = 1 then (NS.choose is) 
         else (ns2str "&" is))
      (if NS.is_empty ds then "" else 
         if NS.cardinal ds = 1 then 
           sprintf "-%s" (NS.choose ds)
         else
           sprintf "-(%s)" (ns2str "|" ds))

let string_of_cset cxs = 
  if CSet.is_empty cxs then "{}" else
    Misc.concat_list "|" (Safelist.map string_of_cvar (CSet.elements cxs))

let string_of_state s0 = match s0 with 
    True         -> "T"
  | False        -> "F"
  | EmptyView    -> "{}"
  | NonEmptyView -> "T-{}"
  | CS s         -> string_of_cset s

let format_state s0 = Util.format "%s" (string_of_state s0)

let format_elt (r,x) = 
  R.format_t r;
  Util.format "[";
  format_state x;
  Util.format "]"

(* pretty printing comes here; it only needs to know about tvars, but we need string_of_t 
to instantiate Mapplus functor for full environments below *)
type format_mode = FCat | FSimpleCat | FUnion | FInter | FNone

let rec format_one_syn mode = function
    Var(x) -> format_state x
  | AtomCats(opt,ns,t) -> 
      let c = Name.Set.cardinal ns in 
      Util.format "@[";
      if mode <> FSimpleCat then Util.format "{"; 
      if opt then Util.format "?";
      if c <> 1 then Util.format "(";
      Misc.format_list "," (fun n -> Util.format "%s" (Misc.whack n)) (Name.Set.elements ns);
      if c <> 1 then Util.format ")";
      Util.format "=";
      format_one_syn FNone t;
      if mode <> FSimpleCat then Util.format "}";
      Util.format "@]"

  | AtomAlts(ns,t) -> 
      let c = Name.Set.cardinal ns in 
      Util.format "@[";
      if mode <> FSimpleCat then Util.format "{"; 
      if c <> 1 then Util.format "(";
      Misc.format_list "|" (fun n -> Util.format "%s" (Misc.whack n)) (Name.Set.elements ns);
      if c <> 1 then Util.format ")";
      Util.format "=";
      format_one_syn FNone t;
      if mode <> FSimpleCat then Util.format "}";
      Util.format "@]"

  | Wild(f,l,u,t) -> 
      Util.format "@[";
      if mode <> FSimpleCat then Util.format "{";         
      (match l,u with
          0,true -> Util.format "*"
        | n,b -> 
            for i=1 to n do Util.format "!" done; 
            if b then Util.format "*");
      if not (Name.Set.is_empty f) then
        (Util.format "~{@[";
         Misc.format_list ",@ " 
           (Util.format "%s") 
           (Safelist.map Misc.whack (Name.Set.elements f));
         Util.format "@]}");
      Util.format "=@,";
      format_one_syn FNone t;
      if mode <> FSimpleCat then Util.format "}"; 
      Util.format "@]" 
  | Cat(ts)    -> 
      let is_atoms = Safelist.for_all 
        (function AtomCats(_) | AtomAlts(_) | Wild(_) -> true | _ -> false) ts in 
        Util.format "@[";
        (match mode,is_atoms with 
            FSimpleCat, true | FCat,false -> ()
          | _,true -> Util.format "{"            
          | _ -> Util.format "(");
        (if is_atoms then Misc.format_list ",@ " 
          else Misc.format_list " +@ ") 
          (format_one_syn (if is_atoms then FSimpleCat else FCat)) ts;
        (match mode,is_atoms with 
            FSimpleCat, true | FCat,false -> ()
          | _,true -> Util.format "}"            
          | _ -> Util.format ")");            
        Util.format "@]"
  | Union(ts)  -> 
      Util.format "@[";
      if mode <> FUnion then Util.format "("; 
      Misc.format_list " |@ " (format_one_syn FUnion) ts;
      if mode <> FUnion then Util.format ")";
      Util.format "@]"
  | Isect(ts)  ->
      Util.format "@[";
      if mode <> FInter then Util.format "("; 
      Misc.format_list " &@ " (format_one_syn FInter) ts;
      if mode <> FInter then Util.format ")";
      Util.format "@]"
  | Neg(t)     -> 
      Util.format "~"; 
      format_one_syn FNone t;
  | Inject(t1,t2) -> 
      Util.format "Inject(";
      format_one_syn FNone t1;
      Util.format ",@ ";
      format_one_syn FNone t2;
      Util.format ")"
  | InjectMap(t1,tms) -> 
      Util.format "InjectMap(";
      format_one_syn FNone t1;
      Util.format ",@ @[{";
      let tms_size = NM.size tms in 
      let _ = NM.fold 
        (fun ni ti count -> 
          Util.format "%s -> " (Misc.whack ni);
          format_one_syn FNone ti;
          if count < tms_size - 1 then Util.format ",@ ";
          succ count)
        tms 0 in 
        Util.format "}@])"
  | Restrict(t,ns,b) -> 
      Util.format "Restrict(";
      format_one_syn FNone t;
      Util.format ",%s{%s})"
        (if b then "" else "~")
        (Misc.concat_list "," (Name.Set.elements ns)) 
  
let format_syn_t syn_t0 = 
  (* gather up all the tvars mentioned in a syn_t *)
  let rec collect acc = function
      [] -> acc
    | x::xs -> begin match x with 
          Var(CS s) -> 
            let s_tvars = CSet.fold 
              (fun (is,ds) acc -> NS.union is (NS.union ds acc)) 
              s NS.empty in
            let new_vars = NS.diff s_tvars (NM.domain acc) in 
            let new_acc,new_xs = NS.fold 
              (fun tvi (new_acc,new_xs) -> 
                 let syn_tvi = syn_of_t (lookup_tvar_required tvi) in 
                   (NM.add tvi syn_tvi new_acc, syn_tvi::new_xs))
              new_vars (acc,xs) in
              collect new_acc new_xs

        | Var(_) -> collect acc xs

        | Cat(ts) | Union(ts) | Isect(ts) -> collect acc (ts@xs)

        | AtomCats(_,_,t) | AtomAlts(_,t) | Wild(_,_,_,t) | Neg(t) 
        | Restrict(t,_,_) -> collect acc (t::xs)
        | Inject(t1,t2) -> 
            collect acc (t1::t2::xs)
        | InjectMap(t1,tm) -> 
            let tms = NM.fold (fun _ ti acc -> ti::acc) tm [] in
              collect acc (t1::tms@xs)
      end in
  (* main formatter *)
  let var_map = collect NM.empty [syn_t0] in
    Util.format "@[@[<2>";
    format_one_syn FNone syn_t0;
    Util.format "@]";
    if not (NM.is_empty var_map) then 
      begin 
        Util.format "@\nwhere @[";
        let _ = NM.fold (fun tvi syn_tvi first -> 
                           if not first then Util.format "@\n";
                           Util.format "@[<2>%s@ =@ " tvi;
                           format_one_syn FNone syn_tvi;
                           Util.format "@]";
                           false)
          var_map true in
          Util.format "@]"
      end;
    Util.format "@]@\n"

let format_one_u = function 
    V(s) -> 
      Util.format "V["; 
      format_state s; 
      Util.format "]"
  | F((p,e)) -> 
      Util.format "F[@[";
      Presburger.format_t p;
      Util.format ".";
      Misc.format_list ",@ " 
	(fun (d,e) -> Util.format "n%d=" d; format_elt e) 
	(Misc.map_index_filter (fun n ei -> Some (n,ei)) e);
      Util.format "@]]" 

let format_one t0 = format_one_syn FNone t0.syn
  
let format_t t0 = 
  format_syn_t t0.syn;
  Trace.debug "schema-formulas+" 
    (fun () -> 
       Util.format "@\n";
       let rec collect acc = function
           []    -> acc 
         | V(CS s)::xs -> 
             let s_tvars = CSet.fold 
               (fun (is,ds) acc -> NS.union is (NS.union ds acc)) 
               s NS.empty in
             let new_vars = NS.diff s_tvars (NM.domain acc) in 
             let new_acc,new_xs = NS.fold 
               (fun tvi (new_acc,new_xs) -> 
		  let ti = lookup_tvar_required tvi in                   
		  let di = ti.def in 
		    (NM.add tvi di new_acc, di::new_xs))
		new_vars (acc,xs) in
               collect new_acc new_xs
         | V(_)::xs -> collect acc xs
         | F(_,e)::xs -> collect acc ((Safelist.map (fun (_,x) -> V(x)) e)@xs) in
       let d0 = t0.def in 
       let vmap = collect NM.empty [d0] in
         Util.format "@[@[<2>";
         format_one_u d0;
         Util.format "@]";
         if not (NM.is_empty vmap) then 
           begin 
             Util.format "@\nwhere @[";
             let _ = NM.fold 
                       (fun tvi ti first -> 
                          if not first then Util.format "@\n";
                        Util.format "@[<2>%s@ =@ " tvi;
                        format_one_u ti;
                        Util.format "@]";
                        false)
                     vmap true in
                     Util.format "@]"
                 end;
               Util.format "@]")

let format_counterexample = function 
    Domain d -> 
      Util.format "Domain({";
      Misc.format_list "," (fun s -> Util.format "%s" (Misc.whack s)) (NS.elements d);
      Util.format "})";
  | Child(ni,vi) -> 
      Util.format "Child(%s=" (Misc.whack ni); 
      Tree.format_t vi; 
      Util.format ")" 
let format_member_step = function
    StepYes -> Util.format "StepYes"
  | StepNo(ce) -> Util.format "StepNo("; format_counterexample ce; Util.format ")"
  | StepMaybe(ni,_) -> Util.format "StepMayube(%s,<fun>)" ni

let format_member_result = function
    Yes -> Util.format "Yes"
  | No(ce) -> Util.format "No("; format_counterexample ce; Util.format ")"
    
let format_low_level msg p e = 
  Util.format "%s @[p=@[" msg;
  P.format_t p;
  Util.format "@]@, e=@[";
  Misc.format_list "," (fun (r,x) -> R.format_t r; Util.format "["; format_state x; Util.format "]") e;
  Util.format "@]@]@\n" 


(* --------------- maps, sets, environments, formatting ------------------ *)
(* we maintain two environments: 
   - delta_tvar maps tvars to ts
   - delta_tvar maps compound type states to ts
   we could (and did, in an earlier version) use a unified
   environment, but lookups are slower when we have a simple type
   variable *)


module CSetEnv = Env.Make(
  struct
    type t = CSet.t
    let compare = CSet.compare
    let to_string = string_of_cset 
  end)

let delta : (t CSetEnv.t) ref = ref (CSetEnv.empty ())

let update_cset cxs t = delta := (CSetEnv.update (!delta) cxs t)
let update_cvar cx t = update_cset (cset_of_cvar cx) t

(* note: the other lookup functions, lookup_cvar and lookup_cset, are
   defined below. as they need to compute intersections/differences,
   they must be defined mutually with several other functions *)

(* ------------- simple lookup functions ------------- *)
(* generic helper: 
   lookup a bunch of variables from a set, returning a list option *)
let lookup_vars fld lkup s = fld
  (fun xi a -> match a,lkup xi with
       None,_ | _,None -> None
     | Some a,Some t -> Some(t::a)) 
  s
  (Some []) 

let lookup_tvars = lookup_vars NS.fold lookup_tvar 

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
  (fun u ti -> match ti.def with 
       V(CS(s)) -> NS.union (marked_cset (NM.domain (!marked_tvars_cell)) s) u
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
               (fun () -> Util.format 
                  "treeschema variable%s%s%s at %s appears in a non-contractive position"
                (if c=1 then " " else "s [")              
                  (Misc.concat_list "," (NS.elements xs))
                  (if c=1 then "" else "]")
                  (Info.string_of_t i))) in

  let check_contractive () = NM.fold
    (fun x io () -> 
       match io,lookup_tvar x with 
           Some i, Some t -> begin 
	     match t.def with 
		 V(CS s) -> 
		   let xs = marked_cset (NM.domain !marked_tvars_cell) s in
		     if not (NS.is_empty xs) then contractive_error i xs
               | _ -> () 
	   end
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
let xtruth = "T" 
let truth_cset = cset_of_tvar xtruth

(* hack *)
let truth_cell = ref None
let get_truth () = match !truth_cell with 
    None -> assert false
  | Some t -> t
let falsity_cell = ref None
let get_falsity () = match !falsity_cell with
    None -> assert false
  | Some t -> t

let xempty_view = "{}" 
let empty_view_cset = cset_of_tvar xempty_view

let xnon_empty_view = "T-{}" 
let non_empty_view_cset = cset_of_tvar xnon_empty_view

let anyE = (R.mk_cofinite [],True)
let anyE_basis = [anyE]

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
       if ei = xtruth then u 
       else CSet.add (NS.singleton xtruth, NS.singleton ei) 
         u)
    es 
    CSet.empty in
    NS.fold (fun di u -> CSet.add (NS.singleton di, NS.empty) u) ds u1

let rec neg_cset cxs = CSet.fold 
  (fun cx cxs_neg -> isect_cset (neg_cvar cx) cxs_neg) 
  cxs
  truth_cset

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
         if NS.mem xtruth es1 then es2
         else if NS.mem xtruth es2 then es1
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

let isect_elt (r1,x1) (r2,x2) = 
  match R.isect r1 r2 with
      None   -> None
    | Some r -> 
        let x = isect_state x1 x2 in 
          if x = False then None
          else Some(r,x)

(* --- union --- *)
let union_cset = CSet.union

let union_state x0 x1 = match x0,x1 with
    True,x | x,True -> True
  | False,x | x,False -> x
  | EmptyView,EmptyView -> EmptyView
  | NonEmptyView, NonEmptyView -> NonEmptyView 
  | EmptyView,NonEmptyView | NonEmptyView,EmptyView -> True
  (* might be able to do better here... *)
  | EmptyView,CS s | CS s,EmptyView       -> CS(union_cset empty_view_cset s)
  | NonEmptyView,CS s | CS s,NonEmptyView -> CS(union_cset non_empty_view_cset s)
  | CS s1, CS s2                          -> CS(union_cset s1 s2)

(* ----- mutually recursive definitions of lookups/refinements/constructors ------ *)

let compress p es = 
  let num_offsets = Safelist.length es in 
    let offsets = Array.make num_offsets 0 in 
    let _,es_annot_rev = Safelist.fold_left 
      (fun (pos,acc) (r,x) -> (pos+1,(pos,r,x,P.easy_var_value pos p)::acc)) (0,[]) es in        
    let es_annot = Safelist.rev es_annot_rev in 
    let rec loop sub acc = function
        []                    -> (P.substitute sub p, Safelist.rev acc)
      | (i,ri,xi,None)::es    -> 
          let sub' = (i,P.mkVar (i-offsets.(i)))::sub in 
          let acc' = (ri,xi)::acc in 
            loop sub' acc' es
      | (i,ri,xi,Some vi)::es ->
          let ok = if vi=0 then (fun v -> v=0) else (fun v -> v > 0) in 
          let matches,misses = Safelist.partition 
            (fun (_,_,y,vo) -> match vo with
                None -> false | Some v -> ok v && compare_state xi y = 0) es in
            let subms,vms,rms = Safelist.fold_left 
              (fun (suba,va,ra) (j,rj,_,vjo) -> match vjo with None -> assert false | Some vj -> 
                for k=j+1 to num_offsets-1 do offsets.(k) <- offsets.(k) + 1 done; 
                ((j,P.mkConst vj)::suba,va+vj,R.union ra rj)) ([],0,R.mk_finite []) matches in 
            let sub' = (i,P.mkSum [P.mkVar (i-offsets.(i)); P.mkConst (-vms)])::subms @ sub in 
            let acc' = (R.union ri rms,xi)::acc in 
              loop sub' acc' misses in 
      loop [] [] es_annot

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
         (fun () -> Util.format 
            "Treeschema.lookup_cset_required: %s not found"
            (string_of_cset cxs))) 

(* expopse the formula and basis of a schema *)
and expose t0 = 
  let truth_formula = P.mkGe (P.mkVar 0) P.zero in
  let empty_view_formula = P.mkEq (P.mkVar 0) P.zero in
    match t0.def with
        V(True)         -> truth_formula, anyE_basis
      | V(False)        -> P.mkNot truth_formula, anyE_basis
      | V(EmptyView)    -> empty_view_formula, anyE_basis
      | V(NonEmptyView) -> P.mkNot empty_view_formula, anyE_basis
      | V(CS s)         -> expose (lookup_cset_required s)
      | F(pi,ei)        -> (pi,ei)

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

  let p1_subst = Misc.map_index_filter
    (fun i _ -> Some (i, sum_of_int_list (IM.safe_find i vmap1 []))) e1 in
  let p2_subst = Misc.map_index_filter
    (fun j _ -> Some (j, sum_of_int_list (IM.safe_find j vmap2 []))) e2 in

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

and mk_neg t0 = match t0.def with 
    V(x) -> t_of_state (neg_state x)
  | F(p,e) -> make_t (F(P.mkNot p,e)) (Neg t0.syn)

and mk_diff t1 t2 = mk_isect [t1; mk_neg t2]

(* gen_mk: a generalized constructor for unions, intersections,
 * concatenations
 * mk_ps: a function to make the top-level Presburger formula from a
 *        list of formulas
 * cset_from_vars_opt: an optional function to make a cset if the ts
 *                     are all vars
 * ts: the list of schemas, with length >= 2
 *)
and gen_mk mk_ps mk_syns cset_from_vars_opt ts = 
  (* go () actually computes the type *)
  let go () = 
    let ps,e = refine ts in
    let p = mk_ps ps in
(*     let p',e' = compress p e in *)
(*     let u = F(p',e') in  *)
(*       Trace.debug "compress+" (fun () ->  *)
(*         Util.format "BARE="; format_one_u (F(p,e));  *)
(*         Util.format "@\nCOMPRESSED="; format_one_u u;  *)
(*         Util.format "@\n"); *)
    let u = F(p,e) in 
    let s = mk_syns (Safelist.map syn_of_t ts) in 
      make_t u s in 

  (* first, calculate the marked vars in ts *)
  let marked_xs = marked_ts ts in
    (* cases 1: if no ts marked *)
    if NS.is_empty marked_xs then     
      match cset_from_vars_opt, (Safelist.for_all is_var ts) with
          (* case 1a: and  all ts are vars and cset_from_vars_opt is Some(f) 
           *   then we make a variable directly *)
          Some f, true -> t_of_state (f ts)
          (* case 1b: some ts are not vars and/or cset_from_vars_opt is None 
           *          then we use go () to make the type *)
        | _ -> go ()

    (* case 2: if some ts have marked vars, add go to delayed work *)
    else 
      begin let x = fresh "_GEND" in
        add_delayed x (fun () -> update_tvar x (go ()));
        t_of_state (cstate_of_tvar x)
      end

and mk_isect = function
    (* knock off a few easy cases *)
    [] -> get_truth ()
  | [ti] -> ti
  | ts -> 
      gen_mk
        P.mkAnd 
        (fun ss -> Isect(ss))
        (Some (fun vs -> Safelist.fold_left 
                 (fun acc ti -> match ti.def with
                      V(xi) -> isect_state acc xi | 
                        _ -> assert false) True vs))
        ts

and mk_union = function
    (* knock off a few easy cases *)
    [] -> get_falsity ()
  | [ti] -> ti
  | ts -> 
      gen_mk 
        P.mkOr
        (fun ss -> Union(ss))
        (Some (fun vs -> Safelist.fold_left 
                 (fun acc ti -> match ti.def with
                      V(xi) -> union_state acc xi 
                    | _ -> assert false) False vs))
        ts

(* member functions *)
and mk_first_step u0 = match u0 with
  | V(True)          -> (fun ns -> StepYes)
  | V(False)         -> (fun ns -> StepNo(Domain ns))
  | V(EmptyView)     -> (fun ns -> if NS.is_empty ns then StepYes else StepNo(Domain ns))
  | V(NonEmptyView)  -> (fun ns -> if NS.is_empty ns then StepNo(Domain ns) else StepYes)
  | V(CS s)          -> (fun ns -> (lookup_cset_required s).first_step ns)
  | F(formula,basis) -> (fun ns -> 
      Trace.debug "member-detailed+" 
        (fun () -> 
          Util.format "first_step ";
          format_one_u u0;
          Util.format " {";
          Misc.format_list "," (Util.format "%s") (NS.elements ns);
          Util.format "}@\n");
      
      (* collect variables *)
      let num_vars,all_vars = Safelist.fold_left 
        (fun (x,s) _ -> (x+1,IS.add x s)) 
        (0,Int.Set.empty) basis in 
      let obvious_zeros = P.easy_zeros formula in 
          
      (* collect possibly-matching elements, var mapping, number of fresh vars *)
      let nl,vmap,fresh = NS.fold 
        (fun ni (nl,vmap,fresh) -> 
          let _,(nis,vmap',fresh') = Safelist.fold_left (fun (basis_pos,acc) (r,x) ->
            let basis_pos' = basis_pos+1 in 
            let acc' = 
              if x = False 
                || not (R.mem ni r) 
                || Int.Set.mem basis_pos obvious_zeros then acc
              else
                let nis,vmap,fresh = acc in 
                let vmap' = Int.Map.add basis_pos (fresh::Int.Map.safe_find basis_pos vmap []) vmap in 
                let nis' = (fresh,x)::nis in 
                let fresh' = fresh+1 in 
                  (nis',vmap',fresh') in
              (basis_pos',acc'))
            (0,([],vmap,fresh)) basis in 
            ((ni,nis)::nl,vmap',fresh'))
        ns
        ([],Int.Map.empty,num_vars) in
        
      let zeros = Int.Set.diff all_vars (Int.Map.domain vmap) in
        
      (* optimization: remove fresh variables for i with vcounts(i) = 1 *)
      let fresh_map,vmap = IM.fold (fun basis_pos fresh_vars acc ->
        match fresh_vars with
            [freshi] ->
              let (fm,vm) = acc in
                (IM.add freshi basis_pos fm, IM.remove basis_pos vm)
          | _        -> acc)
        vmap (Int.Map.empty,vmap) in
        
      (* this rev_map is crticial--it puts the nl, which specifies
         the order that children are checked, back in alphabetical
         order. later on, we could optimize further, but for lists
         we definitely want hd checked before tl! *)
      let nl = Safelist.rev_map 
        (fun (ni,nis) -> (ni, Safelist.map (fun (n,s) -> (IM.safe_find n fresh_map n,s)) nis)) nl in

      let zero_constraint = P.mkEq P.zero (sum_of_int_set zeros) in 
      let and_one_constraints = Safelist.fold_left (fun acc (_,nis) -> P.mkEq P.one (sum_of_int_state_list nis)::acc) [zero_constraint] nl in 
      let and_var_equalities = Int.Map.fold (fun xi xil acc -> P.mkEq (P.mkVar xi) (sum_of_int_list xil)::acc) vmap and_one_constraints in 
      let formula' = P.mkAnd (formula::and_var_equalities) in
      let rec next_step f = function
          [] -> StepYes
        | (ni,[(xi,si)])::nrest -> 
            StepMaybe(ni, 
                      (fun vi -> 
			 (* optimization: can just do the member test and skip satisifiability test here... *)			 
			 Trace.debug "member-detailed+" 
		         (fun () -> 
		            Util.format "CHECK_VI %s " ni;
		            Tree.format_t vi;
		            Util.format "@\n");
			 if member_state_with_counterexample si vi = Yes then next_step f nrest
			 else StepNo(Child(ni,vi))))
              
        | (ni,nis)::nrest -> 
            StepMaybe(ni, 
                      (fun vi -> 
			 Trace.debug "member-detailed+" 
		         (fun () -> 
		            Util.format "CHECK_VI %s " ni;
		            Tree.format_t vi;
		            Util.format "@\n");
			 let found,fs = Safelist.fold_left
	                   (fun (found,fs) (xi,si) -> 
			      if found then (found, zero_of_var xi::fs)
			      else if member_state_with_counterexample si vi = Yes then                       
				(true,one_of_var xi::fs)
		              else 
				(false,zero_of_var xi::fs))
                           (false,[f]) nis in 
		         if not found then StepNo(Child(ni,vi)) 
		         else 
			   let f' = P.mkAnd fs in 
                             if P.satisfiable f' then next_step f' nrest
                             else StepNo(Child(ni,vi)))) in
                       
        (* --- result --- *)
        if P.satisfiable formula' then next_step formula' nl
        else StepNo(Domain ns)) 

and member_state_with_counterexample s0 = match s0 with 
    True         -> (fun v -> Yes)
  | False        -> (fun v -> No(Domain(Tree.dom v)))
  | EmptyView    -> (fun v -> if Tree.is_empty v then Yes else No(Domain(Tree.dom v)))
  | NonEmptyView -> (fun v -> if Tree.is_empty v then No(Domain(Tree.dom v)) else Yes)
  | CS s         -> (fun v -> (lookup_cset_required s).member v)

and mk_member first_step_u0 u0 = match u0 with
    V(s) -> member_state_with_counterexample s
  | F(formula,basis) -> (fun v -> 
      let rec loop mr0 = 
        Trace.debug "member-detailed+" 
          (fun () -> 
            Util.format "MEMBER LOOP "; 
            format_member_step mr0; 
            Util.format "@\n");
        match mr0 with
            StepYes -> Yes
          | StepNo(ce) -> No(ce)
          | StepMaybe(ni,check_vi) -> loop (check_vi (Tree.get_required v ni)) in 
        loop (first_step_u0 (Tree.dom v)))

(* ----- type t ------ *)
and make_t u s = 
  let first_step_u0 = 
    let module M = Memo.Make2(
      struct
        type arg = Name.Set.t
        type res = member_step
        let name = "Treeschema.first_step"
        let f = mk_first_step u 
        let init_size = 3
        let format_arg d = Util.format "{%s}" (Misc.concat_list "," (NS.elements d))
        let format_res = format_member_step
        let hash = Hashtbl.hash
        let equal' = (==)
        let hash' = Hashtbl.hash
        let equal = Name.Set.equal
      end) in 
    M.memoized in

  let member_u0 = 
    let module M = Memo.Make(
      struct
        type arg = Tree.t
        type res = member_result
        let name = "Treeschema.member"
        let f = mk_member first_step_u0 u 
        let init_size = 3
        let format_arg = Tree.format_t
        let format_res = format_member_result
        let hash = Tree.hash
        let equal = (==)
        let hash' = Tree.hash
        let equal' = Tree.equal 
      end) in 
    M.memoized in

    (* result *)
    incr t_hash_mix;
    { def=u;
      hash= 73 * !t_hash_mix + 19 * Hashtbl.hash u; 
      syn=s;    
      first_step = first_step_u0;
      member = member_u0 }

and t_of_state s = make_t (V s) (Var s)

(* --- more constants --- *)
(* truth, the most inclusive schema--contains every tree *)
let truth = 
  let truth = t_of_state(True) in 
    update_tvar xtruth truth;
    truth_cell := Some truth;
    truth
       
(* falsity, the snobbiest schema--contains no trees *)
let falsity = 
  let falsity = t_of_state(False) in 
    falsity_cell := Some falsity;
    falsity

(* empty_view, the singleton schema containing the tree {} *)
let empty_view =
  let empty_view = t_of_state(EmptyView) in 
    update_tvar xempty_view empty_view;
    empty_view 

(* when we create an atom or a wildcard from a subschema t, we need to
   create a basis over t. as elements are defined using states, we
   need to find a state representing t. *)
let state_of_t = 
  let module M = Memo.Make(
    struct
      type arg = t
      type res = state
      let name = "Treeschema.state_of_t"
      let f t0 = match t0.def with
          V(s) -> s
        | _ -> 
            let n = fresh "_GEN" in 
            let x = CS(cset_of_tvar n) in 
              update_tvar n t0;
              x 
      let init_size = 389
      let format_arg t0 = format_one t0
      let format_res = format_state
      let hash t = t.hash
      let equal = (==)
    end) in 
    M.memoized

(* ------------ simple constructors -------------*)

let mk_any = truth

let mk_empty = falsity

let mk_var x = t_of_state (cstate_of_tvar x)

let mk_atom_alts ns t = 
  let x = state_of_t t in 
  let syn = AtomAlts(ns_of_n_list ns,syn_of_t t) in
  let (ps,e) = 
    if x = True then
      [P.mkEq (P.mkVar 0) P.one;
       P.mkEq (P.mkVar 1) P.zero],
      [(R.mk_finite ns,True);
       (R.mk_cofinite ns, True)]
    else
      [P.mkEq (P.mkVar 0) P.one;
       P.mkEq (P.mkVar 1) P.zero;
       P.mkEq (P.mkVar 2) P.zero],
    [(R.mk_finite ns,x);
     (R.mk_finite ns,neg_state x);
     (R.mk_cofinite ns, True)] in
  let u = F(P.mkAnd ps,e) in 
    make_t u syn

let mk_atom_cats opt ns t = 
  let x = state_of_t t in 
  let syn = AtomCats(opt,ns_of_n_list ns,syn_of_t t) in
  let cnstr = if opt then P.mkLe else P.mkEq in 
  let (ps,e) = 
    if x = True then
      [cnstr (P.mkVar 0) (P.mkConst (Safelist.length ns));
       P.mkEq (P.mkVar 1) P.zero],
      [(R.mk_finite ns,True);
       (R.mk_cofinite ns, True)]
    else
      [cnstr (P.mkVar 0) (P.mkConst (Safelist.length ns));
       P.mkEq (P.mkVar 1) P.zero;
       P.mkEq (P.mkVar 2) P.zero],
    [(R.mk_finite ns,x);
     (R.mk_finite ns,neg_state x);
     (R.mk_cofinite ns, True)] in
  let u = F(P.mkAnd ps,e) in 
    make_t u syn

let mk_atom n t = mk_atom_cats false [n] t
      
let mk_wild f l u t = 
  let x = state_of_t t in   
  let syn = Wild(f,l,u,syn_of_t t) in

  (* FIX LATER: replace this fold with R.from_list or something *)
  let cof_pos,fin_ps_rev,fin_es_rev = NS.fold 
    (fun ni (pos,ps,es) -> 
       (pos+1,
       (P.mkEq (P.mkVar pos) P.zero)::ps,
       (R.mk_finite [ni],True)::es))
    f
    (0,[],[]) in

  (* FIX LATER: no reason for f to be a set; might as well keep it as a list *)
  let (ps_rev,e_rev)=
    if x = True then 
      ((if u then P.mkGe (P.mkVar cof_pos) (P.mkConst l)
        else P.mkEq (P.mkVar cof_pos) (P.mkConst l))::fin_ps_rev,
       (R.mk_cofinite (NS.elements f), x)::fin_es_rev)
    else
      ((P.mkEq (P.mkVar (cof_pos + 1)) P.zero)
       :: (if u then P.mkGe (P.mkVar cof_pos) (P.mkConst l)
           else P.mkEq (P.mkVar cof_pos) (P.mkConst l))
       :: fin_ps_rev,
       ((R.mk_cofinite (NS.elements f), neg_state x)
        ::(R.mk_cofinite (NS.elements f), x)
        ::fin_es_rev)) in
  let u = F(P.mkAnd (Safelist.rev ps_rev), Safelist.rev e_rev) in 
    make_t u syn

let rec mk_cat = function
    [] -> empty_view
  | [t1] -> t1
  | ts -> gen_mk P.add (fun ss -> Cat(ss)) None ts 
        
(* list schemas *)
let mk_nil = mk_cat [mk_atom Tree.nil_tag (mk_cat [])]
let mk_cons h t = mk_cat [mk_atom Tree.hd_tag h; mk_atom Tree.tl_tag t]

let mk_list i t =
  (* check that we are not calling this while the tvar marking stuff
     is already in use, e.g., from inside the compiler *)
  assert (NM.is_empty !marked_tvars_cell);
  let fresh_x = Syntax.fresh "_LIST" in 
  let x_t = mk_var fresh_x in
  mark_tvars [fresh_x,i];
  update_tvar fresh_x (mk_union [mk_nil; mk_cons t x_t]);
  finalize ();        
  x_t

module T_of_TreeMemo = Memo.MakeLater(struct
  type arg = Tree.t
  type res = t
  let name = "Treeschema.t_of_tree"
  let init_size = 389
  let format_arg = Tree.format_t
  let format_res = format_one
  let hash = Tree.hash
  let equal = (==)
  let hash' = Tree.hash
  let equal' = Tree.equal
end)
let rec bare_t_of_tree v = mk_cat (Tree.fold (fun k vk ts -> (mk_atom k (t_of_tree vk))::ts) v [])
and t_of_tree v = T_of_TreeMemo.memoize bare_t_of_tree v
         
(* --------------- operations ----------------------*)

(* --- empty test --- *)
(* cached emptiness results *)

type this_t = t (* hack! *)
module TSet = Set.Make(struct
    type t = this_t
    let compare = compare_t
  end)

let (empties,non_empties) = (ref TSet.empty, ref TSet.empty)

let rec empty_aux print (es,nes) t0 = 
  let res = 
    if TSet.mem t0 es then (true,es,nes)
    else if TSet.mem t0 nes then (false,es,nes)
    else 
      let t0_empty,es',nes' = match t0.def with
          V(x) -> begin match x with
              True | EmptyView | NonEmptyView -> (false,es,nes)
            | False -> (true,es,nes)
            | CS s  -> empty_aux false (TSet.add t0 es,nes) (lookup_cset_required s)
            end 
        | F(p,e) -> check_empty (es,nes) (p,e) in
        if t0_empty then (true,TSet.add t0 es',nes')
        else (false,es,TSet.add t0 nes) in
  let b_res,_,_ = res in 
    Trace.debug "treeschema+" 
      (fun () -> 
         Util.format "@\nEMPTY AUX %b " b_res; 
         format_t t0;
         if print then begin
           Util.format "@\nes=@[";
           Misc.format_list "@\n" format_t (TSet.elements es);
           Util.format "@]@\nnes=@[";
           Misc.format_list "@\n" format_t (TSet.elements nes);
           Util.format "@]@\n";
         end
         else Util.format "@\n");
    res
        
and check_empty (es,nes) (p,e) = 
  (* FIX LATER: we can avoid computing this refinement in (we hope)
     many common cases *)
  (* (1) refine so that each finite R.t is a singleton *)
  let res = 
    let finite_names = Safelist.fold_left 
      (fun acc (ri,_) -> match R.finite_domain ri with 
           Some ns -> NS.union ns acc 
         | None -> acc)
      NS.empty e in 
    let dummy_basis = NS.fold 
      (fun ni db -> (R.mk_finite [ni], True)::db)
      finite_names [R.mk_cofinite (NS.elements finite_names),True] in 
    let (_,subst),basis = refine_bases dummy_basis e in 
    let formula = P.substitute subst p in
      
    (* construct a sum of vars and set of names mentioned in finite
       elts, and all the sub_formulas *)
    let collect es = 
      let rec aux (pos, nmap, facc) = function 
          [] -> (pos, nmap, Safelist.rev facc)
        | (r,x)::rest -> begin
            match R.singleton_domain r with 
                None -> aux (pos+1, nmap, x::facc) rest 
              | Some n -> 
                 let old_sum = NM.safe_find n nmap (P.mkConst 0) in 
                  let new_nmap = NM.add n (P.mkSum [P.mkVar pos; old_sum]) nmap in
                    aux (pos+1, new_nmap, x::facc) rest 
          end in
      let _,nmap,facc = aux (0,NM.empty,[]) es in 
        (nmap,facc) in
      
    let nmap, sub_formulas = collect basis in 
      
    (* add constraint that these are feature trees *)
    let ft_constraints = NM.fold 
      (fun _ sum acc -> P.mkLe sum P.one::acc)
      nmap [] in
      
    (* determine which are empty/non-empty *)
    let rec empties (fs,es,nes) pos = function
        []   -> (fs,es,nes)
      | x::xrest -> 
          let fake_t0 = t_of_state x in
          let (b,es',nes') = empty_aux false (es,nes) fake_t0 in
            if b then begin 
              let fs' = (P.mkEq (P.mkVar pos) P.zero)::fs in 
                empties (fs',TSet.add fake_t0 es', nes') (pos+1) xrest
            end
            else 
              empties (fs,es,TSet.add fake_t0 nes') (pos+1) xrest in
    let ft_zero_constraints,es',nes' = empties (ft_constraints,es,nes) 0 sub_formulas in      
    let checked_formula = (P.mkAnd (formula::ft_zero_constraints)) in            
      (not (P.satisfiable checked_formula),es',nes'),formula,basis in
  let real_res,formula,basis = res in
  let b_res,_,_ = real_res in
    Trace.debug "treeschema+"
      (fun () -> 
         Util.format "--- CHECK_EMPTY --- %b" b_res;
         Util.format "@\n(p,e) = "; 
         P.format_t formula; 
         Util.format ","; 
         (Misc.format_list "," (fun (r,x) -> R.format_t r; Util.format "["; format_state x; Util.format "]") basis);
         Util.format "@\nes=@[";
         Misc.format_list "@\n" format_t (TSet.elements es);
         Util.format "@]@\nnes=@[";
         Misc.format_list "@\n" format_t (TSet.elements nes);
         Util.format "@]@\n";
         Util.format "@\n");
    real_res

let is_empty t0 = 
  Trace.debug "treeschema+"
    (fun () -> 
       Util.format ">>> START EMPTY@\n  @[";
       format_t t0;
       Util.format "@\n");
  let res = 
    let t0_empty,new_empties,new_non_empties = 
      empty_aux true (!empties,!non_empties) t0 in
      empties := new_empties;
      non_empties := new_non_empties;
      t0_empty in 
    
    Trace.debug "treeschema+"
      (fun () -> 
         Util.format "@]@\n<<<EMPTY %b\t" res;
         format_one t0;
         Util.format "@\n");
    res
  
let subschema t0 t1 = is_empty (mk_diff t0 t1)

let equivalent t0 t1 = subschema t0 t1 && subschema t1 t0

(* -------------------------- member testing --------------------------------------*)
(* JNF: this is the slow, direct version. later we should merge it to
   use setup_member and a loop... *)
(* --- dom_member --- *)            
let rec dom_member ns t0 = 
  let res = match t0.def with
    V(x) -> begin match x with 
        True -> true
      | False -> false
      | EmptyView -> NS.is_empty ns
      | NonEmptyView -> not (NS.is_empty ns)
      | CS s -> dom_member ns (lookup_cset_required s)
      end
    | F(p,e) -> 
        let (new_vars,nmap,vmap) = NS.fold
          (fun k (new_vars,nmap,vmap) -> 
            let _,new_vars,nmap,vmap = Safelist.fold_left 
              (fun (pos,new_vars,nmap,vmap) (r,x) -> 
                let yes () = 
                  let nmap' = NM.add k (new_vars::(NM.safe_find k nmap [])) nmap in
                  let vmap' = IM.add pos (new_vars::(IM.safe_find pos vmap [])) vmap in
                    (pos+1, new_vars+1, nmap', vmap') in
                let no () = (pos+1, new_vars, nmap, vmap) in
                let on_match = function
                    True | EmptyView | NonEmptyView -> yes ()
                  | False -> no ()
                  | CS s  -> if is_empty (lookup_cset_required s) then no () else yes () in
                  if R.mem k r then on_match x else no ())
              (0,new_vars,nmap,vmap) 
              e in
              (* result of Safelist.fold_left *)
              (new_vars,nmap,vmap))
          ns 
          (0,NM.empty,IM.empty)in        
        let f1 = NM.fold (fun _ xl acc -> (P.mkEq (sum_of_int_list xl) P.one)::acc) nmap [] in
        let subst = IM.fold (fun x xl acc -> (x+new_vars, sum_of_int_list xl)::acc) vmap [] in
        let formula = P.mkAnd (P.substitute subst (P.shift_t new_vars p)::f1) in
          P.satisfiable formula 
  in
    (* Util.format "DOM_MEMBER {%s}" (ns2str "," ns);
       format_t t0;
       Util.format " = %b" res;
       Util.format "@\n"; *)
    res
          
let total_member = ref 0

let member_with_counterexample v t0 = t0.member v

let member v t0 = 
  incr total_member;
  let enter_debug () = 
    Util.format ">>> MEMBER";
    Util.format "@\n v : "; Tree.format_t v;
    Util.format "@\nt0 : "; format_one t0;
    Util.format "@\n%!" in 

    Trace.debug "member+" enter_debug;
    
    let res = match member_with_counterexample v t0 with
	Yes   -> true
      | No(_) -> false in 
           
  let exit_debug () =        
    Util.format "<<< MEMBER";
    Util.format "@\n v : "; Tree.format_t v;
    Util.format "@\nt0 : "; format_t t0;
    Util.format "@\n  = %b@\n%!" res in 

    Trace.debug "member+" exit_debug;
    res
      
(* --- project --- *)
let rec project k t0 = match t0.def with
    V(True) | V(NonEmptyView) -> truth
  | V(False) | V(EmptyView)   -> falsity
  | V(CS(s)) -> project k (lookup_cset_required s)
  | F(p,e)   ->
      let eks = Misc.map_index_filter
        (fun pos (r,x) -> 
           if R.mem k r && (P.satisfiable (P.mkAnd [p; P.mkEq (P.mkVar pos) P.one])) then
             match x with 
                 True | EmptyView | NonEmptyView | CS _ -> Some (t_of_state x)
               | False -> None
           else None)
        e in
      mk_union eks

let rec project_all t0 = match t0.def with 
    V(True) | V(NonEmptyView) -> Some (truth)
  | V(False) | V(EmptyView) -> Some (falsity)
  | V(CS(s)) -> project_all (lookup_cset_required s)
  | F(p,e) -> 
      let _,ok,reso = Safelist.fold_left
        (fun (pos,ok,reso) (_,x) -> 
           if not ok then (pos,ok,reso) 
           else if not (P.satisfiable (P.mkAnd [p; P.mkEq (P.mkVar pos) P.one])) then 
               (pos+1,ok,reso)
             else match reso with 
                 None   -> (pos+1,true,Some x)
               | Some y -> 
                   if equivalent (t_of_state x) (t_of_state y) then (pos+1,true,Some y)
                   else (pos+1,false,None))
        (0,true,None)
        e in
      if ok then map_opt reso t_of_state else None

(* quick & dirty hack: finite/cofinite name sets *)
let r2fds r = match R.finite_domain r with 
    Some ns -> (true,ns)
  | _ -> begin
      match R.cofinite_domain r with 
          Some ns -> (false,ns)
        | _ -> assert false
    end           

let fds_union fds1 fds2 = 
  match fds1,fds2 with
      (true,ns1),(true,ns2)                           -> (true,NS.union ns1 ns2)
    | (true,ns1),(false,ns2) | (false,ns2),(true,ns1) -> (false,NS.diff ns2 ns1)
    | (false,ns1),(false,ns2)                         -> (false,NS.inter ns1 ns2) 
  
let fds_mem n = function
    (true,ns1) -> NS.mem n ns1
  | (false,ns1) -> not (NS.mem n ns1)  
  
let fds_isect_regexp fds r = match fds with
    (true,ns1) -> NS.fold (fun n b -> b || (R.isect (R.mk_finite [n]) r = None)) ns1 false
  | (false,ns1) -> R.isect (R.mk_cofinite (NS.elements ns1)) r = None  

let complete_basis_rev x_count formulas basis_rev = function
    (* if covered is a cofinite set *)
    (false,ns) -> 
      if NS.is_empty ns then formulas,basis_rev
      else
        let _,formulas,basis_rev = NS.fold
          (fun ni (x_pos,formulas,basis_rev) -> 
             (x_pos+1,P.mkEq (P.mkVar x_pos) P.zero::formulas,
              (R.mk_finite [ni],True)::basis_rev))
          ns
          (x_count,formulas,basis_rev) in 
          formulas,basis_rev
            
  (* covered is a finite set *)
  | (true,ns) -> 
      (P.mkEq (P.mkVar x_count) P.zero::formulas,
       ((R.mk_cofinite (NS.elements ns),True)::basis_rev)) 
   
let inject t0 t1 =   
  let formula,basis = expose t0 in 
    
  (* construct states, or a map to states *)
  let y_t1 = state_of_t t1 in
  let y_neg_t1 = neg_state y_t1 in 
    
  (* construt new basis, new formulas, etc. *)
  let _,x_count,subst,formulas,basis_rev,covered = Safelist.fold_left
    (fun acc (r,x) -> 
       let pos,x_pos,subst,formulas,basis,covered = acc in
       let pos' = pos+1 in                     
       let do_zero () =            
         (* zero out this position, don't cover it *)
         (pos',x_pos,(pos,P.zero)::subst,formulas,basis,covered) in 
       let do_found () = 
         (* replace this position with y y_neg *)
         let covered' = fds_union covered (r2fds r) in
           if y_neg_t1 = False then 
             let x_pos' = x_pos+1 in 
             let subst' = (pos,P.mkVar x_pos)::subst in 
             let basis' = (r,y_t1)::basis in 
               (pos',x_pos',subst',formulas,basis',covered')
           else
             let x_pos' = x_pos+2 in
             let subst' = (pos,P.mkVar x_pos)::subst in
             let formulas' = P.mkEq (P.mkVar (x_pos+1)) P.zero::formulas in
             let basis' = (r,y_neg_t1)::(r,y_t1)::basis in
               (pos',x_pos',subst',formulas',basis',covered') in 
         
       let pos_satisfiable = 
         let pos_var = P.mkVar pos in 
           if R.is_singleton r then (P.satisfiable (P.mkAnd [formula; P.mkEq pos_var P.one]))
           else 
             let constr_formula = P.mkAnd [formula; P.mkGt pos_var P.zero] in               
               match R.finite_domain r with 
                   Some ns -> 
                     let lt_constr = P.mkLe pos_var (P.mkConst (NS.cardinal ns)) in
                       P.satisfiable (P.mkAnd [ lt_constr; constr_formula ])                         
                 | None -> P.satisfiable constr_formula in 
         if (fds_isect_regexp covered r) || not pos_satisfiable then do_zero ()
         else do_found ())
    (0,0,[],[],[],(true,NS.empty))
    basis in
    
  (* add some zero-d elements to make this a basis *)
  let formulas,basis_rev = complete_basis_rev x_count formulas basis_rev covered in

  (* inject result *)
  let res_formula = P.mkAnd ((P.substitute subst formula)::formulas) in
  let res_basis = Safelist.rev basis_rev in     
  let res_syn = Inject(syn_of_t t0, syn_of_t t1) in
  let res_u =  F(res_formula,res_basis) in 
    make_t res_u res_syn		  
          
let inject_map t0 fm = 
  let basis, basis_length, formula = 
    let p,e = expose t0 in       
    let ns = NM.domain fm in 
    let ns_e = NS.fold 
      (fun ni acc -> (R.mk_finite [ni],True)::acc) 
      ns [R.mk_cofinite (NS.elements ns),True] in
    let (_,subst),basis = refine_bases ns_e e in
    let basis_length = Safelist.length basis in
    let formula = P.substitute subst p in
      basis, basis_length, formula in 
    
  (* map from names to state pairs *)
  let y_fm = NM.fold 
    (fun ni ti acc -> 
       let y_ti = state_of_t ti in 
       let y_neg_ti = neg_state y_ti in 
         NM.add ni (y_ti,y_neg_ti) acc)
    fm NM.empty in 
    
  (* construct new basis, new formulas, etc. *)
  let _,x_count,subst,formulas,basis_rev,covered = Safelist.fold_left
    (fun acc (r,x) -> 
       let pos,x_pos,subst,formulas,basis,covered = acc in
       let pos' = pos+1 in                     
       let do_zero () = 
         (pos',x_pos,(pos,P.zero)::subst,formulas,basis,covered) in 
       let do_skip () = 
         let x_pos' = x_pos+1 in 
         let subst' = (pos,P.mkVar x_pos)::subst in 
         let basis' = (r,x)::basis in 
         let covered' = fds_union covered (r2fds r) in
           (pos',x_pos',subst',formulas,basis',covered') in 
       let do_found y y_neg = 
         let covered' = fds_union covered (r2fds r) in
           if y_neg = False then 
             let x_pos' = x_pos+1 in 
             let subst' = (pos,P.mkVar x_pos)::subst in 
             let basis' = (r,y)::basis in 
               (pos',x_pos',subst',formulas,basis',covered')
           else
             let x_pos' = x_pos+2 in
             let subst' = (pos,P.mkVar x_pos)::subst in
             let formulas' = P.mkEq (P.mkVar (x_pos+1)) P.zero::formulas in
             let basis' = (r,y_neg)::(r,y)::basis in
               (pos',x_pos',subst',formulas',basis',covered') in 
         
         match R.singleton_domain r with 
           | None -> do_skip ()
           | Some n -> 
               (try let y,y_neg = NM.find n y_fm in 
                  if fds_mem n covered then do_zero ()
                  else do_found y y_neg
                with Not_found -> do_skip ()))
    (0,0,[],[],[],(true,NS.empty))
    basis in
    
  (* add some zero-d elements to make this a basis *)
  let formulas,basis_rev = complete_basis_rev x_count formulas basis_rev covered in 
  let res_formula = P.mkAnd ((P.substitute subst formula)::formulas) in
  let res_basis = Safelist.rev basis_rev in 
  let res_syn = 
    InjectMap(syn_of_t t0, 
              NM.fold (fun ni ti acc -> NM.add ni (syn_of_t ti) acc) fm NM.empty) in
    (* inject map result *)
  let res_u = F(res_formula,res_basis) in 
    make_t res_u res_syn
      
let restrict ns t0 = 
  (* FIX LATER: see if we can avoid refining once we have full RegExp functionality *)
  let restrict_aux (p,e) syn = 
    (* (1) rewrite p and e so that no ns intersect a CoFinite element of e *)
    let ns_e = NS.fold (fun ni acc -> (R.mk_finite [ni],True)::acc) 
      ns 
      [R.mk_cofinite (NS.elements ns),True] in
    let (_,subst),basis = refine_bases ns_e e in
    let basis_length = Safelist.length basis in
    let formula = P.substitute subst p in

    Trace.debug "restrict+" 
      (fun () -> format_low_level (sprintf "RESTRICT {%s}" (Misc.concat_list "," (NS.elements ns))) formula basis);

    (* (2) fold over the basis and construct schema restricted to ns, neg(ns) *)
    let _,(ns_xs,ns_basis_rev,ns_fs,ns_covered),(neg_ns_xs,neg_ns_basis_rev,neg_ns_fs) = Safelist.fold_left
      (fun (pos,ns_acc,neg_ns_acc) ei ->
         let ns_hit n ei = 
           let ns_pos,ns_basis,ns_fs,covered = ns_acc in
           let fi = P.mkEq (P.mkVar pos) (P.mkVar ns_pos) in 
             (pos+1,
              (ns_pos+1,ei::ns_basis, fi::ns_fs,NS.add n covered),
              neg_ns_acc) in 
         let neg_ns_hit ei = 
           let neg_ns_pos,neg_ns_basis,neg_ns_fs = neg_ns_acc in
           let fi = P.mkEq (P.mkVar pos) (P.mkVar neg_ns_pos) in 
             (pos+1,
              ns_acc,
              (neg_ns_pos+1,ei::neg_ns_basis,fi::neg_ns_fs)) in
         let (r,_) = ei in            
           match R.singleton_domain r with 
               Some n -> 
                 if NS.mem n ns then ns_hit n ei
                 else neg_ns_hit ei
             | None -> neg_ns_hit ei)
      (0,(basis_length,[],[],NS.empty),(basis_length,[],[]))
      basis in
      
    (* (3) make both a basis *)
    let ns_basis_rev,ns_fs = 
      ((R.mk_cofinite (NS.elements ns_covered),True)::ns_basis_rev),
      (P.mkEq (P.mkVar ns_xs) P.zero)::ns_fs in
      
    let _,neg_ns_basis_rev,neg_ns_fs = NS.fold 
      (fun ni (x,b,fs) -> (x+1,(R.mk_finite [ni],True)::b, (P.mkEq (P.mkVar x) P.zero)::fs))
      ns (neg_ns_xs,neg_ns_basis_rev,neg_ns_fs) in
      
    let ns_basis = Safelist.rev ns_basis_rev in 
    let neg_ns_basis = Safelist.rev neg_ns_basis_rev in
      
    (* (4) construct the forumlas, bases *)
    let ns_f = P.wrap basis_length (P.mkAnd (formula::ns_fs)) in
    let neg_ns_f = P.wrap basis_length (P.mkAnd (formula::neg_ns_fs)) in
    let restr_syn = Restrict(syn,ns,true) in
    let restr_u = F(ns_f,ns_basis) in 
    let restr_t = make_t restr_u restr_syn in 
    let restr_neg_syn = Restrict(syn,ns,false) in
    let restr_neg_u = F(neg_ns_f,neg_ns_basis) in 
    let restr_neg_t = make_t restr_neg_u restr_neg_syn in 
      Trace.debug "restrict+" 
        (fun () -> 
           format_low_level (sprintf "NS RESULT {%s}" (Misc.concat_list "," (NS.elements ns))) ns_f ns_basis;
           format_low_level (sprintf "NEG NS RESULT {%s}" (Misc.concat_list "," (NS.elements ns))) neg_ns_f neg_ns_basis);
      (restr_t, restr_neg_t) in
    match t0.def with 
        V(False)     -> (falsity, falsity)
      | V(EmptyView) -> (empty_view, empty_view)
      | _            -> restrict_aux (expose t0) (syn_of_t t0)

let is_list t =
  let i = Info.M "is_list" in
     equivalent t (mk_list i falsity)
  || (let elt = project Tree.hd_tag t in
      not (is_empty elt) && equivalent t (mk_list i elt))

(* external exports *)
let update = update_tvar

(* (* debug setup member *) *)          
(*           Trace.debug "member+" (fun () ->  *)
(*             let ns2str s = sprintf "{%s}" (Misc.concat_list "," (NS.elements s)) in  *)
(*             let is2str s = sprintf "{%s}" (Misc.concat_list "," (Safelist.map string_of_int (IS.elements s))) in  *)
(*             let p2str f = Util.format_to_string (fun () -> P.format_t f) in  *)
(*             let format_nl x =  *)
(*               Util.format "["; *)
(*               Misc.format_list ","  *)
(*                 (fun (ni,nis) ->  *)
(*                   Util.format "(%s,[" ni; *)
(*                   Misc.format_list "," (fun (xi,si) -> Util.format "(%d,%s)" xi (string_of_state si)) nis; *)
(*                   Util.format "])";) x;           *)
(*               Util.format "]" in  *)
(*             let format_vmap vm =  *)
(*               Util.format "{"; *)
(*               ignore (Int.Map.fold  *)
(*                          (fun xi xs sep ->  *)
(*                            if sep then Util.format ","; *)
(*                            Util.format "%d->[" xi; *)
(*                            Misc.format_list "," (Util.format "%d") xs; *)
(*                            Util.format "]"; *)
(*                            true) *)
(*                          vm false); *)
(*               Util.format "}" in  *)
              
(*               Util.format "SETUP_MEMBER %s %s@\n" (ns2str ns) (p2str formula); *)
(*               Util.format "OBVIOUS_ZEROS: %s@\n" (is2str obvious_zeros); *)
              
(*               Util.format "ZEROS: %s" (is2str zeros); *)
(*               Util.format "@\nNL: "; format_nl nl; *)
(*               Util.format "@\nVMAP: "; format_vmap vmap; *)
(*               Util.format "@\n"); *)
          
