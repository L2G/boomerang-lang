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
let fresh = Syntax.fresh
module NS = Name.Set
module NM = Name.Map 
module P = Presburger
module IM = P.IntMap

module type SRegExp = sig 
  type t 
  val mk_singleton : Name.t -> t
  val mk_cofinite : Name.t list -> t
  val fformat_t : Format.formatter -> t -> unit
  val format_t : t -> unit
  val isect : t -> t -> t option
  val finite_domain : t -> Name.Set.t option
  val cofinite_domain : t -> Name.Set.t option
  val is_singleton : t -> bool
  val is_finite : t -> bool
  val singleton_domain : t -> Name.t option
  val mem : Name.t -> t -> bool
end

module RegExp : SRegExp = struct
  type t = Single of Name.t | CoFinite of NS.t
  let fformat_t fmtr = function 
      Single(n)    -> Format.fprintf fmtr "@[%s@]" (Misc.whack n)
    | (CoFinite ns) -> Format.fprintf fmtr "@[~{%s}@]" 
        (Misc.concat_list "," (Safelist.map Misc.whack (NS.elements ns)))
  let format_t = fformat_t Format.std_formatter
  let mk_singleton n = Single(n)
  let mk_cofinite l = 
    let ns = Safelist.fold_left 
      (fun acc ni -> NS.add ni acc) 
      NS.empty l in 
      CoFinite(ns)
  let isect r1 r2 = match r1,r2 with
      (Single n1),(Single n2)       -> if (n1=n2) then Some(r1) else None
    | (CoFinite ns1),(CoFinite ns2) -> Some(CoFinite (NS.union ns1 ns2))
    | (Single n), (CoFinite ns)     -> if NS.mem n ns then None else Some r1
    | (CoFinite ns), (Single n)     -> if NS.mem n ns then None else Some r2
  let finite_domain = function
      Single n -> Some (NS.singleton n)
    | _ -> None
  let cofinite_domain = function
      CoFinite ns -> Some ns
    | _ -> None
  let is_singleton = function Single _ -> true | _ -> false
  let is_finite = is_singleton
  let singleton_domain = function
      Single n -> Some n 
    | _ -> None
  let mem n = function 
      Single m -> m=n
    | CoFinite ns -> not (NS.mem n ns)
end 

module R = RegExp

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

(* an element is a pair of a location and a state *)
type elt = R.t * state

(* syntactic reprsentations of schemas *)
type syn_t = 
    Var of state 
  | Atom of Name.t * syn_t 
  | Wild of Name.Set.t * int * bool * syn_t
  | Cat of syn_t list
  | Union of syn_t list
  | Isect of syn_t list
  | Neg of syn_t
  | Restrict of syn_t * Name.Set.t * bool
  | Inject of syn_t * syn_t
  | InjectMap of syn_t * (syn_t Name.Map.t)

(* the type t of schemas is either a variable given by a state, or a a
   counting formula over a proper basis *)
type t = V of state | F of (Presburger.t * elt list) * syn_t

(* --------------- conversions / simple operations --------------- *)
let cset_of_cvar cx = CSet.singleton cx
let cset_of_tvar x = cset_of_cvar (NS.singleton x, NS.empty)
let cstate_of_tvar x = CS (cset_of_tvar x)
let is_cvar s = CSet.cardinal s = 1
let is_tvar s = (is_cvar s) && 
  let (is,ds) = CSet.choose s in
    (NS.cardinal is = 1) && (NS.is_empty ds)
let is_var = function V _ -> true | _ -> false

let syn_of_t t0 = match t0 with
    V(x) -> Var(x)
  | F(_,si) -> si

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

let fformat_state fmtr s0 = Format.fprintf fmtr "%s" (string_of_state s0)
let format_state = fformat_state Format.std_formatter

let fformat_elt fmtr (r,x) = 
  R.fformat_t fmtr r;
  Format.fprintf fmtr "[";
  fformat_state fmtr x;
  Format.fprintf fmtr "]"

(* --------------- maps, sets, environments, formatting ------------------ *)
type this_t = t (* hack! *)

module TVarEnv = Env.Make(
  struct
    type t = tvar
    let compare = compare
    let to_string x = x
  end)

let delta_tvar : (t TVarEnv.t ref) = ref (TVarEnv.empty ())
let update_tvar x t = delta_tvar := (TVarEnv.update (!delta_tvar) x t)
let lookup_tvar x = TVarEnv.lookup (!delta_tvar) x
let lookup_tvar_required x = match lookup_tvar x with 
    Some t -> t
  | None -> raise (Error.Harmony_error 
                     (fun () -> Format.printf 
                        "Treeschema.lookup_tvar_required: %s not found" x))

(* pretty printing comes here; it only needs to know about tvars, but we need string_of_t 
to instantiate Mapplus functor for full environments below *)

type format_mode = FCat | FSimpleCat | FUnion | FInter | FNone

let fformat_syn_t fmtr syn_t0 = 
  (* gather up all the tvars mentioned in a syn_t *)
  let rec collect acc = function
      [] -> acc
    | x::xs -> begin match x with 
          Var(CS s) -> 
            (* Format.printf "NEW VAR: %s@\n" (string_of_cset s);*)
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

        | Atom(_,t) | Wild(_,_,_,t) | Neg(t) 
        | Restrict(t,_,_) -> collect acc (t::xs)
        | Inject(t1,t2) -> 
            collect acc (t1::t2::xs)
        | InjectMap(t1,tm) -> 
            let tms = NM.fold (fun _ ti acc -> ti::acc) tm [] in
              collect acc (t1::tms@xs)
      end in
  let rec format_one mode = function
      Var(x) -> fformat_state fmtr x
    | Atom(n,t) -> 
        Format.fprintf fmtr "@[";
        if mode <> FSimpleCat then Format.fprintf fmtr "{"; 
        Format.fprintf fmtr "%s=" (Misc.whack n);
        format_one FNone t;
        if mode <> FSimpleCat then Format.fprintf fmtr "}";
        Format.fprintf fmtr "@]"
    | Wild(f,l,u,t) -> 
        Format.fprintf fmtr "@[";
        if mode <> FSimpleCat then Format.fprintf fmtr "{"; 
        (match l,u with 
             0,true -> Format.printf "*"
           | n,b -> 
               for i=1 to n do Format.printf "!" done; 
               if b then Format.printf "*");
        if not (Name.Set.is_empty f) then
          (Format.printf "\\(@[";
           Misc.fformat_list fmtr ",@ " 
             (Format.fprintf fmtr "%s") 
             (Safelist.map Misc.whack (Name.Set.elements f));
           Format.printf "@])");
        Format.printf "=@,";
        format_one FNone t;
        if mode <> FSimpleCat then Format.fprintf fmtr "}"; 
        Format.printf "@]" 
    | Cat(ts)    -> 
        let is_atoms = Safelist.for_all 
          (function Atom(_) | Wild(_) -> true | _ -> false) ts in 
        Format.fprintf fmtr "@[";
          (match mode,is_atoms with 
               FSimpleCat, true | FCat,false -> ()
             | _,true -> Format.fprintf fmtr "{"            
             | _ -> Format.fprintf fmtr "(");
          (if is_atoms then Misc.fformat_list fmtr ",@ " 
           else Misc.fformat_list fmtr " +@ ") 
            (format_one (if is_atoms then FSimpleCat else FCat)) ts;
          (match mode,is_atoms with 
               FSimpleCat, true | FCat,false -> ()
             | _,true -> Format.fprintf fmtr "}"            
             | _ -> Format.fprintf fmtr ")");            
          Format.fprintf fmtr "@]"
    | Union(ts)  -> 
        Format.fprintf fmtr "@[";
        if mode <> FUnion then Format.fprintf fmtr "("; 
        Misc.fformat_list fmtr " |@ " (format_one FUnion) ts;
        if mode <> FUnion then Format.fprintf fmtr ")";
        Format.fprintf fmtr "@]"
    | Isect(ts)  ->
        Format.fprintf fmtr "@[";
        if mode <> FInter then Format.fprintf fmtr "("; 
        Misc.fformat_list fmtr " &@ " (format_one FInter) ts;
        if mode <> FInter then Format.fprintf fmtr ")";
        Format.fprintf fmtr "@]"
    | Neg(t)     -> 
        Format.fprintf fmtr "~"; 
        format_one FNone t;
    | Inject(t1,t2) -> 
        Format.fprintf fmtr "Inject(";
        format_one FNone t1;
        Format.fprintf fmtr ",@ ";
        format_one FNone t2;
        Format.fprintf fmtr ")"
    | InjectMap(t1,tms) -> 
        Format.fprintf fmtr "InjectMap(";
        format_one FNone t1;
        Format.fprintf fmtr ",@ @[{";
        let tms_size = NM.size tms in 
        let _ = NM.fold 
          (fun ni ti count -> 
             Format.fprintf fmtr "%s -> " (Misc.whack ni);
             format_one FNone ti;
             if count < tms_size - 1 then Format.fprintf fmtr ",@ ";
             succ count)
          tms 0 in 
          Format.fprintf fmtr "}@])"
    | Restrict(t,ns,b) -> 
        Format.fprintf fmtr "Restrict(";
        format_one FNone t;
        Format.fprintf fmtr ",%s{%s})"
          (if b then "" else "~")
          (Misc.concat_list "," (Name.Set.elements ns)) in          

  (* main formatter *)
  let var_map = collect NM.empty [syn_t0] in
    Format.fprintf fmtr "@[@[<2>";
    format_one FNone syn_t0;
    Format.fprintf fmtr "@]";
    if not (NM.is_empty var_map) then 
      begin 
        Format.fprintf fmtr "@\nwhere @[";
        let _ = NM.fold (fun tvi syn_tvi first -> 
                           if not first then Format.fprintf fmtr "@\n";
                           Format.fprintf fmtr "@[<2>%s@ =@ " tvi;
                           format_one FNone syn_tvi;
                           Format.fprintf fmtr "@]";
                           false)
          var_map true in
          Format.fprintf fmtr "@]"
      end;
    Format.fprintf fmtr "@]"

let fformat_t fmtr t0 = match t0 with
    V(x) -> fformat_state fmtr x
   | F((p,e),syn) -> fformat_syn_t fmtr syn;
       Trace.debug "schema-formulas" 
         begin           
           let thk () = 
             Format.print_newline ();
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
                          (NM.add tvi ti new_acc, ti::new_xs))
                     new_vars (acc,xs) in
                     collect new_acc new_xs
               | V(_)::xs -> collect acc xs
               | F((_,e),_)::xs -> 
                   collect acc ((Safelist.map (fun (_,x) -> V(x)) e)@xs) 
             in
             let format_one = function
                 V(s) -> 
                   Format.fprintf fmtr "V["; 
                   format_state s; 
                   Format.fprintf fmtr "]"
               | F((p,e),_) -> 
                   Format.fprintf fmtr "F[@[";
                   Presburger.fformat_t fmtr p;
                   Format.fprintf fmtr "@]@,.@[";
                   Misc.fformat_list fmtr ",@ " 
                     (fun (d,e) -> Format.fprintf fmtr "n%d=" d; fformat_elt fmtr e) 
                     (Misc.map_index_filter (fun n ei -> Some (n,ei)) e);
                   Format.fprintf fmtr "@]]" in
             let vmap = collect NM.empty [t0] in
               Format.fprintf fmtr "@[@[<2>";
               format_one t0;
               Format.fprintf fmtr "@]";
               if not (NM.is_empty vmap) then 
                 begin 
                   Format.fprintf fmtr "@\nwhere @[";
                   let _ = NM.fold 
                     (fun tvi ti first -> 
                        if not first then Format.fprintf fmtr "@\n";
                        Format.fprintf fmtr "@[<2>%s@ =@ " tvi;
                        format_one ti;
                        Format.fprintf fmtr "@]";
                        false)
                     vmap true in
                     Format.fprintf fmtr "@]"
                 end;
               Format.fprintf fmtr "@]"
           in
             thk
         end

             

let format_elt = fformat_elt Format.std_formatter
let format_t = fformat_t Format.std_formatter

let string_of_t t0 = 
  let buf = Buffer.create 20 in 
  let fmtr = Format.formatter_of_buffer buf in 
    fformat_t fmtr t0;
    Format.pp_print_flush fmtr ();
    Buffer.contents buf

(* we maintain two environments: 
   - delta_tvar maps tvars to ts
   - delta_tvar maps compound type states to ts
   we could (and did, in an earlier version) use a unified
   environment, but lookups are slower when we have a simple type
   variable *)

let format_low_level msg p e = 
  Format.printf "%s @[p=@[" msg;
  P.format_t p;
  Format.printf "@]@, e=@[";
  Misc.format_list "," (fun (r,x) -> R.format_t r; Format.printf "["; format_state x; Format.printf "]") e;
  Format.printf "@]@]@\n" 


module TMapplus = Mapplus.Make(
  struct
    type t = this_t
    let compare t0 t1 = match t0,t1 with
        V(s1),V(s2)     -> compare s1 s2
      | F(f1,_),F(f2,_) -> compare f1 f2
      | V(_),F(_)       -> -1
      | F(_),V(_)       -> 1
    let to_string = string_of_t
  end)
module TSet = TMapplus.KeySet
module TMap = TMapplus.Map   

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
                  "treeschema variable%s%s%s at %s appears in a non-contractive position"
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
let anyE = (R.mk_cofinite [],True)

(* true, the largest schema *)
let truth,truth_formula,truth_basis = 
  let xtruth = "T" in
  let ftruth = P.mkGe(P.mkVar 0) P.zero in
  let btruth = [anyE] in
  let syn_truth = Wild(Name.Set.empty,0,true,Var(True)) in
    update_tvar xtruth (F((ftruth, btruth),syn_truth));
    (xtruth,ftruth,btruth)
let truth_cs = cset_of_tvar truth

(* empty, the singleton schema containing V.empty *)
let empty,empty_formula,empty_basis = 
  let xempty = "{}" in
  let fempty = P.mkEq(P.mkVar 0) P.zero in
  let bempty = [anyE] in
  let syn_empty = Wild(Name.Set.empty, 0, false, Var(EmptyView)) in
    update_tvar xempty (F((fempty, bempty),syn_empty));
    (xempty,fempty,bempty)
let empty_cs = cset_of_tvar empty

(* non_empty, the schema T-{} *)
let non_empty,non_empty_formula,non_empty_basis = 
  let xnon_empty = "T-{}" in
  let fnon_empty = P.mkGt(P.mkVar 0) P.zero in
  let bnon_empty = [anyE] in
  let syn_non_empty = Wild(Name.Set.empty,1,true,Var(NonEmptyView)) in    
    update_tvar xnon_empty (F((fnon_empty, bnon_empty),syn_non_empty));
    (xnon_empty,fnon_empty,bnon_empty)
let non_empty_cs = cset_of_tvar non_empty

(* --- convert a t to a state ----- *)

(* when we create an atom or a wildcard from a subschema t, we need to
   create a basis over t. as elements are defined using states, we need
   to find a state representing t. if we already have a variable x
   representing t, then we use it. otherwise we generate a fresh x and
   bind it to t in delta_tvar *)

(* JNF-CHECK: hosing (==) by keeping syntactic representation
   alongside semantical one? *)
module THash = Hashtbl.Make(
  struct
    type t = this_t
    let equal t1 t0 = match t1,t0 with 
        V(s1),V(s2)     -> (s1 = s2)
      | F(f1,_),F(f2,_) -> (f1 == f2)
      | _               -> false
    let hash = function
        V(s1)   -> Hashtbl.hash s1
      | F(f1,_) -> Hashtbl.hash (Obj.magic f1 : int)
  end)

let varcache : (state THash.t) = THash.create 17

let state_of_t t0 = match t0 with 
    V(x) -> x
  | F((p,e),syn_t) -> 
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
            "Treeschema.lookup_cset_required: %s not found"
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
  | F((pi,ei),_) -> (pi,ei)

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
  | F((p,e),syn_t) -> F((P.mkNot p,e),Neg(syn_t))

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
      F((p,e),mk_syns (Safelist.map syn_of_t ts)) in

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

let mk_empty = V(False)

let mk_var x = V(cstate_of_tvar x)

let mk_atom n t = 
  let x = state_of_t t in 
  let syn = Atom(n,syn_of_t t) in
  let (ps,e) = 
    if x = True then
      [P.mkEq (P.mkVar 0) P.one;
       P.mkEq (P.mkVar 1) P.zero],
      [(R.mk_singleton n,True);
       (R.mk_cofinite [n], True)]
    else
      [P.mkEq (P.mkVar 0) P.one;
       P.mkEq (P.mkVar 1) P.zero;
       P.mkEq (P.mkVar 2) P.zero],
    [(R.mk_singleton n,x);
     (R.mk_singleton n,neg_state x);
     (R.mk_cofinite [n], True)] in
    F((P.mkAnd ps,e),syn) 

let mk_wild f l u t = 
  let x = state_of_t t in   
  let syn = Wild(f,l,u,syn_of_t t) in

  (* FIX LATER: replace this fold with R.from_list or something *)
  let cof_pos,fin_ps_rev,fin_es_rev = NS.fold 
    (fun ni (pos,ps,es) -> 
       (pos+1,
        (P.mkEq (P.mkVar pos) P.zero)::ps,
        (R.mk_singleton ni,True)::es))
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
    F((P.mkAnd (Safelist.rev ps_rev), Safelist.rev e_rev),syn)

let rec mk_cat = function
    [] -> V(EmptyView)
  | [t1] -> t1
  | ts -> gen_mk P.add (fun ts -> Cat(ts)) None ts

(* list schemas *)
let mk_nil = mk_cat [mk_atom Tree.nil_tag (mk_cat [])]
let mk_cons h t = mk_cat [mk_atom Tree.hd_tag h; mk_atom  Tree.tl_tag t]


(* [schema_of_tree v] yields the singleton [Value.ty] containing [v] *)
module TreeHash = Hashtbl.Make(
struct
  type t = Tree.t
  let equal = (==)
  let hash o = (Obj.magic o : int)
end)
let treecache = TreeHash.create 31
let rec t_of_tree v = 
  try TreeHash.find treecache v
  with Not_found -> 
    let s = mk_cat
      (Tree.fold (fun k vk ts -> (mk_atom k (t_of_tree vk))::ts) v []) in 
      TreeHash.add treecache v s;
      s      
    
(* --------------- operations ----------------------*)

(* --- empty test --- *)
(* cached emptiness results *)
let (empties,non_empties) = (ref TSet.empty, ref TSet.empty)

let rec empty_aux print (es,nes) t0 = 
  let res = if TSet.mem t0 es then (true,es,nes)
  else if TSet.mem t0 nes then (false,es,nes)
  else 
    let t0_empty,es',nes' = match t0 with
        V(x) -> begin match x with
            True | EmptyView | NonEmptyView -> (false,es,nes)
          | False -> (true,es,nes)
          | CS s  -> empty_aux false (TSet.add t0 es,nes) (lookup_cset_required s)
        end 
      | F((p,e),_) -> check_empty (es,nes) (p,e) in
      if t0_empty then (true,TSet.add t0 es',nes')
      else (false,es,TSet.add t0 nes)
  in
  let b_res,_,_ = res in 
    Trace.debug "treeschema+" 
      (fun () -> 
         Format.printf "@\nEMPTY AUX %b " b_res; 
         format_t t0;
         if print then begin
           Format.printf "@\nes=@[";
           Misc.format_list "@\n" format_t (TSet.elements es);
           Format.printf "@]@\nnes=@[";
           Misc.format_list "@\n" format_t (TSet.elements nes);
           Format.printf "@]@\n";
         end
         else Format.print_newline ());
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
      (fun ni db -> (R.mk_singleton ni, True)::db)
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
                  let new_nmap = NM.add n (P.mkSum (P.mkVar pos) old_sum) nmap in
                    aux (pos+1, new_nmap, x::facc) rest 
          end in
      let _,nmap,facc = aux (0,NM.empty,[]) es in 
        (nmap,facc) in
      
    let nmap, sub_formulas = collect basis in 
      
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
          let (b,es',nes') = empty_aux false (es,nes) fake_t0 in
            if b then begin 
              P.Valuation.zonk z_val pos;
              empties (TSet.add fake_t0 es', nes') (pos+1) xrest
            end
            else 
              empties (es,TSet.add fake_t0 nes') (pos+1) xrest in
    let es',nes' = empties (es,nes) 0 sub_formulas in
      Trace.debug "treeschema+" (fun () -> Format.printf "z_val: "; P.Valuation.format_t z_val; Format.print_newline ());
      let checked_formula = (P.mkAnd (formula::ft_constraints)) in      
        (* check satisfiability of formula ft, zero constraints *)
        (not (P.fast_sat checked_formula z_val),es',nes'),formula,basis in
  let real_res,formula,basis = res in
  let b_res,_,_ = real_res in
    Trace.debug "treeschema+"
      (fun () -> 
         Format.printf "--- CHECK_EMPTY --- %b" b_res;
         Format.printf "@\n(p,e) = "; 
         P.format_t formula; 
         Format.printf ","; 
         (Misc.format_list "," (fun (r,x) -> R.format_t r; Format.printf "["; format_state x; Format.printf "]") basis);
         Format.printf "@\nes=@[";
         Misc.format_list "@\n" format_t (TSet.elements es);
         Format.printf "@]@\nnes=@[";
         Misc.format_list "@\n" format_t (TSet.elements nes);
         Format.printf "@]@\n";
         Format.print_newline ());
    real_res

let empty t0 = 
  Trace.debug "treeschema+"
    (fun () -> 
       Format.printf ">>> START EMPTY@\n  @[";
       format_t t0;
       Format.print_newline ());
  let res = 
    let t0_empty,new_empties,new_non_empties = 
      empty_aux true (!empties,!non_empties) t0 in
      empties := new_empties;
      non_empties := new_non_empties;
      t0_empty in 
    
    Trace.debug "treeschema+"
      (fun () -> 
         Format.printf "@]@\n<<<EMPTY %b\t" res;
         format_t t0;
         Format.print_newline ());
    res
  
let subschema t0 t1 = empty (mk_diff t0 t1)

let equivalent t0 t1 = subschema t0 t1 && subschema t1 t0

(* --- member --- *)
let total_member_tests = ref 0
let member v t0 = 
  let rec aux print v t0 = match t0 with
    V(x) -> begin match x with 
        True -> true
      | False -> false
      | EmptyView -> Tree.is_empty v
      | NonEmptyView -> not (Tree.is_empty v)
      | CS s -> aux print v (lookup_cset_required s) 
    end
  | F((p,e),_) -> 
      let old_tmts = !total_member_tests in
        incr total_member_tests;
        (* count the basis elts that the elts of v belongs to *)
        (* this code is critical. we use an Array.t for speed *)
        let nbits,nints = Safelist.fold_left 
          (fun (b,i) (r,x) -> 
             if i>0 then (b,succ i)
             else if R.is_singleton r then (succ b, i)
             else (b, succ i))
          (0,0) 
          e in
        let counts = P.Valuation.create nbits nints false in
        let not_falsified = Tree.fold
          (fun ni vni mem_so_far ->
             let rec count_elts pos mem_so_far = function
                 [] -> assert false (* can't happen, because e is a basis *)
               | (r,x)::t -> 
                   let yes () = P.Valuation.bump counts pos in
                   let no mem_res = count_elts (pos+1) mem_res t in
                   let on_match mem_fun = function
                       EmptyView    -> 
                         if Tree.is_empty vni then (yes (); mem_fun true)
                         else no (mem_fun false)
                     | True         -> yes (); mem_fun true
                     | False        -> no (mem_fun false)
                     | NonEmptyView -> 
                         if Tree.is_empty vni then no (mem_fun false) 
                         else (yes (); mem_fun true)
                     | CS s         -> 
                         if aux false vni (lookup_cset_required s) 
                         then (yes (); mem_fun true)
                         else no (mem_fun false) in
                     (* note: could short circuit out of this when false *)
                     if not mem_so_far then false 
                     else begin 
                       if R.is_singleton r then 
                         let mk_mem_fun b = 
                           let v = P.Valuation.create nbits nints true in
                             if b then P.Valuation.bump v pos
                             else P.Valuation.zonk v pos;
                             P.fast_sat p v 
                         in
                           if R.mem ni r then on_match mk_mem_fun x
                           else no true
                       else 
                         if R.mem ni r then on_match (fun _ -> true) x
                         else no true
                     end
             in count_elts 0 mem_so_far e)
          v true in

        (* final result: check if the formula is satisfiable *)
        let res = not_falsified && P.fast_sat p counts in
          if print then 
            Trace.debug "member" 
              (fun () -> 
                 Format.printf "MEMBER (%d)" (!total_member_tests - old_tmts);
                 Format.printf "@\n v : "; Tree.format_t v;
                 Format.printf "@\nt0 : "; format_t t0;
                 Format.printf "@\n  = %b" res;
                 Format.print_newline ());
          res in 
    aux true v t0

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
  | F((p,e),_) -> 
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
                  | CS s  -> if empty (lookup_cset_required s) then no () else yes () in
                  if R.mem k r then on_match x else no ())
             (0,new_vars,nmap,vmap) 
             e in
             (* result of Safelist.fold_left *)
             (new_vars,nmap,vmap))
        ns 
        (0,NM.empty,IM.empty)in
      let rec aux_sum acc = function [] -> acc | h::t -> aux_sum (P.mkSum (P.mkVar h) acc) t in        
      let f1 = NM.fold (fun _ xl acc -> (P.mkEq (aux_sum P.zero xl) P.one)::acc) nmap [] in
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
  | F((p,e),_)   ->           
      let e_length = Safelist.length e in
      let eks = Misc.map_index_filter
        (fun pos (r,x) -> 
           if R.mem k r then 
             let pi_val = P.Valuation.create e_length 0 true in 
               P.Valuation.bump pi_val pos;
               if P.fast_sat p pi_val then
                 match x with 
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
  | F((p,e),_) -> 
      let e_length = Safelist.length e in
      let _,ok,reso = Safelist.fold_left
        (fun (pos,ok,reso) (_,x) -> 
           if not ok then (pos,ok,reso) 
           else let pi_val = P.Valuation.create e_length 0 true in 
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
    (true,ns1) -> NS.fold (fun n b -> b || (R.isect (R.mk_singleton n) r = None)) ns1 false
  | (false,ns1) -> R.isect (R.mk_cofinite (NS.elements ns1)) r = None  

let complete_basis_rev x_count formulas basis_rev = function
    (* if covered is a cofinite set *)
    (false,ns) -> 
      if NS.is_empty ns then formulas,basis_rev
      else
        let _,formulas,basis_rev = NS.fold
          (fun ni (x_pos,formulas,basis_rev) -> 
             (x_pos+1,P.mkEq (P.mkVar x_pos) P.zero::formulas,
              (R.mk_singleton ni,True)::basis_rev))
          ns
          (x_count,formulas,basis_rev) in 
          formulas,basis_rev
            
  (* covered is a finite set *)
  | (true,ns) -> 
      (P.mkEq (P.mkVar x_count) P.zero::formulas,
       ((R.mk_cofinite (NS.elements ns),True)::basis_rev)) 
   
let inject t0 t1 =   
  let formula,basis = expose t0 in 
  let basis_length = Safelist.length basis in 

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
         let pi_val = P.Valuation.create basis_length 0 true in            
           if R.is_singleton r then 
             (P.Valuation.bump pi_val pos;
              P.fast_sat formula pi_val)
           else 
             let pos_var = P.mkVar pos in 
             let constr_formula = P.mkAnd [P.mkGt pos_var P.zero; formula] in
               match R.finite_domain r with 
                   Some ns -> 
                     let lt_constr = P.mkLe pos_var (P.mkConst (NS.cardinal ns)) in
                       P.fast_sat 
                         (P.mkAnd [ lt_constr; constr_formula ])
                         pi_val
                 | None -> P.fast_sat constr_formula pi_val in 

         if (fds_isect_regexp covered r) (* only correct because of proj_all *) 
           || not pos_satisfiable then do_zero ()
         else do_found ())
    (0,0,[],[],[],(true,NS.empty))
    basis in

  (* add some zero-d elements to make this a basis *)
  let formulas,basis_rev = complete_basis_rev x_count formulas basis_rev covered in
  let res_formula = P.mkAnd ((P.substitute subst formula)::formulas) in
  let res_basis = Safelist.rev basis_rev in 
  let res_syn = Inject(syn_of_t t0, syn_of_t t1) in
    (* inject result *)
    F((res_formula,res_basis),res_syn)
      
let inject_map t0 fm = 
  let basis, basis_length, formula = 
    let p,e = expose t0 in       
    let ns = NM.domain fm in 
    let ns_e = NS.fold 
      (fun ni acc -> (R.mk_singleton ni,True)::acc) 
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
    F((res_formula,res_basis),res_syn)
      
let restrict ns t0 = 
  (* FIX LATER: see if we can avoid refining once we have full RegExp functionality *)
  let restrict_aux (p,e) syn = 
    (* (1) rewrite p and e so that no ns intersect a CoFinite element of e *)
    let ns_e = NS.fold (fun ni acc -> (R.mk_singleton ni,True)::acc) 
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
      (fun ni (x,b,fs) -> (x+1,(R.mk_singleton ni,True)::b, (P.mkEq (P.mkVar x) P.zero)::fs))
      ns (neg_ns_xs,neg_ns_basis_rev,neg_ns_fs) in
      
    let ns_basis = Safelist.rev ns_basis_rev in 
    let neg_ns_basis = Safelist.rev neg_ns_basis_rev in
      
    (* (4) construct the forumlas, bases *)
    let ns_f = P.wrap basis_length (P.mkAnd (formula::ns_fs)) in
    let neg_ns_f = P.wrap basis_length (P.mkAnd (formula::neg_ns_fs)) in
    let restr_ns = F((ns_f,ns_basis),Restrict(syn,ns,true)) in
    let restr_neg_ns = F((neg_ns_f,neg_ns_basis),Restrict(syn,ns,false)) in

      Trace.debug "restrict+" 
        (fun () -> 
           format_low_level (sprintf "NS RESULT {%s}" (Misc.concat_list "," (NS.elements ns))) ns_f ns_basis;
           format_low_level (sprintf "NEG NS RESULT {%s}" (Misc.concat_list "," (NS.elements ns))) neg_ns_f neg_ns_basis);

      (restr_ns, restr_neg_ns) in
    match t0 with 
        V(x) -> begin match x with 
            False | EmptyView -> (V(x),V(x))
          | _ -> restrict_aux (expose t0) (syn_of_t t0)
        end
      | F((p,e),syn) -> restrict_aux (p,e) syn

(* external exports *)
let update = update_tvar
