(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* presburger.ml - presburger arithmetic constraints     *)
(*********************************************************)
(* $Id$ *)

let sprintf = Printf.sprintf

module IntMapplus =
  Mapplus.Make(
    struct
      type t = int
      let compare = compare
      let to_string = string_of_int
    end)
module IntMap = IntMapplus.Map
module IntSet = IntMapplus.KeySet

(* -------------- abstract syntax ------------- *)
(* exps are only used for easy construction of EqZs and GeqZs. *)
type exp = 
    Const of int
  | Var of int 
  | Sum of exp * exp 

(* formulas, represented in de Bruijn notation *)
module rec F : sig 
  type t = 
      EqZ of int IntMap.t * int
    | GeqZ of int IntMap.t * int
    | Not of t 
    | Or of TSet.t
    | And of TSet.t
    | Exists of t
  val compare : t -> t -> int 
end = struct
  type t = 
      EqZ of int IntMap.t * int
    | GeqZ of int IntMap.t * int
    | Not of t
    | Or of TSet.t
    | And of TSet.t
    | Exists of t
  let rec compare t1 t2 = match t1,t2 with 
      EqZ(vs1,c1),EqZ(vs2,c2) 
    | GeqZ(vs1,c1),GeqZ(vs2,c2)
        -> 
        let cmp1 = Pervasives.compare c1 c2 in         
          if cmp1 <> 0 then cmp1 
          else 
            let cmp2, dom1 = IntMap.fold 
              (fun xi w1 (cmp,dom1) -> 
                 let dom1' = IntSet.add xi dom1 in 
                 let cmp' = 
                   if cmp <> 0 then cmp
                   else 
                     try 
                       let w2 = IntMap.find xi vs2 in 
                         Pervasives.compare w1 w2
                     with Not_found -> -1 in 
                   (cmp',dom1'))
              vs1 (0,IntSet.empty) in 
              if cmp2 <> 0 then cmp2 
              else 
                let dom2 = IntMap.domain vs2 in 
                  IntSet.compare dom1 dom2
    | Not(t1),Not(t2) | Exists(t1),Exists(t2) -> 
        compare t1 t2
    | Or(ts1),Or(ts2) | And(ts1),And(ts2) -> 
        TSet.compare ts1 ts2    
    | EqZ(_),_  -> -1
    | _,EqZ(_)  -> 1
    | GeqZ(_),_ -> -1
    | _,GeqZ(_) -> 1
    | Not(_),_   -> -1
    | _,Not(_)   -> 1
    | Or(_),_   -> -1
    | _,Or(_)   -> 1
    | And(_),_   -> -1
    | _,And(_)   -> 1          
end 
and TSet : Set.S with type elt = F.t = 
  Set.Make(
    struct
      type t = F.t
      let compare = F.compare
    end)

open F
type t = F.t

let map_set fold add empty f s = fold 
  (fun ei acc -> add (f ei) acc) 
  s
  empty

let map_map fold add empty f m = fold 
  (fun k v acc -> let k',v' = f k v in add k' v' acc)
  m
  empty

let map_int_set = map_set IntSet.fold IntSet.add IntSet.empty 
let map_int_map = map_map IntMap.fold IntMap.add IntMap.empty
let map_t_set = map_set TSet.fold TSet.add TSet.empty

(* --------------- utility functions --------------- *)
(* --- free variables --- *)
let rec fvs_t = function
    EqZ(vs,_) | GeqZ(vs,_) -> IntMap.domain vs 
          
  | Not(f)    -> fvs_t f

  | Or(fs) | And(fs) -> TSet.fold 
      (fun fi fvs -> IntSet.union (fvs_t fi) fvs)
        fs
        IntSet.empty        
        
  | Exists(f)    -> 
      IntSet.fold 
        (fun x s -> if (x <> 0) then (IntSet.add (x-1) s) else s) 
        (fvs_t f)
        IntSet.empty 

(* --- formatter --- *)
let rec fformat_exp_aux fmtr = function
    Const(n) -> Format.fprintf fmtr "%d" n
  | Var(x) -> Format.fprintf fmtr "n%d" x
  | Sum(e1,e2) -> 
      fformat_exp_aux fmtr e1; 
      Format.fprintf fmtr "+";
      fformat_exp_aux fmtr e2

let format_exp = fformat_exp_aux Format.std_formatter
  
let rec fformat_t_aux fmtr g t0 = 
  let format_constraint vs opr c = 
(*     let _ =  *)
(*       if false then ( *)
(*       Util.format " DEBUG("; *)
(*       IntMap.iter_with_sep  *)
(*         (fun xi wi -> Util.format "%dn%d" wi xi) *)
(*         (fun () -> Util.format "+") *)
(*         vs; *)
(*       Util.format ",%d) " c) in *)
    let non_neg, neg = IntMap.partition (fun _ wi -> wi >= 0) vs in 
    let pos, zero = IntMap.partition (fun _ wi -> wi > 0) non_neg in 
    let pos_empty = IntSet.is_empty (IntMap.domain pos) in 
    let neg_empty = IntSet.is_empty (IntMap.domain neg) in 
      if pos_empty && neg_empty then 
        Format.fprintf fmtr "%d=%d" 
          (if c >=0 then c else 0)
          (if c < 0 then -c else 0)
      else 
        begin 
          if pos_empty then Format.fprintf fmtr "%d" (if c >= 0 then c else 0)
          else (IntMap.iter_with_sep 
                  (fun xi wi -> Util.format "%sn%d" 
                     (if wi=1 then "" else sprintf "%d" wi) xi)
                  (fun () -> Util.format "+")
                  pos;
                if c > 0 then Format.fprintf fmtr "+%d" c);
          Format.fprintf fmtr "%s" opr;
          if neg_empty then Format.fprintf fmtr "%d" (if c < 0 then -c else 0) 
          else 
            (IntMap.iter_with_sep 
               (fun xi wi -> Util.format "%sn%d" 
                  (if wi=(-1) then "" else sprintf "%d" (-wi)) xi)
               (fun () -> Util.format "+")
               neg;
             if c < 0 then Format.fprintf fmtr "%d" (-c))
        end in 
    match t0 with        
        EqZ(vs,c) -> format_constraint vs "=" c
      
      | GeqZ(vs,c) -> format_constraint vs ">=" c
          
      | Not(f1) -> 
          Format.fprintf fmtr "!("; 
          fformat_t_aux fmtr g f1;
          Format.fprintf fmtr ")"
      | Or(fs) ->
          Format.fprintf fmtr "(";
          Misc.fformat_list fmtr  " | " (fformat_t_aux fmtr g) (TSet.elements fs);
          Format.fprintf fmtr ")";
      | And(fs) ->
          Format.fprintf fmtr "(";
          Misc.fformat_list fmtr " & " (fformat_t_aux fmtr g) (TSet.elements fs);
          Format.fprintf fmtr ")"
      | Exists(f1) -> 
          let next (qd,g) = (qd+1,fun n->if n=0 then sprintf "x%d" qd else g (n-1)) in
          let rec format_exs g = function
              Exists(f1) -> 
                Format.fprintf fmtr ",x%d" (fst g);
                format_exs (next g) f1
            | f -> 
                Format.fprintf fmtr ".";
                fformat_t_aux fmtr g f in
            Util.format "EX x%d" (fst g);
            format_exs (next g) f1

let fformat_t fmtr f = 
  fformat_t_aux fmtr (0,fun n -> sprintf "n%d" n) f

let format_t = fformat_t Format.std_formatter

(* --------------- de Bruijn shifting -------------- *)
(* --- shift with cutoff --- *)
let rec shift_exp_aux n c e0 = match e0 with
    Const(n)   -> e0
  | Var(x)     -> if x < c then e0 else Var(x+n)
  | Sum(e1,e2) -> Sum(shift_exp_aux n c e1, shift_exp_aux n c e2)

let rec shift_aux n c = 
  let shift_var x wi = 
    let y = if x < 0 then -x else x in 
    let pre_res = if y<c then y else y+n in 
      if x < 0 then (-pre_res,wi) else (pre_res,wi) in 
    function 
        EqZ(vs,c)  -> EqZ(map_int_map shift_var vs, c)
      | GeqZ(vs,c) -> GeqZ(map_int_map shift_var vs, c)
      | Not(f)     -> Not(shift_aux n c f)
      | Or(fs)     -> Or(map_t_set (shift_aux n c) fs)
      | And(fs)    -> And(map_t_set (shift_aux n c) fs)
      | Exists(f)  -> Exists(shift_aux n (c+1) f)

(* --- top-level shift operator --- *)
let shift_exp n = shift_exp_aux n 0
let shift_t n f = 
  Trace.debug "presburger+" (fun() -> Util.format "SHIFT: "; format_t f);
  let res = shift_aux n 0 f in 
    Trace.debug "presburger+" (fun() -> Util.format " -> "; format_t res); 
    res
    
(* -------------- substitution -------------- *)
let combine_constraints (vs1,c1) (vs2,c2) = 
  let vs' = IntSet.fold 
    (fun xi vs' -> 
       let w1 = IntMap.safe_find xi vs1 0 in 
       let w2 = IntMap.safe_find xi vs2 0 in 
         IntMap.add xi (w1+w2) vs')
    (IntSet.union (IntMap.domain vs1) (IntMap.domain vs2))
    IntMap.empty in 
  let c' = c1+c2 in 
    (vs',c')
  
let rec constraint_of_exp is_lhs = function 
    Const(n) -> 
      (IntMap.empty,if is_lhs then n else -n)
  | Var(x) -> 
      (IntMap.add x (if is_lhs then 1 else -1) IntMap.empty, 0)
  | Sum(e1,e2) -> 
      combine_constraints 
        (constraint_of_exp is_lhs e1)
        (constraint_of_exp is_lhs e2)

let rec substitute_exp es e0 = match e0 with 
    Const(_) -> e0
  | Var(x) -> (try Safelist.assoc x es with Not_found -> e0)
  | Sum(e1,e2) -> Sum(substitute_exp es e1,substitute_exp es e2)
        
let rec substitute es t0 = 
  (* Util.format "@\n--- SUBSTITUTE ---@\nSUBS={";
  Misc.format_list "," (fun (xi,ei) ->  Util.format "n%d->" xi; format_exp ei;) es;
  Util.format "}@\nt0=";
  format_t t0; 
  Util.format "@\n"; *)
  let substitute_constraint (vs,c) = 
    let mult (vs,c) n = (map_int_map (fun xi wi -> (xi,n*wi)) vs, c*n) in 
      IntMap.fold 
        (fun xi wi acc ->
           try 
             combine_constraints
               (mult (constraint_of_exp true (Safelist.assoc xi es)) wi)
               acc
           with Not_found -> 
             let (vs,c) = acc in
               (IntMap.add xi wi vs,c))
        vs 
        (IntMap.empty,c) in
  let res = match t0 with
      EqZ(vs,c) -> 
        let vs',c' = substitute_constraint (vs,c) in 
          EqZ(vs',c')
    | GeqZ(vs,c) -> 
        let vs',c' = substitute_constraint (vs,c) in 
          GeqZ(vs',c')
    | Not(f)       -> Not(substitute es f)
    | Or(fs)       -> Or(map_t_set (substitute es) fs)
    | And(fs)      -> And(map_t_set (substitute es) fs)
    | Exists(f)    ->
        let shifted_es = Safelist.map 
          (fun (x,e) -> (x+1, shift_exp 1 e)) es in
          Exists(substitute shifted_es f) in 
(*     Util.format "RES="; *)
(*     format_t res; *)
(*     Util.format "@\n"; *)
    res
            
(* --------------- constants --------------- *)

let zero = Const(0)
let one = Const(1)
let tru = EqZ(IntMap.empty,0)
let fls = EqZ(IntMap.empty,1)

(* --------------- constructors --------------- *)

let mkConst n = Const(n)

let mkVar x = Var(x)

let mkSum e1 e2 = match e1,e2 with
    Const(0),_ -> e2
  | _,Const(0) -> e1
  | _ -> Sum(e1,e2)

let mkNot f = Not(f)

let mkOr fs = 
  if fs = [] then fls
  else
    let fs' = Safelist.fold_left 
      (fun acc fi -> 
         (match fi with
              Or(gs) -> TSet.union gs acc
            | _      -> TSet.add fi acc))
      TSet.empty fs in
      Or(fs')

let mkAnd fs = 
  if fs = [] then tru
  else
    let fs' = Safelist.fold_left 
      (fun acc fi -> 
         (match fi with
              And(gs) -> TSet.union gs acc
            | _      -> TSet.add fi acc))
      TSet.empty fs in
      And(fs')

let mkExists f = Exists(f)
  
let rec wrap n f = if n <= 0 then f else wrap (n-1) (mkExists f)

(* --- helper function for constructing formulas from exps --- *)
let mkEq e1 e2 = 
  let res1 = constraint_of_exp true e1 in 
  let res2 = constraint_of_exp false e2 in
  let vs,c = combine_constraints res1 res2 in 
    EqZ(vs,c) 
let mkGe e1 e2 = 
  let res1 = constraint_of_exp true e1 in 
  let res2 = constraint_of_exp false e2 in
  let vs,c = combine_constraints res1 res2 in 
    GeqZ(vs,c)
      
let mkGt e1 e2 = 
  let res1 = constraint_of_exp true e1 in   
  let res2 = constraint_of_exp false e2 in     
  let vs,c = combine_constraints res1 res2 in 
    GeqZ(vs,c-1)
      
let mkLt e1 e2 = mkGt e2 e1
let mkLe e1 e2 = mkGe e2 e1
  
(* --- get_zeros: calculate a set of "obviously zero" variables from a
   formula. NOT semantically complete--i.e., it's not the case that 
   n in get_zeros(f) <=> every satisfying valuation of f has n=0 *)
let rec get_zeros f0 = match f0 with    
    EqZ(vs,c) -> 
      if c = 0 then 
        match IntMap.fold 
          (fun xi wi acc -> match acc with 
               (false,_)         -> acc
             | (true,None)       -> (true, Some (IntSet.add xi IntSet.empty, wi > 0))
             | (true,Some(zs,p)) -> ((wi > 0) = p,Some (IntSet.add xi zs, p)))
          vs (true,None)  
        with false,_ | _,None -> IntSet.empty
          | true,Some(zs,_) -> zs
      else IntSet.empty
  | GeqZ(_) -> IntSet.empty
  | Not(f) -> begin match f with
        Not(g) -> get_zeros g
      | _        -> IntSet.empty
    end
  | Or(fs) -> 
      begin match TSet.fold (fun fi acc -> (get_zeros fi)::acc) fs [] with
          [] -> IntSet.empty
        | h::t -> Safelist.fold_left IntSet.inter h t
      end
  | And(fs) ->
      begin match TSet.fold (fun fi acc -> (get_zeros fi)::acc) fs [] with
          [] -> IntSet.empty
        | h::t -> Safelist.fold_left IntSet.union h t
      end
  |  Exists(f) -> IntSet.fold
       (fun xi u -> IntSet.add (xi-1) u) 
        (IntSet.remove 0 (get_zeros f))
        IntSet.empty

(* --------------- addition -------------- *)

(* given two formulas, f1 and f2, a formula representing (f1+f2) can
   be constructed by: 
   
   (1) instantiating each with fresh existentially-quantified
   variables and
   
   (2) adding constraints that each original variable is equal to the
   sum of the corresponding fresh variables. 

   however, omega is limited to a fixed number of variables, so
   whenever possible we try to avoid introducing unecessary
   variables. for add, we can avoid introducing fresh variables for
   variables that only appear in one formula, or for variables that
   are "obviously zero" (adding to zero is the identity. *)
let add2 f1 f2 =
  (* calculate free variables *)
  let fv1,fv2 = fvs_t f1, fvs_t f2 in
  let common_fvs = IntSet.inter fv1 fv2 in 

  (* calculate the "obviously zero" variables *)
  let zs1,zs2 = get_zeros f1, get_zeros f2 in
  
  (* construct a formula equisatisfiable with f1 and f2 where some
     "obviously zero" variables is instantiated with the constant
     zero. we need to be careful: blindly substituting for ALL
     obviously zero variables in both formulas would lose the
     constraint entirely. thus, we only instantiate obviously zero
     variables that (a) are free in both formulas, and (b) we
     instantiate each obviously zero variable in at most one of f1 and
     f2. *)
  let f1_zeros = IntSet.inter common_fvs zs1 in 
  let f1_zeroed = substitute (Safelist.map (fun z -> (z,zero)) (IntSet.elements f1_zeros)) f1 in
  let f2_zeros = IntSet.inter common_fvs (IntSet.diff zs2 zs1) in 
  let f2_zeroed = substitute (Safelist.map (fun z -> (z,zero)) (IntSet.elements f2_zeros)) f2 in   
  let remaining_common_vars = IntSet.diff common_fvs (IntSet.union f1_zeros f2_zeros) in
    Trace.debug "add+" 
      (fun () -> 
         Util.format " --- Presburger.add --- @\nf1="; 
         format_t f1; 
         Util.format "@\nf2="; 
         format_t f2;
         Util.format "@\nZS1={%s} (ALL={%s})"  
           (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements f1_zeros)))
           (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements zs1)));
         Util.format "@\nZS2={%s} (ALL={%s})" 
           (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements f2_zeros))) 
           (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements zs2)));
         Util.format "@\nf1_zeroed: "; format_t f1_zeroed;
         Util.format "@\nf2_zeroed: "; format_t f2_zeroed;
         Util.format "@\nREMAINING_COMMON: {%s}" (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements remaining_common_vars)));
         Util.format "@\n");
    
    (* next we recognize an easy common case: if f1_zeroed and
       f2_zeroed have no remaining free variables in common, then
       their sum is equivalent to their conjunction. *)
    if IntSet.is_empty remaining_common_vars then 
      let res = mkAnd[f1_zeroed;f2_zeroed] in 
        (Trace.debug "add+" (fun () -> Util.format "QUICK RES="; format_t res; Util.format "@\n"); 
         res)
    else
      (* otherwise, we fall back and introduce fresh quantified
         variables for the remaining common variables as described
         above. 
         
         because we use a debruijn encoding, calcuating the correct
         instantiation requires a little arithmetic. variables
         {0..c_common-1} are used to instantiate the common variables
         in f1 and {c_common..2*c_common-1} for the common variables
         in f2.  {2*c_common..2*c_common+c_1} are used to instantiate
         the remaining non-shared variables in f1--at the end, when we
         wrap up the formula with 2*c_common existentials, they will
         point to the same variables; similarly
         {2*c_common+c_1..2*c_common+c_1+(IntSet.cardinal
         remaining_fv2)-1} are used to instantiate the remaining
         non-shared variables in f2. *)
      let c_common = IntSet.cardinal remaining_common_vars in
      let remaining_fv1 = IntSet.diff fv1 f1_zeros in 
      let c_1 = IntSet.cardinal remaining_fv1 in 
      let remaining_fv2 = IntSet.diff fv2 f2_zeros in 
      let remaining_fvs = IntSet.union remaining_fv1 remaining_fv2 in
        Trace.debug "add+" 
          (fun () -> 
             Util.format "REMAINING_FV1 {%s}@\n" (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements remaining_fv1)));
             Util.format "REMAINING_FV2 {%s}@\n" (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements remaining_fv2)));
             Util.format "REMAINING_FVS {%s}@\n" (Misc.concat_list "," (Safelist.map string_of_int (IntSet.elements remaining_fvs)));
             Util.format "C_COMMON=%d, C_1=%d@\n" c_common c_1);        
        let init_i1 = 0 in 
        let init_o1 = 2 * c_common in 
        let init_o2 = c_1 + (2 * c_common) in 

        (* calculate instantiions for each formula and equalities, the
           equality constraints between (shifted up values of)
           variables common to both and corresponding pairs of fresh
           existential variables *)
        let _,_,f1_subst_rev,_,f2_subst_rev,equalities_rev = Safelist.fold_left 
          (fun (i_pos,o1,s1,o2,s2,e) n -> 
             match IntSet.mem n remaining_fv1, IntSet.mem n remaining_fv2 with 
                 true,true -> 
                   let x1 = mkVar i_pos in 
                   let x2 = mkVar (i_pos+c_common) in 
                   let n_shifted = mkVar (n+2*c_common) in 
                   let i_pos' = i_pos + 1 in 
                   let s1' = (n,x1)::s1 in 
                   let s2' = (n,x2)::s2 in 
                   let e' = (mkEq n_shifted (mkSum x1 x2))::e in
                     (i_pos',o1,s1',o2,s2',e')
               | true,false -> 
                   let o1' = o1 + 1 in                  
                   let s1' = (n,Var(o1))::s1 in 
                     (i_pos,o1',s1',o2,s2,e)
               | false,true -> 
                   let o2' = o2 + 1 in 
                   let s2' = (n,Var(o2))::s2 in 
                     (i_pos,o1,s1,o2',s2',e)
               | false,false -> assert false)
          (init_i1,init_o1,[],init_o2,[],[])
          (IntSet.elements remaining_fvs) in 
          
        (* instantiate f1 and f2 with vars generated by mk_assoc *)
        let f1_subst = Safelist.rev f1_subst_rev in 
        let f1_fresh = substitute f1_subst f1_zeroed in 
        let f2_subst = Safelist.rev f2_subst_rev in 
        let f2_fresh = substitute f2_subst f2_zeroed in
        let equalities = Safelist.rev equalities_rev in 
          
          Trace.debug "add+"
            (fun () -> 
               Util.format "F1_FRESH: ";
               format_t f1_fresh;
               Util.format "@\nF2_FRESH: ";
               format_t f2_fresh;
               Util.format "@\nEQUALITIES: "; 
               Misc.format_list " & " format_t equalities;
               Util.format "@\n");
          
      (* final result *)
          let res = wrap (2*c_common) (mkAnd (f1_fresh::f2_fresh::equalities)) in 
            Trace.debug "add+" (fun () -> Util.format "RES="; format_t res; Util.format "@\n");
            res

let add ts0 = match ts0 with 
    [] -> Error.simple_error "P.add: zero-length addition"
  | t::ts -> Safelist.fold_left (fun s ti -> add2 s ti) t ts 

(* ---- BIT VECTOR ---- *)
module BitVector = struct

  let bpi = Sys.word_size - 2

  type t = { data : int array;
             length : int }

  let length bv = bv.length

  let create n = 
    let data_length = if n=0 then 0 else n/bpi+1 in
      { data=Array.create data_length 0;
        length=n; }

  let pos n = (n / bpi, n mod bpi) 

  let masks = Array.init bpi (fun j -> 1 lsl j)

  let neg_masks = Array.init bpi (fun j -> max_int - masks.(j))

  let get v n =         
    let (idx,j) = pos n in 
      min 1 ((Array.unsafe_get v.data idx) land (Array.unsafe_get masks j))

  let set v n x =
    if x < 0 or x > 1 then 
      raise (Invalid_argument 
               (Printf.sprintf 
                  "BitVector.set: cannot set bit %d to %d" n x));
    let (idx,j) = pos n in
    let new_vn =   
      if x=0 then (Array.unsafe_get v.data idx) land (Array.unsafe_get neg_masks j) 
      else (Array.unsafe_get v.data idx) lor (Array.unsafe_get masks j) in
      Array.unsafe_set v.data idx new_vn

  (* same as (set v n (succ (get v n))), but inlined and w/o error checking *)
  let bump v n = 
    let (idx,j) = pos n in
    let v_idx = Array.unsafe_get v.data idx in        
      Array.unsafe_set v.data idx (v_idx lor (Array.unsafe_get masks j))

  let format_t v = 
    Util.format "[";
    for i=0 to v.length-1 do       
      Util.format "%d%s" (get v i) (if i=v.length-1 then "" else ",")
    done;
    Util.format "]"

end

(* ----- VALUATION ----- *)
module Valuation = struct  
  type t = { bit_data : BitVector.t;
             int_data : int array;
             mem_data : BitVector.t option;
             num_ints : int;
           }

  let length v = BitVector.length v.bit_data + v.num_ints

  let create nbits nints do_mems =
    { bit_data = BitVector.create nbits;
      int_data = Array.create nints 0;    
      mem_data = if do_mems then Some (BitVector.create (nbits + nints)) else None;
      num_ints = nints; }

(*   let get v n =  *)
(*     let blength = BitVector.length v.bit_data in  *)
(*       if n < blength then BitVector.get v.bit_data n  *)
(*       else Array.unsafe_get v.int_data (n - blength) *)

(*   let set v n x =  *)
(*     let blength = BitVector.length v.bit_data in  *)
(*       if n < blength then BitVector.set v.bit_data n x *)
(*       else Array.unsafe_set v.int_data (n - blength) x *)

  let zonk v n = 
    match v.mem_data with 
      None -> ()
    | Some mems -> BitVector.set mems n 1


  let bump v n = 
    zonk v n;
    let blength = BitVector.length v.bit_data in 
      if n < blength then BitVector.bump v.bit_data n
      else 
        let idx = n-blength in 
          Array.unsafe_set v.int_data idx ((Array.unsafe_get v.int_data idx)+1)

  let format_t v = 
    let concat l = List.fold_left (fun a e -> if a="" then e else Printf.sprintf "%s,%s" e a) "" l in
      Util.format "{ bit_data="; 
      BitVector.format_t v.bit_data;
      Util.format ", int_data=[%s], mem_data="
        (concat (List.map string_of_int (Array.to_list v.int_data)));
      (match v.mem_data with
           None -> Util.format "None";
         | Some mems -> BitVector.format_t mems);
      Util.format  " }@\n%!" 

let _ = Callback.register "Valuation.get_bpi" (fun () -> BitVector.bpi)
end

(* ---- OMEGA ---- *)    
let rec f2omega g o f = match f with 
    EqZ(vs,c) ->
      let l = IntMap.fold (fun xi wi acc -> (wi, (snd g) xi)::acc) vs [] in
        OmegaLibrary.add_eq o l c

  | GeqZ(vs,c) ->
      let l = IntMap.fold (fun xi wi acc -> (wi, (snd g) xi)::acc) vs [] in
        OmegaLibrary.add_geq o l c

  | Not(f1) -> 
      let not_o = OmegaLibrary.add_not o in
        f2omega g not_o f1;
        OmegaLibrary.finalize not_o

  | Or(fs) ->
      let or_o = OmegaLibrary.add_or o in 
        TSet.iter (f2omega g or_o) fs;
        OmegaLibrary.finalize or_o

  | And(fs) ->
      let and_o = OmegaLibrary.add_and o in 
        TSet.iter (f2omega g and_o) fs;
        OmegaLibrary.finalize and_o

  | Exists(f1) -> 
      let (qd,g') = g in    
      let exists_o, var_x = OmegaLibrary.add_exists o (sprintf "x%d" qd) in 
      let g' = (qd+1, fun n -> if n=0 then var_x else g' (n-1)) in
        f2omega g' exists_o f1;
        OmegaLibrary.finalize exists_o


let close f = 
  let xs = fvs_t f in 
    if IntSet.is_empty xs then f 
    else wrap ((IntSet.max_elt xs)+1) f

let empty_env n = raise 
  (Error.Harmony_error 
     (fun () ->
        Util.format "Presburger.satisfiable: unknown variable: n%d" n))

let satisfiable f = 
  let omega_init_g = (0, empty_env) in    
  let f' = close f in    
  let o = OmegaLibrary.empty (0) in
    f2omega omega_init_g o f';
    OmegaLibrary.finalize o;
    OmegaLibrary.satisfiable o

type this_t = t
module TCache = Hashtbl.Make(
  struct
    type t = this_t
    let equal = (==)
    let hash o = Hashtbl.hash (Obj.magic o : int)
  end)

module VCache = Hashtbl.Make(
  struct
    type t = Valuation.t
    let equal v1 v2 = Pervasives.compare v1 v2 = 0 (* BOGOSITY *)
    let hash = Hashtbl.hash 
  end)  

let omega_cache : ((OmegaLibrary.t * bool VCache.t) TCache.t) = TCache.create 101
let l1_hits, l1_misses = ref 0, ref (-1)
let l2_hits, l2_misses = ref 0, ref 0

let fast_sat f vs = 
  let (o,l2) = 
    try 
      let res = TCache.find omega_cache f in
        incr l1_hits;
        res
    with Not_found ->
      incr l1_misses;
      decr l2_misses; (* this one doesn't count *)
      let n = Valuation.length vs in
      let o = OmegaLibrary.empty n in
      let vcache = VCache.create 3 in
      let res = (o,vcache) in
        f2omega (0, fun k -> 
                   if k <= n-1 then OmegaLibrary.get_var o k 
                   else 
                     raise 
                       (Error.Harmony_error 
                          (fun () ->
                             Util.format "Presburger.fast_sat: unknown variable: n%d" k))                     
                ) o f;
        OmegaLibrary.finalize o;
        TCache.add omega_cache f res;
        res in
    try 
      let sat = VCache.find l2 vs in
        incr l2_hits;
        sat
    with Not_found ->
      incr l2_misses;
      let sat = OmegaLibrary.fast_sat o vs in        
        (* Util.format "res=%b@\n%!" sat; *)
        VCache.add l2 vs sat;
        sat

let print_stats () = 
  let mk_pct hits misses =
    100.0 *. (float_of_int (!hits) /.
                float_of_int ((!hits + !misses))) in
  let l1_entries,l2_total_entries,l2_number = TCache.fold
    (fun _ (_,l2) (l1, l2e, l2n) ->
       (l1 + 1, l2e + (VCache.length l2), l2n + 1))
    omega_cache
    (0,0,0) in
    Trace.debug "cache"
      (fun () ->           
           Util.format "--- Presburger cache stats ---@\n";
           Util.format "total queries: %d@\n" (!l2_hits + !l2_misses);
           Util.format "l1_cache@\n";
           Util.format "\tentries\t\t: %d@\n" l1_entries;
           Util.format "\thit rate\t: %.3f%s@\n" (mk_pct l1_hits l1_misses) "%";
           Util.format "l2_cache@\n";
           Util.format "\taverage entries\t: %.3f@\n" ((float_of_int l2_total_entries) /. (float_of_int l2_number));
           Util.format "\thit rate\t: %.2f%s" (mk_pct l2_hits l2_misses) "%";
           Util.format "@\n")

(*          let bytes2str b = 
           let thresh = 1024 in
           let fthresh = float_of_int thresh in
             if b < thresh then sprintf "%dB" b
             else let kb = (float_of_int b /. fthresh) in 
               if kb < fthresh then sprintf "%.1fKB" kb
               else let mb = kb /. fthresh in 
                 if mb < fthresh then sprintf "%.1fMB" mb 
                 else sprintf "%.1fGB" (mb /. fthresh) in
*)

(*
  let (_,_,n,bv,l1),l2_rev = Array.fold_left 
    (fun ((fill_bv,pos,n,bv,l1),l2) ai ->
       let acc1 = 
         if fill_bv && ai <= 1 then 
           let fill_bv' = n+1 < Sys.word_size in
           let bv' = bv lor (ai lsl pos) in
             (fill_bv', pos+1, n+1, bv',l1)
         else
           (false,0,n,bv,ai::l1) in
         (acc1,ai::l2))
    ((true,0,0,0,[]),[])
    a in
(*     Util.format "@\n --- array2fingerprint ---@\n";
    Util.format "a=[%s]@\n"
      (Misc.concat_list "," (Safelist.map string_of_int (Array.to_list a)));
    Util.format "fingerprint=((%d,%d),[%s])@\n" 
      n bv (Misc.concat_list "," (Safelist.map string_of_int l1));
    Util.format "to_list=[%s]@\n"
      (Misc.concat_list "," (Safelist.map string_of_int (Safelist.rev l2_rev)));
*)
    ((n::bv::l1),(Safelist.rev l2_rev))
*) 

