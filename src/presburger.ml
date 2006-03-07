(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* presburger.ml - presburger arithmetic                 *)
(*********************************************************)
(* $Id$ *)

module IntSet = Set.Make(
  struct 
    type t = int
    let compare = compare
  end)

(* -------------- abstract syntax ------------- *)
type exp = 
    Const of int
  | Var of int 
  | Sum of exp * exp
and formula = 
    Equal of Info.t * exp * exp 
  | Not of Info.t * formula
  | Or of Info.t * formula * formula
  | Exists of Info.t * formula

(* calculate the free variables of an exp or formula *)
let rec fvs =
  let rec fvs_exp = function
      Const(_)   -> IntSet.empty
    | Var(x)     -> IntSet.singleton x
    | Sum(e1,e2) -> IntSet.union (fvs_exp e1) (fvs_exp e2) 
  in function
        Equal(_,e1,e2) -> IntSet.union (fvs_exp e1) (fvs_exp e2)
    | Not(_,f)       -> fvs f
    | Or(_,f1,f2)    -> IntSet.union (fvs f1) (fvs f2)
    | Exists(_,f)    -> IntSet.fold 
        (fun x s -> if (x = 0) then vs else IntSet.add (x+1) vs)
          (fvs f)        

(* --------------- de Bruijn shifting -------------- *)
let shift n = 
  let shift_exp_aux n c = function`
      Const(n) -> Const(n) 
    | Var(x) -> if x < c then x else x + n
    | Sum(e1,e2) -> Sum(shift_exp_aux n c e1, shift_exp_aux n c e2)
  in
  let shift_aux n c = function 
      Equal(i,e1,e2) -> Equal(i, shift_exp_aux n c e1, shift_exp_aux n c e2)
    | Not(i,f)       -> Not(i,shift_aux n c f)
    | Or(i,f1,f2)    -> Or(i, shift_aux n c f1, shift_aux n c f2)
    | Exists(i,f)    -> Exists(i, shift_aux n (c+1) f)
  in
    shift_aux n 0

(** substitute: -> exp -> int -> formula -> formula *)
let substitute e x = 
  let substitute_exp e x = function
      Const(n)   -> Const(n)
    | Var(y)     -> if (x = y) then e else e0        
    | Sum(e1,e2) -> Sum(substitute e x e1, substitute e x e2)
  in function
      Equal(i,e1,e2) -> Equal(i, substitute e x f1, substitute e x f2)
    | Not(i,f)       -> Not(i, substitute e x f)
    | Or(i,f1,f2)    -> Or(i, substitute e x f1, substitute e x f2)
    | Exists(i,f)    -> Exists(i, substitute e x (shift_formula 1 f)) (* FIXME: optimize this to single pass *)
    
(** instantiate: exp list -> formula -> formula *)
let instantiate es f = 
  let rec zip_trunc l1 l2 = match l1,l2 with 
      [],_ | _,[]       -> []
    | (h1::t1),(h2::t2) -> (h1,t2)::(t1,t2) in
  let instantiate_exp xs_es = function
      Const(n)   -> Const(n)
    | Var(y)     -> begin try Safelist.assoc y xs_es with Not_found -> Var(y) end
    | Sum(e1,e2) -> Sum(instantiate_exp xs_es e1, instantiate_exp xs_es e2) in            
  let instantiate_aux xs_es = function
      Equal(i,e1,e2) -> Equal(i, instantiate_exp xs_es e1, instantiate_exp xs_es e2)
    | Not(i,f)       -> Not(i, instantiate_aux xs_es f)
    | Or(i,f1,f2)    -> Or(i, instantiate xs_es f1, instantiate xs_es f2)
    | Exists(i,f)    -> Exists(i, instantiate (Safelist.remove_assoc 0 xs_es) f) in
  let xs = IntSet.elements (fvs f) in 
  let xs_es = zip_trunc xs es in
    instantiate_aux xs_es
      
(** mk_fresh_vars: int -> int -> exp list *)
(** creates @n@ fresh variables starting from @d@ *)
let mk_fresh_vars n d = 
  let rec aux acc = function 
      0 -> Safelist.rev acc 
    | n -> aux (Var(n + d)::acc) (n-1) in
    aux i [] n in


(* --------------- constructors for derived forms --------------- *)
(** mkAnd Info.t -> formula -> formula -> formula *)
let mkAnd i f1 f2 = Not(i,Or(i,Not(i,f1),Not(i,f2)))

(** mkImplies Info.t -> formula -> formula -> formula *)
let mkImplies i f1 f2 = Or(i,Not(i,f1),f2)

(** mkEquiv Info.t -> formula -> formula -> formula *)
let mkEquivalent i f1 f2 = And(i,mkImplies(i,f1,f2),mkImplies(i,f2,f1))

(** mkForall: Info.t -> formula -> formula *)
let mkForall i f = Not(i,Exists(i,Not(i,f)))

(** mkLess: Info.t -> exp -> exp -> formula *)  
let mkLess i e1 e2 = Exists(i,Equal(shift_exp 1 e1,Sum(i,shift_exp 1 e2,Sum(i,Var(0),Sum(i,Const(1))))))

(**mkStar: Info.t -> formula -> formula *)
let mkStar i f1 = f1 (* TODO! *)

(** mkSum: Info.t -> formula -> formula -> formula *)
let mkSum i f1 f2 = 
  let fv1 = fv_formula f1 in 
  let fv2 = fv_formula f2 in 
  let c1 = IntSet.cardinal fv1 in
  let c2 = IntSet.cardinal fv2 in 
  let _ = assert(c2 = c2) in 
    
  (* instantiate f1 with 0 to (c1-1) and f1 with c1 to c1+c2-1 *)
  let f1' = instantiate (fresh_vars c1 0) f1 in
  let f2' = instantiate (fresh_vars c2 c1) f2 in 
    
  let rec build_eqs f = function
      -1 -> f
    | i -> 
        let s = Sum(Var(i),(Var(i+c1))) in 
        let e = Equal(i,Var(i+c1+c2),s) in
        let a = mkAnd i f e in
          build_eqs a (n-1)
  in
  let f = build_eqs 
    (* now just close it all up under some exists *)
let rec build_sum n f = 
  if (n = 0) then f
    else Exists(i,f)
    


(* --------------- utility functions --------------- *)
let info_of_exp = function
    Const(i,_) -> i
  | Var(i,_)   -> i
  | Sum(i,_,_) -> i
let info_of_formula = function
    Equal(i,_,_)   -> i
  | Not(i,_)       -> i
  | Or(i,_,_)      -> i
  | Exists(i,_,_)  -> i
 
let format_t t0 = function _ -> Format.printf "UNIMPLEMENTED"
