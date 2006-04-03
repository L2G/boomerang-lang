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
    Const of Info.t * int
  | Var of Info.t * int 
  | Sum of Info.t * exp * exp
(* formulas are represented in de Bruijn notation *)
and formula = 
    Equal of Info.t * exp * exp 
  | Not of Info.t * formula
  | Or of Info.t * formula * formula
  | Exists of Info.t * formula

(* --------------- utility functions --------------- *)
let info_of_exp = function
    Const(i,_) -> i
  | Var(i,_)   -> i
  | Sum(i,_,_) -> i
let info_of_formula = function
    Equal(i,_,_)   -> i
  | Not(i,_)       -> i
  | Or(i,_,_)      -> i
  | Exists(i,_)  -> i

(* calculate the free variables of an exp or formula *)
let rec fvs =
  let rec fvs_exp = function
      Const(_,_)   -> IntSet.empty
    | Var(_,x)     -> IntSet.singleton x
    | Sum(_,e1,e2) -> IntSet.union (fvs_exp e1) (fvs_exp e2) 
  in function
      Equal(_,e1,e2) -> IntSet.union (fvs_exp e1) (fvs_exp e2)
    | Not(_,f)       -> fvs f
    | Or(_,f1,f2)    -> IntSet.union (fvs f1) (fvs f2)
    | Exists(_,f)    -> 
        IntSet.fold 
          (fun x s -> if (x <> 0) then (IntSet.add (x-1) s) else s) 
          (fvs f)
          IntSet.empty 

(* --------------- de Bruijn shifting -------------- *)
(* cutoff *)
let rec shift_exp_aux n c = function
    Const(i,n)    -> Const(i,n) 
  | Var(i,x)      -> Var(i, if (x <= c) then x else x + n)
  | Sum(i,e1,e2)  -> Sum(i,shift_exp_aux n c e1, shift_exp_aux n c e2)
and shift_aux n c = function 
    Equal(i,e1,e2) -> Equal(i, shift_exp_aux n c e1, shift_exp_aux n c e2)
  | Not(i,f)       -> Not(i,shift_aux n c f)
  | Or(i,f1,f2)    -> Or(i, shift_aux n c f1, shift_aux n c f2)
  | Exists(i,f)    -> Exists(i, shift_aux n (c+1) f)

(* plain shifting *)
let shift_exp n = shift_exp_aux n 0
let shift n = shift_aux n 0

(* simulatneous substitution *)
(** substitute: -> int list -> exp list -> formula -> formula *)
let rec substitute xs es = 
  let rec do_subst xs es i y = 
    match xs,es with
        [],_ | _,[] -> Var(i,y)
      | (x1::xt),(e1::et) -> 
          if (y = x1) then e1 else do_subst xt et i y in
  let rec substitute_exp xs es = function
      Const(i,n)   -> Const(i,n)
    | Var(i,y)     -> do_subst xs es i y
    | Sum(i,e1,e2) -> 
        Sum(i,substitute_exp xs es e1, substitute_exp xs es e2) in 
    function        
      Equal(i,e1,e2) -> Equal(i, substitute_exp xs es e1, substitute_exp xs es e2)
    | Not(i,f)       -> Not(i, substitute xs es f)
    | Or(i,f1,f2)    -> Or(i, substitute xs es f1, substitute xs es f2)
    | Exists(i,f)    -> Exists(i, substitute (List.map succ xs) (List.map (shift_exp 1) es) f)
    
(** instantiate: exp list -> formula -> formula *)
let instantiate es f = 
  let rec mk_xs i = function [] -> [] | _::t -> i::(mk_xs (i+1) t) in
    substitute (mk_xs 0 es) es f

(* --------------- constructors --------------- *)

(** mkConst: Info.t -> int -> exp *)
let mkConst i n = Const(i,n)

(** mkVar: Info.t -> int -> exp *)
let mkVar i x = Var(i,x)

(** mkSum: exp -> exp -> exp *)
let mkSum e1 e2 = Sum(Info.merge_inc (info_of_exp e1) (info_of_exp e2),e1 ,e2)

(** mkEqual: exp -> exp -> formula *)
let mkEqual e1 e2 = Equal(Info.merge_inc (info_of_exp e1) (info_of_exp e2),e1,e2)

(** mkNot: formula -> formula *)
let mkNot f = Not(info_of_formula f, f)
 
(** mkOr: formula -> formula -> formula *)
let mkOr f1 f2 = 
  Or(Info.merge_inc (info_of_formula f1) (info_of_formula f2),f1,f2)

(** mkExists: formula -> formula *)
let mkExists f = Exists(info_of_formula f,f)

(** mkAnd  formula -> formula -> formula *)
let mkAnd f1 f2 =  mkNot (mkOr (mkNot f1) (mkNot f2))

(** mkLt: exp -> exp -> formula *)
let mkLt e1 e2 = 
  mkExists
    (mkEqual 
       (shift_exp 1 e1)
       (mkSum (shift_exp 1 e2) (mkVar (info_of_exp e2) 0)))

(** mkLe: exp -> exp -> formula *)
let mkLe e1 e2 = mkOr (mkEqual e1 e2) (mkLt e1 e2)

(** mkGt: exp -> exp -> formula *)
let mkGt e1 e2 = mkLt e2 e1

(** mkGe: exp -> exp -> formula *)
let mkGe e1 e2 = mkLe e2 e1

(** mkSum: -> formula -> formula -> formula *)
let mkSum_formula f1 f2 = 
  let i = Info.merge_inc (info_of_formula f1) (info_of_formula f2) in
  let rec fresh start num acc =     
    if num = 0 then acc
    else fresh (start+1) (num-1) (Var(i,start)::acc) in
  let c1 = IntSet.cardinal (fvs f1) in
  let c2 = IntSet.cardinal (fvs f2) in 
  let _ = assert(c1 = c2) in 
    
  (* instantiate f1 with 0 to (c1-1) and f1 with c1 to c1+c2-1 *)
  let f1' = instantiate (List.rev (fresh 0 c1 [])) f1 in
  let f2' = instantiate (List.rev (fresh c1 c2 [])) f2 in 
  let rec mk_sums n acc = 
    if n = 0 then acc 
    else 
      mk_sums (n-1)
        ((mkEqual 
            (mkVar i (n+c1+c2))
            (mkSum (mkVar i n) (mkVar i (n+c1))))::acc) in
  let rec f' = List.fold_left (fun c cs -> mkAnd cs c) f1' (f2'::mk_sums c1 []) in
  let rec mk_exists n acc = if n = 0 then acc else mk_exists (n-1) (mkExists acc) in             
    mk_exists c1 f'
      
let rec format_exp = function _ -> Format.printf "UNFINISHED"
let format = function _ -> Format.printf "UNFINISHED"
