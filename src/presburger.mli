(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(*********************************************************)
(* $Id: treeschema.mli 2000 2006-07-15 19:04:58Z jnfoster $ *)

(* Expressions *)
type exp 
val zero : exp
val one : exp
val mkConst: int -> exp
val mkVar : int -> exp    (* debruijn representation *)
val mkSum : exp list -> exp

(* Presburger formulas -- quantified boolean combinations
   of (in)equalities between expressions.  A formula
   denotes a set of integer vectors giving valuations
   for its variables that satisfy the constraints it
   expresses. *)
type t
val compare_t : t -> t -> int

val tru : t
val fls : t
val mkNot : t -> t
val mkOr : t list -> t
val mkAnd : t list -> t
val mkEx : t -> t
val wrap : int -> t -> t 
val mkEq : exp -> exp -> t
val mkLt : exp -> exp -> t
val mkLe : exp -> exp -> t
val mkGt : exp -> exp -> t
val mkGe : exp -> exp -> t

val format_t : t -> unit

val satisfiable : t -> bool

(* Free variables *)
val fvs_t : t -> Int.Set.t

(* Shift debruijn indices in expressions and formulas *)
val shift_exp : int -> exp -> exp
val shift_t : int -> t -> t

(* Substitutions are represented as alists between
   variables (indices) and expressions *)
val substitute_exp : (int * exp) list -> exp -> exp
val substitute : (int * exp) list -> t -> t

(* If t1..tn are formulas over the same set of variables,
   the formula [add [t1..tn]] is satisfied by integer vectors
   that can be expressed as the pointwise sum of a vector
   satisfying t1 plus a vector satisfying t2 plys ...
   plus a vector satisfying tn. *)
val add : t list -> t

(* Find a set of variables that are "obviously zero" in
   any satisfying valuation.  (For example, after a basis
   has been refined, there will typically be many variables
   that are obviously zero-valued.) *)
val easy_zeros : t -> Int.Set.t

val finish : unit -> unit
