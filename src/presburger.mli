
module IntSet : Set.S with type elt = int

type exp 
type formula

val info_of_exp : exp -> Info.t
val info_of_formula : formula -> Info.t

val fvs_exp : exp -> IntSet.t
val fvs_formula : formula -> IntSet.t
val shift_exp : int -> exp -> exp
val shift_formula : int -> formula -> formula
val substitute : int list -> exp list -> formula -> formula
val instantiate : exp list -> formula -> formula

val mkConst: Info.t -> int -> exp
val mkVar : Info.t -> int -> exp
val mkSum : exp -> exp -> exp
val mkEqual : exp -> exp -> formula
val mkNot : formula -> formula
val mkOr : formula -> formula -> formula
val mkExists : formula -> formula
val mkAnd : formula -> formula -> formula
val mkLt : exp -> exp -> formula
val mkLe : exp -> exp -> formula
val mkGt : exp -> exp -> formula
val mkGe : exp -> exp -> formula

val mkSum_formula : formula -> formula -> formula

val format_exp : exp -> unit
val format_formula : exp -> unit

val satisfiable : formula -> bool 
val init : unit -> unit
