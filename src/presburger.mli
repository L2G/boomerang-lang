
module IntSet : Set.S with type elt = int
module IntMap : Mapplus.SMap with type key_t = int and type key_set = IntSet.t

type exp 
type t

val get_atom : t -> t option

val fvs_t : t -> IntSet.t
val shift_exp : int -> exp -> exp
val shift_t : int -> t -> t
val substitute_exp : (int * exp) list -> exp -> exp
val substitute : (int * exp) list -> t -> t

(* constants *)
val zero : exp
val one : exp
val tru : t
val fls : t

val mkConst: int -> exp
val mkVar : int -> exp
val mkSum : exp -> exp -> exp
val mkNot : t -> t
val mkOr : t list -> t
val mkAnd : t list -> t
val mkExists : t -> t
val wrap : int -> t -> t
val mkEq : exp -> exp -> t
val mkLt : exp -> exp -> t
val mkLe : exp -> exp -> t
val mkGt : exp -> exp -> t
val mkGe : exp -> exp -> t

val add2 : t -> t -> t
val add : t list -> t

val fformat_t : Format.formatter -> t -> unit
val format_t : t -> unit

val satisfiable : t -> bool

module Valuation : sig
  type t
  val length : t -> int
  val create : int -> int -> bool -> t
  val zonk : t -> int -> unit
  val bump : t -> int -> unit
  val format_t : t -> unit
end

val fast_sat : t -> Valuation.t -> bool

val print_stats : unit -> unit
