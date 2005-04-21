(* Useful predicates *)

type t = Name.t -> bool

(* Prd.tru is always true *)
let tru = fun k -> true

(* Prd.fal is always false *)
let fal = fun k -> false

(* Prd.s is true if its argument is the given value *)
let s k = fun k' -> k=k'

(* Prd.m is true if its argument is one of the given values *)
let m kl = fun k' -> Safelist.mem k' kl

(* Prd.neg negates a Prd.t *)
let neg p = fun k -> not (p k)
