module type DIFF3ARGS = sig
  type elt
  type action
  val format_action: action -> unit
  val has_conflict: action -> bool
  val eqv : elt -> elt -> bool
  val tostring : elt -> string
  val format : elt -> unit
  val sync: Treeschema.t -> (elt option * elt option * elt option)
         -> (action * elt option * elt option * elt option)
end

module type DIFF3RES = sig
  type elt 
  type action
  val format_action: action -> unit
  val has_conflict: action -> bool
  val sync : Treeschema.t
          -> (elt list * elt list * elt list)
          -> (action * elt list * elt list * elt list)
end

module Make(A: DIFF3ARGS) : DIFF3RES with type elt = A.elt
