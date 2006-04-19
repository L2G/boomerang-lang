module type DIFF3ARGS = sig
  type elt
  val eqv : elt -> elt -> bool
  val tostring : elt -> string
  val format : elt -> unit
end

module Make(A: DIFF3ARGS) : 
sig
      
  val sync : ((A.elt option * A.elt option * A.elt option)
              -> ([`Conflict | `NoConflict] * A.elt option * A.elt option * A.elt option))  
                                                                (* elt sync *)
          -> bool                                               (* log? *)
          -> (A.elt list * A.elt list * A.elt list)             (* inputs *)
          -> ([`Conflict | `NoConflict] * A.elt list * A.elt list * A.elt list)
                                                                (* outputs *)

end
