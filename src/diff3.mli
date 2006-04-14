module type DIFF3ARGS = sig
  type elt
  val eqv : elt -> elt -> bool
  val tostring : elt -> string
  val format : elt -> unit
  (* Later: val sync_elts : (elt * elt * elt) -> (elt * elt * elt) *)
end

module Make(A: DIFF3ARGS) : 
sig
      
  val sync : (A.elt list * A.elt list * A.elt list)  
    -> (A.elt list * A.elt list * A.elt list)  

end
