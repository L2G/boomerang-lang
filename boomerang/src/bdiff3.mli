module type DIFF3ARGS = sig
  type elt
  val eqv : elt -> elt -> bool
  val format : elt -> unit
end

module type DIFF3RES = sig
  type elt
  type seq = elt list
  
  type chunk = 
    | Stable of elt * elt * elt
    | AChange of seq * seq * seq
    | BChange of seq * seq * seq
    | Conflict of seq * seq * seq
  val parse : seq -> seq -> seq -> chunk list
end

module Make(A: DIFF3ARGS) : DIFF3RES with type elt = A.elt
