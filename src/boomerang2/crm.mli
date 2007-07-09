module Make
  (S:Set.S)
  (RS:sig
     type sym
     val char_code_min : sym
     val char_code_max : sym 
     val leq : sym -> sym -> bool
     val l : sym -> sym -> bool
     val compare_sym : sym -> sym -> int
     val pred : sym -> sym
     val succ : sym -> sym
   end): 
sig
  type t 
  val empty : t 
  val single_trans : RS.sym -> S.elt -> t
  val map : (S.t -> S.t) -> t -> t
  val iter : ((RS.sym * RS.sym) -> S.t -> unit ) ->t -> unit
  val is_empty: t -> bool
  val fold : ((RS.sym * RS.sym) -> S.t -> 'a -> 'a) -> t -> 'a -> 'a
  val add : (RS.sym * RS.sym) -> S.t -> t -> t
  val add_elt : RS.sym -> S.t -> t -> t
  val rem : (RS.sym * RS.sym) -> t -> t
  val rem_elt : RS.sym -> t -> t
  val fill_holes : S.t -> t -> t
  val union : t -> t -> t
  val find : (RS.sym * RS.sym) -> t -> S.t
  val safe_find : (RS.sym * RS.sym) -> t -> S.t -> S.t
  val find_elt : RS.sym -> t -> S.t
  val safe_find_elt : RS.sym -> t -> S.t -> S.t
  val isect_range : (RS.sym * RS.sym) -> (RS.sym * RS.sym) -> (RS.sym * RS.sym) option
  val product : (S.t -> S.t -> S.t) -> t -> t -> t
end
