module Make(S: 
  sig 
    include Set.S 
    val format_elt : elt -> unit 
  end) : sig
  type t 
  val empty : t 
  val single_trans : Rstring.sym -> S.elt -> t
  val map : (S.t -> S.t) -> t -> t
  val iter : ((Rstring.sym * Rstring.sym) -> S.t -> unit ) ->t -> unit
  val is_empty: t -> bool
  val fold : ((Rstring.sym * Rstring.sym) -> S.t -> 'a -> 'a) -> t -> 'a -> 'a
  val add : (Rstring.sym * Rstring.sym) -> S.t -> t -> t
  val add_elt : Rstring.sym -> S.t -> t -> t
  val rem : (Rstring.sym * Rstring.sym) -> t -> t
  val rem_elt : Rstring.sym -> t -> t
  val fill_holes : S.t -> t -> t
  val union : t -> t -> t
  val find : (Rstring.sym * Rstring.sym) -> t -> S.t
  val safe_find : (Rstring.sym * Rstring.sym) -> t -> S.t -> S.t
  val find_elt : Rstring.sym -> t -> S.t
  val safe_find_elt : Rstring.sym -> t -> S.t -> S.t
  val isect_range : (Rstring.sym * Rstring.sym) -> (Rstring.sym * Rstring.sym) -> (Rstring.sym * Rstring.sym) option
  val product : (S.t -> S.t -> S.t) -> t -> t -> t
end
