(** Extended maps
 
  An extended version of OCaml's standard Map module, providing several
  additional operations
*)

module type S = 
  sig
    type key
    (** The type of map keys. *)

    module KeySet: Set.S
    (** Sets of keys. *)

    module Map : sig
      type ('a) t
      (** The type of maps from type [key] to type ['a]. *)
	    
      val empty: 'a t
      (** The empty map. *)

      val is_empty: 'a t -> bool
      (** Tells whether the map is empty **)

      val size: 'a t -> int
      (** the size of the map as an int *)

      val domain: 'a t -> KeySet.t
      (** The domain of the maps as a set of keys *)

      val add: key -> 'a -> 'a t -> 'a t
      (** [add x y m] returns a map containing the same bindings as
          [m], plus a binding of [x] to [y]. If [x] was already bound
          in [m], its previous binding disappears. *)

      val find: key -> 'a t -> 'a
      (** [find x m] returns the current binding of [x] in [m],
          or raises [Not_found] if no such binding exists. *)

      val safe_find: key -> 'a t -> 'a -> 'a
      (** [find x m d] returns the current binding of [x] in [m],
          or a default value [d] if no such binding exists. *)

      val from_list: (key * 'a) list -> 'a t
      (** [from_list [k1, v1; k2, v2; ...; kn, vn]] returns the map that maps
          [ki] to [vi].  Assume that there's no repetition in the keys *)

      val from_function: KeySet.t -> (key -> 'a) -> 'a t
      (** [from_function d f] returns the map with domain d, whose value
          at each x in d is given by [f x]. *)

      val remove: key -> 'a t -> 'a t
      (** [remove x m] returns a map containing the same bindings as
          [m], except for [x] which is unbound in the returned map. *)

      val mem: key -> 'a t -> bool
      (** [mem x m] returns [true] if [m] contains a binding for [x],
          and [false] otherwise. *)

      val iter: (key -> 'a -> unit) -> 'a t -> unit
      (** [iter f m] applies [f] to all bindings in map [m].
          [f] receives the key as first argument, and the associated value
          as second argument. The order in which the bindings are passed to
          [f] is unspecified. Only current bindings are presented to [f]:
          bindings hidden by more recent bindings are not passed to [f]. *)

      val iter_with_sep: (key -> 'a -> unit) -> (unit -> unit) -> 'a t -> unit
      (** [iter_with_sep f sep m] applies [f] to all bindings in map [m],
          just like [iter f m], and furthermore invokes [sep()] after each
          binding except the last. *) 

      val filter: (key -> 'a -> bool) -> 'a t -> 'a t

      val map: ('a -> 'b) -> 'a t -> 'b t
      (** [map f m] returns a map with same domain as [m], where the
          associated value [a] of all bindings of [m] has been
          replaced by the result of the application of [f] to [a].
          The order in which the associated values are passed to [f]
          is unspecified. *)

      val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
      (** Same as {!Map.S.map}, but the function receives as arguments both the
         key and the associated value for each binding of the map. *)
  
      val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
          where [k1 ... kN] are the keys of all bindings in [m],
          and [d1 ... dN] are the associated data.
          The order in which the bindings are presented to [f] is
          unspecified. *)

      val for_all: ('a -> bool) -> 'a t -> bool
      val for_alli: (key -> 'a -> bool) -> 'a t -> bool

      val dump: (key list -> key list)
             -> (key -> string)
             -> ('a -> unit)
             -> ('a -> bool)
             -> 'a t
             -> unit
    end
  end

(* ----- *)

module type OrderedType = sig
  type t 
  val compare : t -> t -> int
  val to_string : t -> string
end

(* ----- *)

module Make (Ord : OrderedType) : S 
  with type key = Ord.t and type KeySet.elt = Ord.t

	    
	    
