(** Lenses for relations. *)

type bias = Left | Right | Both
val rename :
  string -> string -> (Relation.t, Relation.t) Lens.t
val union :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val inter :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val diff :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val select :
  (Relation.record -> bool) -> (Relation.t, Relation.t) Lens.t
val project :
  string list -> string list -> Relation.t -> (Relation.t, Relation.t) Lens.t
val ijoin :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val ojoin :
  Relation.t -> Relation.t ->
    (Relation.record -> bool) -> (Relation.record -> bool) ->
      (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
