(** Lenses for relations. *)

type bias = Left | Right | Both
val rename :
  string -> string -> (Db.Relation.t, Db.Relation.t) Lens.t
val union :
  (Db.Relation.record -> bias) -> (Db.Relation.t * Db.Relation.t, Db.Relation.t) Lens.t
val inter :
  (Db.Relation.record -> bias) -> (Db.Relation.t * Db.Relation.t, Db.Relation.t) Lens.t
val diff :
  (Db.Relation.record -> bias) -> (Db.Relation.t * Db.Relation.t, Db.Relation.t) Lens.t
val select :
  (Db.Relation.record -> bool) -> (Db.Relation.t, Db.Relation.t) Lens.t
val project :
  string list -> string list -> Db.Relation.t -> (Db.Relation.t, Db.Relation.t) Lens.t
val ijoin :
  (Db.Relation.record -> bias) -> (Db.Relation.t * Db.Relation.t, Db.Relation.t) Lens.t
val ojoin :
  Db.Relation.t -> Db.Relation.t ->
    (Db.Relation.record -> bool) -> (Db.Relation.record -> bool) ->
      (Db.Relation.record -> bias) -> (Db.Relation.t * Db.Relation.t, Db.Relation.t) Lens.t
