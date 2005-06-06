type bias = Left | Right | Both
val rename :
  string -> string -> (Relation.t, Relation.t) Lens.t
val generic_union :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val generic_inter :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val generic_diff :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
val generic_select :
  (Relation.record -> bool) -> (Relation.t, Relation.t) Lens.t
val project :
  string list -> string list -> Relation.t -> (Relation.t, Relation.t) Lens.t
val generic_join :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) Lens.t
