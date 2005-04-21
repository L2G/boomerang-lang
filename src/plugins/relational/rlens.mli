(* Pure lens type *)
type ('c, 'a) lens = {
  get : 'c -> 'a;
  put : 'a -> 'c -> 'c;
}
type bias = Left | Right | Both
val rename :
  string -> string -> (Relation.t, Relation.t) lens
val uniongen :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) lens
val intergen :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) lens
val diffgen :
  (Relation.record -> bias) -> (Relation.t * Relation.t, Relation.t) lens
val select :
  string -> string -> (Relation.t, Relation.t) lens
val selectgen :
  (Relation.record -> bool) -> (Relation.t, Relation.t) lens
val project :
  string list -> string list -> Relation.t -> (Relation.t, Relation.t) lens
val join :
  (Relation.t * Relation.t, Relation.t) lens
