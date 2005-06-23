(** A module for converting trees to and from relational databases. *)

(** Relational databases are values of type [Relation.t Map.Make(String).t]. *)

val rcd_to_tree : (string * string) list -> V.t
val tree_to_rcd : V.t -> (string * string) list
val rel_to_tree : Relation.t -> V.t
val tree_to_rel : V.t -> Relation.t
val db_to_tree : Relation.t Map.Make(String).t -> V.t
val tree_to_db : V.t -> Relation.t Map.Make(String).t
