(** A module for embedding relational databases into trees. *)

(** Relational databases are values of type [Relation.t Map.Make(String).t]. *)

val rcd_to_view : (string * string) list -> V.t
val view_to_rcd : V.t -> (string * string) list
val rel_to_view : Relation.t -> V.t
val view_to_rel : V.t -> Relation.t
val db_to_view : Relation.t Map.Make(String).t -> V.t
val view_to_db : V.t -> Relation.t Map.Make(String).t
