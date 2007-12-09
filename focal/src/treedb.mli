(** A module for converting trees to and from relational databases. *)

val rcd_to_tree : Db.Relation.record -> Tree.t
val tree_to_rcd : Tree.t -> Db.Relation.record
val rel_to_tree : Db.Relation.t -> Tree.t
val tree_to_rel : Tree.t -> Db.Relation.t
val db_to_tree : Db.t -> Tree.t
val tree_to_db : Tree.t -> Db.t
