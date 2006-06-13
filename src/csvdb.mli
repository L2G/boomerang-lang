(** A module for converting directories of CSV files to and from relational
    databases. *)

val load_tbl : string (*filename*) -> Db.Relation.t
val save_tbl : string (*filename*) -> Db.Relation.t -> unit

val load_db : string (*filename*) -> Db.t
val save_db : string (*filename*) -> Db.t -> unit
