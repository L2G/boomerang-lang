(** A module for converting directories of CSV files to and from relational
    databases. *)

val load_tbl : string (*filename*) -> Relation.t
val save_tbl : string (*filename*) -> Relation.t -> unit

val load_db : string (*filename*) -> Relation.t Map.Make(String).t
val save_db : string (*filename*) -> Relation.t Map.Make(String).t -> unit
