(** Lenses for relational databases. *)

(** Relational databases are values of type [Db.Relation.t Map.Make(String).t]. *)

(** All relational lenses take the name(s) of the table(s) in the concrete view
    to use, followed by the name of the table in the abstract view to produce,
    as their final arguments. *)

val rename :
  string -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [rename cf af ct at] is a lens that renames the field [cf] to [af] in the
    table [ct] and gives the altered table the name [at] in the abstract
    view. *)

val union :
  (Db.Relation.record -> Rlens.bias) -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [union], but the bias decision is a parameter. *)

val inter :
  (Db.Relation.record -> Rlens.bias) -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [union], but the bias decision is a parameter. *)

val diff :
  (Db.Relation.record -> Rlens.bias) -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [union], but the bias decision is a parameter. *)

(*
val union :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [union ct1 ct2 at] is a lens that makes the union of [ct1] and [ct2].  The
    {e put} operation treats the input table symmetrically. *)

val unionl :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [union], but the {e put} operation is left-biased. *)

val unionr :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [union], but the {e put} operation is right-biased. *)

val inter :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [inter ct1 ct2 at] is a lens that makes the intersection of [ct1] and
    [ct2].  The {e put} operation treats the input table symmetrically. *)

val interl :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [inter], but the {e put} operation is left-biased. *)

val interr :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [inter], but the {e put} operation is right-biased. *)

val diff :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [diff ct1 ct2 at] is a lens that makes the difference of [ct1] and
    [ct2].  The {e put} operation treats the input table symmetrically. *)

val diffl :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [diff], but the {e put} operation is left-biased. *)

val diffr :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [diff], but the {e put} operation is right-biased. *)
*)

(*
val select :
  string -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [select f v ct at] selects the records in [ct] for which the field [f] has
    the value [v]. *)
*)

val select :
  (Db.Relation.record -> bool) -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [select p ct at] selects the records in [ct] that satisfy the predicate
    [p]. *)

val select_eq :
  string -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [select f1 f2 ct at] selects the records in [ct] for which the fields [f1]
    and [f2] have the same value. *)

val project :
  string list -> string list -> Db.Relation.t ->
  string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [project flds keys def ct at] projects [ct] on the fields [flds].  The
    behavior of the {e put} operation will depend upon [keys], which are the
    fields of [ct] that should be considered as forming a key, and [def], which
    is a relation that supplies default values for the non-projected fields in
    case records are added in the abstract view. *)

(*
val join :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [join ct1 ct2 at] performs an inner join on [ct1] and [ct2].  The operation
    in the {e put} direction is symmetric. *)

val joinl :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [join], but the {e put} operation is left-biased. *)

val joinr :
  string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [join], but the {e put} operation is right-biased. *)
*)

val ijoin :
  (Db.Relation.record -> Rlens.bias) -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [join], but the bias decision is a parameter. *)

val ojoin :
  Db.Relation.t -> Db.Relation.t ->
    (Db.Relation.record -> bool) -> (Db.Relation.record -> bool) ->
      (Db.Relation.record -> Rlens.bias) -> string -> string -> string ->
        (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [ojoin], but the bias decision is a parameter. *)

(*
val ojoin :
  Db.Relation.t -> Db.Relation.t -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** [ojoin ct1 ct2 at] performs an inner ojoin on [ct1] and [ct2].  The operation
    in the {e put} direction is symmetric. *)

val ojoinl :
  Db.Relation.t -> Db.Relation.t -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [ojoin], but the {e put} operation is left-biased. *)

val ojoinr :
  Db.Relation.t -> Db.Relation.t -> string -> string -> string ->
    (Db.Relation.t Map.Make(String).t, Db.Relation.t Map.Make(String).t) Lens.t
(** Same as [ojoin], but the {e put} operation is right-biased. *)
*)

