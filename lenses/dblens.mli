
val rename :
  string -> string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val union :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val unionl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val unionr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val inter :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val interl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val interr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val diff :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val diffl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val diffr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val select :
  string -> string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val select_eq :
  string -> string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val project :
  string list -> string list -> Relation.t ->
  string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val join :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val joinl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
val joinr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) Lens.t
