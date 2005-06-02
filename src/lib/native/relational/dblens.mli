
type ('c, 'a) lens = {
  get : 'c -> 'a;
  put : 'a -> 'c -> 'c;
}

val rename :
  string -> string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val union :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val unionl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val unionr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val uniongen :
  (Relation.record -> Rlens.bias) ->
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val inter :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val interl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val interr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val intergen :
  (Relation.record -> Rlens.bias) ->
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val diff :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val diffl :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val diffr :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val diffgen :
  (Relation.record -> Rlens.bias) ->
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val select :
  string -> string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val selectgen :
  (Relation.record -> bool) ->
  string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val project :
  string list -> string list -> Relation.t ->
  string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
val join :
  string -> string -> string ->
    (Relation.t Map.Make(String).t, Relation.t Map.Make(String).t) lens
