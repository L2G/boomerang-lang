(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* sync.mli - interface for synchronizer                        *)
(****************************************************************)
(* $Id *)

(** The synchronization algorithm *)

type copy_value
type action

val get_action_name : action -> string
val conflict_free : action -> bool
val schema_conflict : action -> bool
val find_conflict : action -> action option
val format_copy : string -> copy_value -> unit
val format : action -> unit
val format_without_equal : action -> unit
val accumulate : (Name.t * V.t) list -> Name.t -> V.t option -> (Name.t * V.t) list
val sync : Schema.t -> V.t option -> V.t option -> V.t option -> 
  action * V.t option * V.t option * V.t option
val propagate : V.t option -> V.t option -> V.t option -> action -> 
  V.t option * V.t option * V.t option 
