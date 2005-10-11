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
val equal : action

val conflict_free : action -> bool
val format : action -> unit
val sync : Schema.t -> V.t option -> V.t option -> V.t option -> 
  action * V.t option * V.t option * V.t option
val propagate : V.t option -> V.t option -> V.t option -> action -> 
  V.t option * V.t option * V.t option 
