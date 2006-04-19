(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* sync.mli - interface for synchronizer                        *)
(****************************************************************)
(* $Id *)

(** The synchronization algorithm *)

val sync : Schema.t -> V.t option -> V.t option -> V.t option -> bool ->
           [`Conflict | `NoConflict] * V.t option * V.t option * V.t option

