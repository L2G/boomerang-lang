(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* sync.mli - interface for synchronizer                        *)
(****************************************************************)
(* $Id *)

(** The synchronization algorithm *)

type action
val equal: action
val format_action: action -> unit
val has_conflict: action -> bool
val sync: Treeschema.t
       -> (Tree.t option * Tree.t option * Tree.t option)
       -> action * Tree.t option * Tree.t option * Tree.t option

