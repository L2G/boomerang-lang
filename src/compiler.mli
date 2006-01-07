(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* compiler.mli - interface for Focal compiler                      *)
(********************************************************************)
(* $Id$ *)

(** The Focal compiler *)

val init : unit -> unit
(** [init ()] forces this module to be loaded, when used in a dynamically linked library *)
