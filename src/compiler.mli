(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* compiler.mli - interface for Focal compiler                      *)
(********************************************************************)
(* $Id$ *)

(** The Focal compiler *)

val init : unit -> unit
(** [init ()] forces this module to be loaded, when used in a dynamically linked
  * library.  When it runs, it registers functions [compile_file_impl] and
  * [compile_fcl_str_impl] with the [Registry]. *)

(* This is exposed so that it can be set from the Toplevel *)
val test_all : bool Prefs.t   
