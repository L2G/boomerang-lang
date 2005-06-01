(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* compiler.mli - interface for Focal compiler                      *)
(*                                                                  *)
(********************************************************************)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)

(** The Focal compiler *)

val compile_file : string -> Syntax.id -> unit
val compile_string : string -> string -> unit
