(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.mli - interface for run-time representations               *)
(*                                                                  *)
(* $Id$ *)
(*                                                                  *)
(********************************************************************)

type t = 
    N of Name.t                 (* names *)
  | L of Lens.t                 (* lenses *)      
  | T of Type.t                 (* types *)
  | V of V.t                    (* views *)
  | F of (t -> t)               (* functions *)

val string_of_t : t -> string
val dummy : Syntax.sort -> t
val memoize : t -> t
