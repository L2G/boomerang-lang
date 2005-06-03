(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* info.mli - interface of error reporting locations                           *)
(*******************************************************************************)
(* $Id$ *)

type pos = int * int
(** [pos] represents a pin-point location in a file **)

type t = pos * pos
(** [t] represents a location that spans a chunk of a file **)

val bogus : t
(** [bogus] is a dummy location **)

val string_of_t : t -> string 
(** [string_of_t] pretty prints a location for easy parsing by [compile-mode] in [emacs] **)
  
val merge_inc : t -> t -> t
(** [merge_inc i1 i2] merges the locations [i1] and [i2] into a new
    location; includes the endpoints **)

val merge_exc : t -> t -> t
(** [merge_exc i1 i2] merges the locations [i1] and [i2] into a new
    location; excludes the endpoints **)
