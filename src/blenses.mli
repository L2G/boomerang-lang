(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/blenses.mli                                                   *)
(* Boomerang lens combinators interface                                         *)
(* $Id$ *)
(*******************************************************************************)

type key
type skeleton 

val generic_iter : Info.t 
  -> 'a               (* epsilon *)
  -> ('a -> 'a -> 'a) (* union *)
  -> ('a -> 'a -> 'a) (* concat *)
  -> ('a -> 'a)       (* star *)
  -> 'a               (* x *)
  -> int -> int       (* min / max *)
  -> 'a

val valid_permutation : int list -> 'a list -> bool
(** [valid_permutation sigma ls] is true iff sigma is a valid permutation for ls, 
    i.e. if they are both length k and sigma is a rearrangement of the list 
    [0;1;...;k-1] *)
val permutations : int -> int list list
(** [permutations k] returns all valid permutations of lists of length k 
    The identity permutation [0;1;...;k-1] will be first in this list. *)
val invert_permutation : Info.t -> int list -> int list
(** [invert_permutation i sigma] inverts sigma *)
val permute_list : Info.t -> int list -> 'a list -> 'a list
(** [permute_list i sigma ls] permutes [ls] according to [sigma]; an error will
    be signalled if it is not the case that [valid_permutation sigma ls] *)

module Canonizer : sig
  type t 
  val info : t -> Info.t
  val string : t -> string
  val uncanonized_type : t -> Bregexp.t
  val canonized_type : t -> Bregexp.t
  val cnrel_identity : t -> bool
  val canonize : t -> (string -> string)
  val choose : t -> (string -> string)

  val columnize : Info.t -> int -> Bregexp.t -> char -> string -> t
  val copy : Info.t -> Bregexp.t -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val normalize : Info.t -> (string -> string) -> Bregexp.t -> Bregexp.t -> t
  val sort : Info.t -> Bregexp.t list -> t
  val iter : Info.t -> t -> int -> int -> t
end

module DLens : sig 
  type t
  type dict
  val std_lookup : string -> string -> dict -> ((skeleton * dict) * dict) option
  val sim_lookup : float -> string -> string -> dict -> ((skeleton * dict) * dict) option

  val info : t -> Info.t
  val string : t -> string
  val ctype : t -> Bregexp.t
  val atype : t -> Bregexp.t
  val crel_identity : t -> bool
  val arel_identity : t -> bool
  val bij : t -> bool
  val xtype : t -> Erx.t option
  val rget : t -> (string -> string)
  val rput : t -> string -> string -> string
  val rcreate : t -> string -> string
  val forgetkey : t -> t
  val canonizer_of_t : Info.t -> t -> Canonizer.t
  val invert : Info.t -> t -> t
  val copy : Info.t -> Bregexp.t -> t
  val key : Info.t -> Bregexp.t -> t
  val const : Info.t -> Bregexp.t -> string -> string -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val disjoint_union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val iter : Info.t -> t -> int -> int -> t
  val permute : Info.t -> int list -> t list -> t
  val compose : Info.t -> t -> t -> t
  val default : Info.t -> string -> t -> t
  val dmatch : Info.t -> 
               (string -> string -> dict -> ((skeleton * dict) * dict) option) -> 
               string -> t -> t
  val filter : Info.t -> Bregexp.t -> Bregexp.t -> t
  val left_quot : Info.t -> Canonizer.t -> t -> t
  val right_quot : Info.t -> t -> Canonizer.t -> t
  val dup : Info.t -> bool -> t -> (string -> string) -> Bregexp.t -> t
end
