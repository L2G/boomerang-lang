(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /boomerang/src/blenses.mli                                                  *)
(* Boomerang lens combinators interface                                        *)
(* $Id$ *)
(******************************************************************************)

type key
type skeleton 

val generic_iter :
     'a               (* epsilon *)
  -> ('a -> 'a -> 'a) (* union *)
  -> ('a -> 'a -> 'a) (* concat *)
  -> ('a -> 'a)       (* star *)
  -> int -> int       (* min / max *)
  -> 'a               (* x *)
  -> 'a

module Permutations : sig 
  val valid_permutation : int list -> 'a list -> bool
    (** [valid_permutation sigma ls] is true iff sigma is a valid permutation for ls, 
        i.e. if they are both length k and sigma is a rearrangement of the list 
        [0;1;...;k-1] *)
  val permutations : int -> int list list
    (** [permutations k] returns all valid permutations of lists of length k
        The identity permutation [0;1;...;k-1] will be first in this list. *)
  val invert_permutation : int list -> int list
    (** [invert_permutation i sigma] inverts sigma *)
  val permute_list : int list -> 'a list -> 'a list
    (** [permute_list sigma ls] permutes [ls] according to [sigma]; an error will
        be signalled if it is not the case that [valid_permutation sigma ls] *)
end

module Canonizer : sig
  type t 

  (* meta data *)
  val info : t -> Info.t
  val string : t -> string
  (* types *)
  val uncanonized_type : t -> Brx.t
  val canonized_type : t -> Brx.t
  val cnrel_identity : t -> bool
  (* components *)
  val canonize : t -> (string -> string)
  val choose : t -> (string -> string)

  (* constructors *)
  val copy : Info.t -> Brx.t -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val normalize : Info.t -> Brx.t -> Brx.t -> (string -> string) ->  t
  val sort : Info.t -> Brx.t list -> t
  val columnize : Info.t -> int -> Brx.t -> char -> string -> t
  val iter : Info.t -> t -> int -> int -> t
end

module DLens : sig 
  type t

  val info : t -> Info.t
  val string : t -> string

  val ctype : t -> Brx.t
  val atype : t -> Brx.t
  val crel_identity : t -> bool
  val arel_identity : t -> bool
  val bij : t -> bool
  val xtype : t -> Erx.t option
  val rget : t -> (string -> string)
  val rput : t -> string -> string -> string
  val rcreate : t -> string -> string
  val forgetkey : Info.t -> t -> t
  val probe : Info.t -> string -> t -> t
  val canonizer_of_t : Info.t -> t -> Canonizer.t
  val invert : Info.t -> t -> t
  val copy : Info.t -> Brx.t -> t
  val key : Info.t -> Brx.t -> t
  val const : Info.t -> Brx.t -> string -> string -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val iter : Info.t -> t -> int -> int -> t
  val permute : Info.t -> int list -> t list -> t
  val compose : Info.t -> t -> t -> t
  val default : Info.t -> t -> string -> t
  val dmatch : Info.t -> string -> t -> t
  val smatch : Info.t -> float -> string -> t -> t
  val filter : Info.t -> Brx.t -> Brx.t -> t
  val left_quot : Info.t -> Canonizer.t -> t -> t
  val right_quot : Info.t -> t -> Canonizer.t -> t
  val dup1 : Info.t -> t -> (string -> string) -> Brx.t -> t
  val dup2 : Info.t -> (string -> string) -> Brx.t -> t -> t
end
