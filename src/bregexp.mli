(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007-2008                                                     *)
(* J. Nathan Foster and Benjamin C. Pierce                                     *)
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
(* /boomerang/src/bregexp.mli                                                  *)
(* Old Boomerang RegExp engine                                                 *)
(* $Id$ *)
(*******************************************************************************)

type t

(* constants *)
val epsilon : t 
val anything : t
val empty : t

(* constructors *)
val mk_cset : (char * char) list -> t
val mk_neg_cset : (char * char) list -> t
val mk_string : string -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_iter : t -> int -> int -> t
val mk_diff : t -> t -> t
val mk_complement: t -> t
val mk_inter : t -> t -> t

(* pretty printing *)
(* ranks *)
type r = 
  | Urnk (* union *)
  | Drnk (* diff *)
  | Irnk (* inter *)
  | Crnk (* concat *)
  | Srnk (* star *)
  | Arnk (* atomic *)
val rank : t -> r
val lpar : r -> r -> bool
val rpar : r -> r -> bool

val format_t : t -> unit
val string_of_t : t -> string

(* operations *)
val is_empty : t -> bool
val is_singleton : t -> bool
val disjoint_cex : t -> t -> string option
val disjoint : t -> t -> bool
val equiv : t -> t -> bool
val representative : t -> string option

(* string matching *)
val match_string : t -> string -> bool
val match_string_positions : t -> string -> Int.Set.t

(* ambiguity *)
val splittable_cex : t -> t -> ((string * string * string),t) Misc.alternative
val splittable : t -> t -> bool
val iterable_cex : t -> ((string * string * string),t) Misc.alternative
val iterable : t -> bool

(* splitting *)
val split_positions : t -> t -> string -> Int.Set.t
val split_bad_prefix : t -> string -> string * string
val seq_split : t -> t -> string -> (string * string) option
val star_split : t -> string -> string list
