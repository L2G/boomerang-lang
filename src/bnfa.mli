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
(* /boomerang/src/erx.mli                                                      *)
(* Extended regexp interface                                                   *)
(* $Id$                                                                        *)
(*******************************************************************************)

val print_stat_trim : unit -> unit

type t

val format: t -> unit

(* constructors *)
val mk_str : string -> t
val mk_cset : bool -> (char * char) list -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_complement : t -> t
val mk_diff : t -> t -> t
val mk_inter : t -> t -> t
val mk_reverse : t -> t
val mk_lowercase : t -> t
val mk_uppercase : t -> t
val expand : t -> char -> string -> t

(* operations *)
val representative : t -> string (* raises Not_found *)
val is_empty : t -> bool
val is_singleton : t -> bool

val trim : t -> t
val determinize : t -> t

type dual_single_split = { dss_example : string;
			   dss_cut1 : int;
			   dss_cut2 : int}

val example_of_dss : dual_single_split -> (string * string) * (string * string)

type not_ambig = NA_true of t | NA_false of dual_single_split

type dual_multi_split = { dms_example : string;
			  dms_cut1 : int list;
			  dms_cut2 : int list}

val example_of_dms : dual_multi_split -> string

type not_star_ambig = NSA_true of t | NSA_empty_word | NSA_false | NSA_false_ce of dual_multi_split


val unambig_seq : t -> t -> not_ambig
val unambig_star : t -> not_star_ambig

val match_str : t -> string -> bool
val match_prefix : t -> string -> Int.Set.t
val find_exit_automaton : t -> string -> (Int.Set.t * bool)
val equiv : t -> t -> bool

val split_positions: t -> t -> string -> Int.Set.t
val unambig_split : t -> t -> string -> (string * string) option
val unambig_star_split : t -> string -> string list
val count_unambig_star_split : t -> string -> int
val print_multi_split : int list -> string -> unit 
val print_split : int -> string -> unit 

val epsilon : t
val empty : t

val easy_seq : t -> t -> bool

val easy_star : t-> bool


