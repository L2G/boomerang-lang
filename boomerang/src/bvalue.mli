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
(* /boomerang/src/bvalue.mli                                                    *)
(* Boomerang run-time value interface                                           *)
(* $Id$ *)
(*******************************************************************************)

(** {2 Boomerang Run-time Values} *)

type blame = Pos of Info.t | NegPos of Info.t * Info.t
(** The type of blame: either positive or negative / positive. *)

type t = 
    | Str of blame * Bstring.t 
    | Rx  of blame * Bregexp.t
    | Lns of blame * Blenses.DLens.t
    | Can of blame * Blenses.Canonizer.t
    | Fun of blame * (t -> t)
    | Unt of blame
    | Par of blame * t * t
    | Vnt of blame * Bsyntax.Qid.t * Bsyntax.Id.t * t option
(** The type of boxed run-time values. *)

val blame_of_t : t -> blame
(** [blame_of_t v] returns the blame associated to [v]. *)

val info_of_blame : blame -> Info.t
(** [info_of_blame b] returns the parsing info contained in the
    positive blame associated to [v]. *)

val blame_of_info : Info.t -> blame
(** [blame_of_info i] constructs (positive) blame from [i]. *)

val info_of_t : t -> Info.t
(** [info_of_t v] extracts the parsing info associated with (the blame of) [v]. *)

val install_blame : blame -> t -> t
(** [install_blame b v] installs [b] as [v]'s blame. *)

val merge_blame : blame -> t -> t
(** [merge_blame b v] merges [b] with [v]'s blame. *)

val equal : t -> t -> bool
(** [equal v1 v2] returns [true] iff [v1] and [v2] represent the same
    value. As usual, [v1] and [v2] must have equality sorts. *)

val format : t -> unit
(** [format v] pretty prints [v] using [Util.format]. *)

val string_of_t : t -> string
(** [string_of_t v] pretty prints [v] as a string. *)

val sort_string_of_t : t -> string
(** [sort_string_of_t v] renders [v]'s sort as a string. *)

(** {2 Conversions on run-time values} *)

val get_s : t -> Bstring.t
(** [get_s v] returns a string if [v] is a [Str], and otherwise raises
    an exception. *)

val get_r : t -> Bregexp.t
(** [get_r v] returns a regexp if [v] is a [Rx], and otherwise raises an
    exception. *)

val get_l : t -> Blenses.DLens.t
(** [get_l v] returns a lens if [v] is a [Lns], and otherwise raises an
    exception. *)

val get_c : t -> Blenses.Canonizer.t
(** [get_c v] returns a canonizer if [v] is a [Can], and otherwise raises an
    exception. *)

val get_f : t -> (t -> t)
(** [get_f v] returns a function if [v] is a [Fun], and otherwise raises an
    exception. *)

val get_u : t -> unit
(** [get_u v] returns a unit if [v] is a [Unt], and otherwise raises an
    exception. *)

val get_p : t -> (t*t)
(** [get_p v] returns a pair if [v] is a [Par], and otherwise raises an
    exception. *)

val get_v : t -> (Bsyntax.Id.t * t option)
(** [get_v v] returns a representation of a variant--a pair consisting
    of a label and an optional value---if [v] is a [Vnt], and
    otherwise raises an exception. *)

val get_b : t -> bool
(** [get_b v] returns a boolean if [v] is a [Vnt] representing a
    [Prelude.bool], and otherwise raises an exception. *)
        
val mk_sfun : blame -> (Bstring.t -> t) -> t
(** [mk_sfun b f] lifts a string to [t] function to a [t] representing that
    function. [b] is used as the blame if the argument has a different
    sort. *)

val mk_rfun : blame -> (Bregexp.t -> t) -> t
(** [mk_rfun b f] lifts a regexp to [t] function to a [t] representing that
    function. [b] is used as the blame if the argument has a different
    sort. *)

val mk_lfun : blame -> (Blenses.DLens.t -> t) -> t
(** [mk_lfun b f] lifts a lens to [t] function to a [t] representing that
    function. [b] is used as the blame if the argument has a different
    sort. *)

val mk_cfun : blame -> (Blenses.Canonizer.t -> t) -> t
(** [mk_cfun b f] lifts a canonizer to [t] function to a [t] representing that
    function. [b] is used as the blame if the argument has a different
    sort. *)

val mk_ufun : blame -> (unit -> t) -> t
(** [mk_ufun b f] lifts a unit to [t] function to a [t] representing that
    function. [b] is used as the blame if the argument has a different
    sort. *)

val mk_poly_fun : blame -> (t -> t) -> t
(** [mk_poly_fun b f] constructs a [t] representing the [t -> t]
    function given by [f] with blame [b]. *)

(** {3 Parsing helper functions for constructing values.} *)

val parse_uid : string -> Bsyntax.Id.t
(** [parse_uid s] parses an [Id.t] from an uppercase string. *)

val parse_qid : string -> Bsyntax.Qid.t
(** [parse_qid s] parses a [Qid.t] from [s]. *)
