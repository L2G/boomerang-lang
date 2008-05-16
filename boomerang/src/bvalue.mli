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

type t = 
    | Unt of Info.t
    | Bol of Info.t * bool
    | Int of Info.t * int
    | Chr of Info.t * Bstring.sym 
    | Str of Info.t * Bstring.t 
    | Rx  of Info.t * Bregexp.t
    | Lns of Info.t * Blenses.DLens.t
    | Can of Info.t * Blenses.Canonizer.t
    | Fun of Info.t * (t -> t)
    | Par of Info.t * t * t
    | Vnt of Info.t * Bsyntax.Qid.t * Bsyntax.Id.t * t option
(** The type of boxed run-time values. *)

val info_of_t : t -> Info.t
(** [info_of_t v] extracts the parsing info associated with (the blame of) [v]. *)

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

val get_ch : t -> Bstring.sym
(** [get_s v] returns a char if [v] is a [Chr], and otherwise raises
    an exception. *)

val get_s : t -> Bstring.t
(** [get_s v] returns a string if [v] is a [Str], and otherwise raises
    an exception. *)

val get_b : t -> bool
(** [get_b v] returns a boolean if [v] is a [Bol], and otherwise raises
    an exception. *)

val get_i : t -> int
(** [get_i v] returns an int if [v] is a [Int], and otherwise raises
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
        
val mk_sfun : Info.t -> (Bstring.t -> t) -> t
(** [mk_sfun i f] lifts a string to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_rfun : Info.t -> (Bregexp.t -> t) -> t
(** [mk_rfun i f] lifts a regexp to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_lfun : Info.t -> (Blenses.DLens.t -> t) -> t
(** [mk_lfun i f] lifts a lens to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_cfun : Info.t -> (Blenses.Canonizer.t -> t) -> t
(** [mk_cfun i f] lifts a canonizer to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_chfun : Info.t -> (Bstring.sym -> t) -> t
(** [mk_chfun i f] lifts a char to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_ufun : Info.t -> (unit -> t) -> t
(** [mk_ufun i f] lifts a unit to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_ifun : Info.t -> (int -> t) -> t
(** [mk_ifun i f] lifts an integer to [t] function to a [t] representing that
    function. [i] is used as the parsing info if the argument has a different
    sort. *)

val mk_poly_fun : Info.t -> (t -> t) -> t
(** [mk_poly_fun i f] constructs a [t] representing the [t -> t]
    function given by [f] with parsing info [i]. *)

(** {3 Parsing helper functions for constructing values.} *)

val parse_uid : string -> Bsyntax.Id.t
(** [parse_uid s] parses an [Id.t] from an uppercase string. *)

val parse_qid : string -> Bsyntax.Qid.t
(** [parse_qid s] parses a [Qid.t] from [s]. *)
