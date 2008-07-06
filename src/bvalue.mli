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
    | Chr of Info.t * char
    | Str of Info.t * string
    | Rx  of Info.t * Bregexp.t
    | Lns of Info.t * Blenses.DLens.t
    | Can of Info.t * Blenses.Canonizer.t
    | Fun of Info.t * (t -> t)
    | Par of Info.t * t * t
    | Vnt of Info.t * Bident.Qid.t * Bident.Id.t * t option
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

val get_u : t -> unit
val get_b : t -> bool
val get_i : t -> int
val get_c : t -> char
val get_s : t -> string
val get_r : t -> Bregexp.t
val get_l : t -> Blenses.DLens.t
val get_q : t -> Blenses.Canonizer.t
val get_p : t -> (t*t)
val get_v : t -> (Bident.Id.t * t option)
val get_f : t -> (t -> t)

val mk_u : Info.t -> unit -> t
val mk_b : Info.t -> bool -> t
val mk_i : Info.t -> int -> t
val mk_c : Info.t -> char -> t
val mk_l : Info.t -> Blenses.DLens.t -> t
val mk_r : Info.t -> Bregexp.t -> t
val mk_s : Info.t -> string -> t
val mk_q : Info.t -> Blenses.Canonizer.t -> t
val mk_p : Info.t -> t * t -> t
val mk_f : Info.t -> (t -> t) -> t

val mk_ufun : Info.t -> (unit -> t) -> t
val mk_bfun : Info.t -> (bool -> t) -> t
val mk_ifun : Info.t -> (int -> t) -> t
val mk_cfun : Info.t -> (char -> t) -> t
val mk_lfun : Info.t -> (Blenses.DLens.t -> t) -> t
val mk_rfun : Info.t -> (Bregexp.t -> t) -> t
val mk_sfun : Info.t -> (string -> t) -> t
val mk_qfun : Info.t -> (Blenses.Canonizer.t -> t) -> t
val mk_pfun : Info.t -> (t * t -> t) -> t
val mk_vfun : Info.t -> (Bident.Id.t * t option -> t) -> t
val mk_ffun : Info.t -> ((t -> t) -> t) -> t

val string_of_t : t -> string

val list_qid : Bident.Qid.t
val get_list : t -> t list
val mk_list : Info.t -> t list -> t
val mk_listfun : Info.t -> (t list -> t) -> t
