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
(* /boomerang/src/bcompiler.mli                                                *)
(* Boomerang type checker interface                                            *)
(* $Id$ *)
(*******************************************************************************)

open Bident
open Bsyntax

val no_alias : bool Prefs.t

val ignore_refinements : bool Prefs.t

val may_coerce : sort -> sort -> bool
(** [may_coerce f t] returns true if the cast from f to t will coerce
    values from type to type, and must be run regardless of whether or not
    refinements are checked *)

val trivial_cast : sort -> sort -> bool
(** [trivial_cast f t] returns true if the cast is trivial, i.e.,
    always satisfied *)

val check_module: modl -> modl

