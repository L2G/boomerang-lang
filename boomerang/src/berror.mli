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
(* /boomerang/src/error.mli                                                    *)
(* Boomerang errors interface                                                  *)
(* $Id$                                                                        *)
(*******************************************************************************)

val nlify : string -> unit

val static_error : Info.t -> string -> ?suppl:(unit->unit) -> string -> 'a

val type_error_string : string * string -> string

val type_error : Info.t -> string * string -> 'a

val sort_error : Info.t -> (unit -> unit) -> 'a

val run_error : Info.t -> (unit -> unit) -> 'a

val blame_error : Info.t -> (unit -> unit) -> 'a
