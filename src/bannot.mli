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
(* src/bannot.mli                                                             *)
(* Annotations                                                                *)
(* $Id$ *)
(******************************************************************************)

module Weight : 
sig
  type t
  val one : t
  val zero : t
  val of_int : int -> t
  val to_int : t -> int
  val succ_int : t -> int -> int
  val weight_int : t -> int -> int
  val to_string : t -> string
  val to_forcestring : bool * t -> string
  val equiv : t -> t -> bool
end

module Lock :
sig
  type t
  type lock
  val empty : t
  val pre_lock : lock -> t
  val post_lock : lock -> t
  val is_empty : t -> bool
  val union : t -> t -> t
  val equiv : t -> t -> bool
  val is_valid : t -> t -> lock option
  val is_valid_create : t -> lock option
  val is_valid_delete : t -> lock option
  val to_string : t -> string
  val lock_to_string : lock -> string
  val lock_of_string : string -> lock
end
