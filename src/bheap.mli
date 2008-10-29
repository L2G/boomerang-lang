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
(* /boomerang/src/bheap.mli                                                   *)
(* Global Boomerang heap                                                      *)
(* $Id$ *)
(******************************************************************************)

open Bsyntax

(* ------ type for heap values ----- *)
type cell = Term of exp | Value of Bvalue.t

(* ------ heap operations ----- *)
val init : unit -> unit
(** [init ()] initializes the heap *)

val alloc : (int * exp) list -> exp -> exp
(** [alloc h ls e] generates a set of new locations fi for ls=[(li,ei)], 
    adds {fi:=ei} to the heap, and substitutes fi for li in ei and e. *)    

val get : Info.t -> int -> cell
(** [get i l] returns the binding for [l] in the heap. *)

val update : int -> Bvalue.t -> unit
(** [update l v] updates the binding for [l] in the heap to [v]. *)
