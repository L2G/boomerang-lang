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
(* /boomerang/src/blenses.mli                                                   *)
(* Boomerang lens combinators interface                                         *)
(* $Id$ *)
(*******************************************************************************)

type key
type skeleton 

module Canonizer : sig
  type t 
  val info : t -> Info.t
  val string : t -> string
  val rtype : t -> Bregexp.t
  val ctype : t -> Bregexp.t
  val canonize : t -> (Bstring.t -> Bstring.t)
  val choose : t -> (Bstring.t -> Bstring.t)

  val copy : Info.t -> Bregexp.t -> t
  val columnize : Info.t -> Bstring.t -> Bregexp.t -> Bstring.t -> Bstring.t -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val iter : Info.t -> t -> int -> int -> t
end

module DLens : sig 
  type t
  type dict
  val std_lookup : Bstring.t -> Bstring.t -> dict -> ((skeleton * dict) * dict) option
  val sim_lookup : float -> Bstring.t -> Bstring.t -> dict -> ((skeleton * dict) * dict) option

  val info : t -> Info.t
  val string : t -> string
  val bij : t -> bool
  val ctype : t -> Bregexp.t
  val atype : t -> Bregexp.t
  val rget : t -> (Bstring.t -> Bstring.t)
  val rput : t -> Bstring.t -> Bstring.t -> Bstring.t
  val rcreate : t -> Bstring.t -> Bstring.t
  val unsafe_rget : t -> (Bstring.t -> Bstring.t)
  val unsafe_rput : t -> Bstring.t -> Bstring.t -> Bstring.t
  val unsafe_rcreate : t -> Bstring.t -> Bstring.t
  val forgetkey : t -> t
  val canonizer_of_t : Info.t -> t -> Canonizer.t
  val invert : Info.t -> t -> t
  val copy : Info.t -> Bregexp.t -> t
  val key : Info.t -> Bregexp.t -> t
  val unsafe_const : Info.t -> Bregexp.t -> Bstring.t -> Bstring.t -> t
  val unsafe_concat : Info.t -> t -> t -> t
  val unsafe_union : Info.t -> t -> t -> t
  val unsafe_disjoint_union : Info.t -> t -> t -> t
  val unsafe_star : Info.t -> t -> t
  val unsafe_iter : Info.t -> t -> int -> int -> t
  val unsafe_swap : Info.t -> t -> t -> t
  val compose : Info.t -> t -> t -> t
  val default : Info.t -> Bstring.t -> t -> t
  val dmatch : Info.t -> 
               (Bstring.t -> Bstring.t -> dict -> ((skeleton * dict) * dict) option) -> 
               Bstring.t -> t -> t
  val unsafe_filter : Info.t -> Bregexp.t -> Bregexp.t -> t
  val left_quot : Info.t -> Canonizer.t -> t -> t
  val right_quot : Info.t -> t -> Canonizer.t -> t
end
