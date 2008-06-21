(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                  *)
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
(* /boomerang/src/bdiff3.ml                                                    *)
(* Boomerang Diff3 implementation                                              *)
(* $Id$ *)
(*******************************************************************************)

module type Diff3Arg = sig
  type elt
  val eqv : elt -> elt -> bool
  val format : elt -> unit
end

module type Diff3Res = sig
  type elt
  type seq = elt list
  
  type chunk = 
    | Stable of elt * elt * elt
    | AChange of seq * seq * seq
    | BChange of seq * seq * seq
    | Conflict of seq * seq * seq
  val parse : seq -> seq -> seq -> chunk list
end

module Make(A:Diff3Arg) : Diff3Res with type elt = A.elt
