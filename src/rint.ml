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
(* /boomerang/src/rint.ml                                                      *)
(* Boomerang ordered int                                                       *)
(* $Id$                                                                        *)
(*******************************************************************************)

module OrderedInt =
struct 
  type t = int
  let compare (x:int) (y:int) = 
    if x = y then 0
    else if x < y then -1 
    else  1
end

module Map = Map.Make(OrderedInt)
module Set = Set.Make(OrderedInt)
