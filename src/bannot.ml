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
(* src/bannot.ml                                                              *)
(* Annotations                                                                *)
(* $Id$ *)
(******************************************************************************)

module Weight = struct
  type t = int
  let zero = 0
  let one = 1
  let of_int i = i
  let to_int t = t
  let succ_int t i = i + t
  let weight_int t i = i * t
  let of_string s =
    try int_of_string s
    with Failure _ -> assert false
  let to_string t =
    string_of_int t
  let to_forcestring (b, t) =
    (if b then "!" else "") ^ (to_string t)
  let compare = compare
end
