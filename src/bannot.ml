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
  type t = NoKey | Key
  let zero = NoKey
  let one = Key
  let of_int = function
    | 0 -> zero
    | 1 -> one
    | _ -> assert false
  let to_int = function
    | NoKey -> 0
    | Key -> 1
  let succ_int t i = i + (to_int t)
  let weight_int t i = i * (to_int t)
  let of_string s =
    try of_int (int_of_string s)
    with Failure _ -> assert false
  let to_string = function
    | NoKey -> "NoKey"
    | Key -> "Key"
  let to_forcestring (b, t) =
    (if b then "!" else "") ^ (to_string t)
  let equiv a b = a = b
end

module Ss = Set.Make (String)

module Lock = struct
  type lock = string
  type t = Ss.t * Ss.t  (* pre, post *)
  let empty = Ss.empty, Ss.empty
  let pre_lock t = Ss.singleton t, Ss.empty
  let post_lock t = Ss.empty, Ss.singleton t
  let is_empty (pre, post) = Ss.is_empty pre && Ss.is_empty post
  let union (a, b) (c, d) =
    Ss.union a c, Ss.union b d
  let equiv (a, b) (c, d) =
    Ss.equal a c && Ss.equal b d
  let is_valid (pre1, post1) (_, post) =
    let pre = Ss.diff pre1 post1 in
    Ss.fold
      (fun lock lko ->
         match lko with
         | Some lock -> Some lock
         | None ->
             if Ss.mem lock pre
             then Some lock
             else None)
      post
      None
  let max_char = 2
  let find_char c s =
    try min (String.index s c) max_char
    with Not_found -> max_char
  let is_valid_crtdel c (_, post) =
    Ss.fold
      (fun lock lko ->
         match lko with
         | Some lock -> Some lock
         | None ->
             if find_char c lock < max_char
             then None
             else Some lock)
      post
      None
  let is_valid_create = is_valid_crtdel 'C'
  let is_valid_delete = is_valid_crtdel 'D'
  let lock_to_string t = t
  let lock_of_string t = t
  let to_string (pre, post) =
    let show_set separator show set =
      let _, s =
        Ss.fold
          (fun a (b, s) ->
             true,
             s ^ (if b then separator else "") ^ show a)
          set
          (false, "")
      in
      s
    in
    let show s = "{" ^ show_set "," (fun x -> x) s ^ "}" in
    show pre ^ show post
end
