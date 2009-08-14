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
(* src/bstring.mli                                                            *)
(* Annotated strings                                                          *)
(* $Id$ *)
(******************************************************************************)

type t
type a
type at = a * t
type attmp
type cat

val empty : t
val of_string : string -> t
val to_attmp : t -> attmp
val to_string : t -> string
val of_at : at -> t
val of_attmp : attmp -> t
val of_cat : cat -> t
val length : t -> int
val dist : int option -> string -> string -> int option
val at_print_flat : at -> string
val cat_print_flat : cat -> string
val at_print_all : at -> string
val toplevel_chunks : cat -> int Btag.MapAList.t
val at_to_chunktree : bool -> at -> cat * ((int * int) * cat) Btag.MapIntMapA.t
val cat_to_key : cat -> string
val cat_fold_on_locs : (Btag.t -> int -> 'a -> 'a) -> cat -> 'a -> 'a
val at_to_locs : at -> Btag.MapInt.t
val match_rx : Brx.t -> t -> bool
val at_to_weight_flat : at -> Bannot.Weight.t array * string
(* val at_dist : at -> at -> int *)
(* val cat_dist : cat -> cat -> int *)
val cat_create_cost : cat -> int * int
val cat_delete_cost : cat -> int * int
val concat_ambiguous_split : int -> Brx.t -> Brx.t -> t -> t * t
val find_concat_split : Brx.t -> Brx.t -> int -> string -> int
val concat_split : Brx.t -> Brx.t -> t -> t * t
val star_ambiguous_split : int list -> Brx.t -> t -> t list
val find_star_split : Brx.t -> int list -> string -> int list
val star_split : Brx.t -> t -> t list
val do_concat : Brx.t -> Brx.t -> (attmp -> attmp) -> (attmp -> attmp) -> attmp -> attmp
val do_star : Brx.t -> (attmp -> attmp) -> attmp -> attmp
val annot_leaf : Bannot.Weight.t -> attmp -> attmp
val before_node : attmp -> attmp
val annot_node : Btag.t -> attmp -> attmp
val at_of_attmp : attmp -> at
