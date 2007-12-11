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
(* /boomerang/src/crm.mli                                                      *)
(* Character range map interface                                               *)
(* $Id$                                                                        *)
(*******************************************************************************)

module Make
  (S:Set.S)
  (RS:sig
     type sym
     val char_code_min : sym
     val char_code_max : sym 
     val leq : sym -> sym -> bool
     val l : sym -> sym -> bool
     val compare_sym : sym -> sym -> int
     val pred : sym -> sym
     val succ : sym -> sym
   end): 
sig
  type 'a ext
  type t = S.t ext
  val empty : t 
  val single_trans : RS.sym -> S.elt -> t
  val map : (S.t -> S.t) -> t -> t
  val iter : ((RS.sym * RS.sym) -> S.t -> unit ) ->t -> unit
  val is_empty: t -> bool
  val fold : ((RS.sym * RS.sym) -> 'b -> 'a -> 'a) -> 'b ext -> 'a -> 'a
  val add : (RS.sym * RS.sym) -> S.t -> t -> t
  val add_elt : RS.sym -> S.t -> t -> t
  val rem : (RS.sym * RS.sym) -> t -> t
  val rem_elt : RS.sym -> t -> t
  val fill_holes : S.t -> t -> t
  val union : t -> t -> t
  val find : (RS.sym * RS.sym) -> t -> S.t
  val safe_find : (RS.sym * RS.sym) -> t -> S.t -> S.t
  val find_elt : RS.sym -> t -> S.t
  val safe_find_elt : RS.sym -> t -> S.t -> S.t
  val isect_range : (RS.sym * RS.sym) -> (RS.sym * RS.sym) -> (RS.sym * RS.sym) option
  val product : (S.t -> S.t -> S.t) -> t -> t -> t
  val ext_product : (S.t -> S.t -> 'a) -> t -> t -> ('a -> 'a -> 'a) -> 'a ext
end
