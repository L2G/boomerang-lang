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
(* /boomerang/src/bregexp.ml                                                   *)
(* Boomerang regular expressions                                               *)
(* $Id: bregexp.ml 3937 2008-07-04 15:44:46Z jnfoster $                                                                        *)
(*******************************************************************************)

module R = Brx

type t = R.t

(* constants *)
let epsilon = R.epsilon 
let anything = R.anything
let empty = R.empty

(* constructors *)
let mk_cset = R.mk_cset
let mk_neg_cset = R.mk_neg_cset
let mk_string = R.mk_string
let mk_alt = R.mk_alt
let mk_seq = R.mk_seq
let mk_star = R.mk_star
let mk_iter = R.mk_iter
let mk_diff = R.mk_diff
let mk_complement= R.mk_complement
let mk_inter = R.mk_inter
let mk_expand = R.mk_expand

(* pretty printing *)
type r = R.r
let arnk = R.Arnk
let crnk = R.Crnk
let urnk = R.Urnk
let srnk = R.Srnk
let drnk = R.Drnk

let rank = R.rank
let lpar = R.lpar
let rpar = R.rpar

let format_t = R.format_t
let string_of_t = R.string_of_t

(* operations *)
let is_empty = R.is_empty
let is_singleton = R.is_singleton
let disjoint_cex = R.disjoint_cex
let disjoint = R.disjoint
let equiv = R.equiv
let representative = R.representative
let derivative = R.derivative

(* string matching *)
let match_string = R.match_string
let match_string_positions = R.match_string_positions

(* ambiguity *)
let splittable_cex = R.splittable_cex
let splittable = R.splittable
let iterable_cex = R.iterable_cex
let iterable = R.iterable

(* splitting *)
let split_positions = R.split_positions
let split_bad_prefix = R.split_bad_prefix
let seq_split = R.seq_split
let star_split = R.star_split

