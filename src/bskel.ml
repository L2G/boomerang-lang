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
(* src/bskel.ml                                                               *)
(* skeletons                                                                  *)
(* $Id$ *)
(******************************************************************************)

module TLMap = Btag.ListMap

type n =
  | Str of Bannot.leaf * string
  | Box of Btag.t
  | Lst of t list
and t = Bannot.node * n

let string an al s = an, Str (al, s)
let box an t = an, Box t
(* let anot_leaf a (l, n) = *)
(*   (l, ( *)
(*      match n with *)
(*      | Str (l, s) -> Str (a l, s) *)
(*      | _ -> assert false *)
(*    ) *)
(*   ) *)
(* let anot_node a (l, n) = (a l, n) *)

let mk_star an l = an, Lst l
let mk_seq an a b = mk_star an [a; b]

let gread (concat, empty, (node, leaf)) t lm =
  let onfst f (a, b) = (f a, b) in
  let onsnd f (a, b) = (a, f b) in
  let rec m t lm =
    let an, n = t in
    onfst (node an) (
      match n with
      | Str (af, s) ->
          leaf af s, lm
      | Box t -> (
          try
            match lm with
            | None -> empty, None
            | Some lm ->
                onsnd (fun x -> Some x) (TLMap.next t lm)
          with Not_found -> assert false)
      | Lst l ->
          Safelist.fold_left (
            fun (a1, lm) t ->
              onfst (concat a1) (m t lm)
          ) (empty, lm) l
    )
  in
  let a, lm = m t lm in
  assert (
    match lm with
    | None -> true
    | Some lm -> TLMap.is_empty lm
  );
  a

let read p t l = gread p t (Some l)

let flat p t = gread p t None
