(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* info.ml - common definitions for reporting locations of errors              *)
(*******************************************************************************)
(* $Id$ *)

type pos = int * int
type t = pos * pos

(* dummy : t
 *     a dummy location *)
let dummy = ((-1,-1),(-1,-1))

(* string_of_t : t
 *     pretty prints a location for easy parsing in emacs compile-mode *)
let string_of_t ((l1,c1),(l2,c2)) = 
  if l2=l2
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2

(* merge_inc : t -> t -> t
 *     merge two locations; includes the endpoints *)
let merge_inc = fun (pos1,_) (_,pos2) -> (pos1,pos2)

(* merge_exc : t -> t -> t
 *     merge two locations; excludes the endpoints *)
let merge_exc = fun (_,pos1) (pos2,_) -> (pos1,pos2)
