(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* info.ml - common definitions for reporting locations of errors             *)
(*                                                                             *)
(* $Id$ *)
(*                                                                             *)
(*******************************************************************************)

(* location info returned by the lexer              *)
(*     : a pos consists of a line and a char column *)
(*     : a t consists of a start and end pos        *)
type pos = int * int
type t = pos * pos

(* pretty printer *)
let string_of_t ((l1,c1),(l2,c2)) = 
  Printf.sprintf "line %d, char %d, to line %d, char %d " l1 c1 l2 c2

(* dummy info *)
let bogus = ((0,0),(0,0))

(* helper functions for constructing and extracting ts *)
let merge_inc = fun (pos1,_) (_,pos2) -> (pos1,pos2)
let merge_exc = fun (_,pos1) (pos2,_) -> (pos1,pos2)


