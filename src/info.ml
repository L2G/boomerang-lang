(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* info.ml - common definitions for reporting locations of errors              *)
(*******************************************************************************)
(* $Id$ *)

type pos = int * int
type t = I of pos * pos | M of string

(* string_of_t : t
 *     pretty prints a location for easy parsing in emacs compile-mode *)
let string_of_t = function
    I ((l1,c1),(l2,c2)) -> 
      if l2=l2
      then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
      else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2
  | M s -> s
	
(* merge_inc : t -> t -> t
 *     merge two locations; includes the endpoints *)
let merge_inc = 
  function I (pos1,_) -> 
    begin 
      function I (_,pos2) -> I (pos1,pos2)
	| _ -> assert false (* FIXME: throw some exception *)
    end
    | _ -> assert false

(* merge_exc : t -> t -> t
 *     merge two locations; excludes the endpoints *)
let merge_exc = 
  function I (_,pos1) -> 
    begin 
      function I (pos2,_) -> I (pos1,pos2)
	| _ -> assert false (* FIXME: throw some exception *)
    end
    | _ -> assert false
