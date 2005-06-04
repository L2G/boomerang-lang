(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(*********************************************************)
(* $Id$ *)

(** Focal types *)

type t = TT of pt | NT of pt
and pt

type thunk = unit -> pt
type it = 
    Empty of Info.t
  | Any of Info.t
  | Var of Info.t * Syntax.qid * thunk
  | App of Info.t * it * it * thunk
  | Fun of Info.t * (it -> it)
  | Name of Info.t * string * pt
  | Star of Info.t * (string list) * pt
  | Bang of Info.t * (string list) * pt
  | Cat of Info.t * pt list 
  | Union of Info.t * pt list 

val mk_ptype : it -> pt
val it_of_pt : pt -> it

(* utility functions *)
val info_of_t : t -> Info.t
val info_of_it : it -> Info.t
val string_of_t : t -> string
val string_of_it : it -> string

(* constants *)
val nil_it : it
val cons_it : it -> it -> it

(* -------------- Functions on types --------------- *)

val project : t -> Name.t -> t option
val member : V.t -> t -> bool 

(* -------------- Type domains --------------- *)
type tdom_atom =
    DAny of Name.Set.t
  | DAll of Name.Set.t
  | DName of string
      
module TDom : Set.S with type elt = tdom_atom
module TDoms : Set.S with type elt = TDom.t
	
val tdoms : t -> TDoms.t
val vdom_in_tdoms : Name.Set.t -> TDoms.t -> bool
