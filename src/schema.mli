(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(*********************************************************)
(* $Id$ *)

(** Focal types *)

(* --------------- Representation --------------- *)
(** re-export [Value.ty] and [Value.string_of_ty] *)
type t = Value.ty
val string_of_t : t -> string

(* -------------- Constants --------------- *)

val mk_nil : Info.t -> t
val mk_cons : Info.t -> t -> t -> t

(* -------------- Functions on types --------------- *)

val project : Value.ty -> Name.t -> Value.ty option
val member : V.t -> Value.ty -> bool 

(* -------------- Type domains --------------- *)
type tdom_atom =
    DAny of Name.Set.t
  | DAll of Name.Set.t
  | DName of string
      
module TDom : Set.S with type elt = tdom_atom
module TDoms : Set.S with type elt = TDom.t
	
val tdoms : Value.ty -> TDoms.t
val vdom_in_tdoms : Name.Set.t -> TDoms.t -> bool
