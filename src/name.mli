(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* name.mli - interface for names                  *)
(***************************************************)
(* $Id$ *)

type t = string
(** A name is just a string **)

module Set : Set.S with type elt = t
  (** Sets with names as elements **)

module Map : Mapplus.SMap with type key_t = t and type key_set = Set.t
  (** Finite maps with names as keys **)
  
module Hash : Hashtbl.S with type key = t
  (** Hashtables with names as keys **)
