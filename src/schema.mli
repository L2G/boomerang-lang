(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(*********************************************************)
(* $Id$ *)

(** Focal types *)

(* --------------- representation --------------- *)
type t 
(** [t] is held abstract *)

(** [string_of_t t] pretty prints the schema [t] *)
val string_of_t : t -> string

(** [info_of_t t] returns the parsing information associated with [t] *)
val info_of_t : t -> Info.t

(* -------------- constructors --------------- *)
val mk_any : Info.t -> t
val mk_atom : Info.t -> Name.t -> t -> t
val mk_cat : Info.t -> t list -> t
val mk_union : Info.t -> t list -> t
val mk_var : Info.t -> Syntax.qid -> (unit -> t) -> t
val mk_wild : Info.t -> Name.Set.t -> int -> int option -> t -> t

(* -------------- constants --------------- *)
val mk_nil : Info.t -> t
val mk_cons : Info.t -> t -> t -> t

(* -------------- operations --------------- *)
val assert_wf : t -> Syntax.qid list -> unit
val project : t -> Name.t -> t option
val dom_member : V.t -> t -> bool
val member : V.t -> t -> bool
