(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: v.mli 1756 2006-05-31 15:08:03Z bohannon $ *)

type t =
  | Tree of Tree.t
  | Db of Db.t

val tree_of: Info.t -> t -> Tree.t
val db_of: Info.t -> t -> Db.t

val format_t : t -> unit
(** The datum passed to [format] is formatted to a string and printed to the
    standard output. *)

val equal : t -> t -> bool
(** [equal v1 v2] is true if v1 and v2 are the same. *)

(** {2 Formatting of error and status messages} *)
(**  A type for easy formatting of error and status messages *)
type msg = [`String of string | `Name of Name.t | `Newline | `Break | `Space 
           | `SpaceOrIndent | `Tree of Tree.t | `Tree_opt of Tree.t option
           | `Db of Db.t | `V of t 
           | `Prim of (unit -> unit) | `Open_box | `Open_vbox | `Close_box ]

(* exception Error of msg list *)
(** General exception for errors in Harmony *)

val format_msg : msg list -> unit
(** Prints a message list to the standard error output. *)

val format_msg_as_string : msg list -> string
(** Prints a message list to a string. *)

val error_msg : msg list -> 'a
(** @raise Error with the message list passed as argument *)

(* --------------------------------------------------------------------- *)
(**{2 Pre-built hashtables of trees} *)

(** Hash tables with trees as keys *)
module Hash : Hashtbl.S with type key = t

