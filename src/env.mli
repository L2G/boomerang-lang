(***********************************************)
(* The Harmony Project                         *)
(* harmony@lists.seas.upenn.edu                *)
(*                                             *)
(* env.mli - interface for Focal environments  *)
(***********************************************)
(* $Id$ *)

(** Environments for resolving Focal qualified identifiers *)

type 'a t
(** Abstract type of environments *)

val empty : unit -> 'a t 
(** [empty ()] yields a fresh environment. *)

val update : 'a t -> Syntax.qid -> 'a -> 'a t
  (** [update ev q r] extends [ev] with a binding for [qid] and
      [rv]. Returns a new environment *)

val overwrite : 'a t -> Syntax.qid -> 'a -> 'a t
  (** [overwrite ev q r] returns [ev], with the binding for [q]
      overwritten to [r]. If no binding for [q] exists, then extends [ev]
      with [q] bound to [r]. *)

val lookup : 'a t -> Syntax.qid -> 'a option
  (** [lookup ev q] returns an option representing the binding for [q]
      in [ev]. *)

val format_t : 'a t -> ('a -> unit) -> unit
  (** [format_t format_r ev] pretty prints [ev] using format_r. *)
  
val iter : (Syntax.qid -> 'a -> unit) -> 'a t -> unit
  
