(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)

module Relschema : sig

  module Fd : sig
    type fd = Name.Set.t * Name.Set.t
    val format_fd : fd -> unit
    val ranges_over : fd -> Name.Set.t -> bool
    val member : Db.Relation.t -> fd -> bool
    val revise :
      Db.Relation.record -> Db.Relation.t -> fd -> Db.Relation.record

    module Set : sig
      include Set.S with type elt = fd

      val format_t : t -> unit
      val ranges_over : t -> Name.Set.t -> bool
      val outputs : t -> Name.Set.t
      val member : Db.Relation.t -> t -> bool

      val revise :
        Db.Relation.record -> Db.Relation.t -> t -> Db.Relation.record

      (* Unnecessary until we begin using semantic equality.

      val closure : t -> t
      val minimal : t -> t
      val includes : t -> t -> bool
      val equiv : t -> t -> bool

      *)
    end
  end

  type t 

  val format_t : t -> unit
  val create : Name.Set.t -> t
  val attributes : t -> Name.Set.t
  val get_fdset : t -> Fd.Set.t
  val set_fdset : t -> Fd.Set.t -> t
  val get_pred : t -> Db.Relation.Pred.t
  val set_pred : t -> Db.Relation.Pred.t -> t
  val member : Db.Relation.t -> t -> bool
  val includes : t -> t -> bool
  val equiv : t -> t -> bool
end

type t 

val format_t : t -> unit
val base : t
val extend : Name.t -> Relschema.t -> t -> t
val remove : Name.t -> t -> t
val lookup : Name.t -> t -> Relschema.t

val member : Db.t -> t -> bool
val includes : t -> t -> bool
val equiv : t -> t -> bool

