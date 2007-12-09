(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)

module Relschema : sig

  module Fd : sig
    type fd = Name.Set.t * Name.Set.t
    (** A functional dependency [(xs, ys)] asserts that the tuple of values
        associated with the attributes [ys] is detemined by the tuple of values
        associated with the attributes [xs]. *)

    val format_fd : fd -> unit
    (** Prints a functional dependency to [stdout]. *)

    val ranges_over : fd -> Name.Set.t -> bool
    (** Checks whether the functional dependency makes sense over the given
        domain. *)

    val member : Db.Relation.t -> fd -> bool
    (** [member r fd] checks whether [r] satisfies [fd]. *)

    val revise :
      Db.Relation.record -> Db.Relation.t -> fd -> Db.Relation.record
    (** [revise rcd r fds] implements record revision over the functional
        dependency.  The behavior is unspecified if [r] does not satisfy [fd].
        *)

    module Set : sig
    (** Sets of functional dependencies. *)

      include Set.S with type elt = fd

      val format_t : t -> unit
      (** Prints a set of functional dependencies to [stdout]. *)

      val ranges_over : t -> Name.Set.t -> bool
      (** Checks whether the set of functional dependencies makes sense over
          the given domain. *)

      val outputs : t -> Name.Set.t
      (** Returns the set of attributes that appear on the right-hand side of a
          functional dependency in an essential way.
          FIXME:  Currently this function simply returns the set of attributes
          that appear on the right-hand side of any functional dependency. *)

      val member : Db.Relation.t -> t -> bool
      (** [member r fds] checks whether [r] satisfies [fds]. *)

      exception Not_tree_form
      (** Indicates that an operation was given a set of functional
          dependencies that was not in tree form. *)

      val tree_form : t -> bool
      (** Returns [true] if the set of functional dependencies is in tree form.
          *)

      val revise :
        Db.Relation.record -> Db.Relation.t -> t -> Db.Relation.record
      (** [revise rcd r fd] implements record revision with a single functional
          dependencies [fds], which must be in tree form.  The behavior is
          unspecified if [r] does not satisfy [fd].
          @raise Not_tree_form if [fds] is not in tree form.
          *)

      val closure : Name.Set.t -> t -> t
      (** [closure u fds] returns the set of all functional dependencies that
          are implied by the [fds] over the universe of attributes [u].  The
          result is undefined if [ranges_over u fds] is false. *)

      (* Unnecessary until we begin using semantic equality.

      val minimal : t -> t
      val includes : t -> t -> bool
      val equiv : t -> t -> bool

      *)
    end
  end

  exception Attribute_not_found of Name.t
  (** Indicates that the given attribute was expected to be present but was
      not. *)

  exception Attribute_not_fresh of Name.t
  (** Indicates that the given attribute was not expected to be present but
      actually was. *)

  type t 
  (** The type of a relational schema. *)

  val format_t : t -> unit
  (** Prints a relational schema to [stdout]. *)

  val create : Name.Set.t -> t
  (** Create a relational schema over the given set of attributes, with no
      other constraints. *)

  val attributes : t -> Name.Set.t
  (** Returns the required domain of the relations that satisfy this schema. *)

  val rename : Name.t -> Name.t -> t -> t
  (** [rename a b rs] returns a relational schema in which all occurrences of
      [a] have been replaced by the fresh attribute name [b].
      @raise Attribute_not_found if [a] is not an attribute in the schema.
      @raise Attribute_not_fresh if [b] is already an attribute in the schema
      and is not equal to [a].  *)

  val get_fdset : t -> Fd.Set.t
  (** Returns the functional dependencies specified by the relational schema. *)

  val set_fdset : t -> Fd.Set.t -> t
  (** [set_fdset rs fds] returns a version of [rs] where the functional
      dependency constraints have been set to [fds].
      @raise Attribute_not_found if
        [Fd.Set.ranges_over fds (attributes rs)] is false. *)

  val get_pred : t -> Db.Relation.Pred.t
  (** Returns the record predicate specified by the relational schema. *)

  val set_pred : t -> Db.Relation.Pred.t -> t
  (** [set_pred rs p] returns a version of [rs] where the record predicate
      constraint has been set to [p].
      @raise Attribute_not_found if
        [Db.Relation.Pred.ranges_over p (attributes rs)] is false. *)

  val member : Db.Relation.t -> t -> bool
  (** [member r rs] checks whether [r] satisfies [rs]. *)

  val includes : t -> t -> bool
  (** [includes rs1 rs2] is [true] if every relation [r] that satisfies [rs2]
      also satisfies [rs1]. *)

  val equiv : t -> t -> bool
  (** [equiv rs1 rs2] is [true] if they describe the same set of relations. *)
end

exception Relname_not_found of Name.t
(** Indicates that a relational name was expected but was not found. *)

type t 
(** The type of database schemas. *)

val format_t : t -> unit
(** Prints a database schema to [stdout]. *)

val base : t
(** The database schema that describes database with no relations.  (There is
    only one such database.) *)

val extend : Name.t -> Relschema.t -> t -> t
(** [extend rn rs ds] describes databases that associate a relation satisfying
    [rs] with then name [rn] and whose other associations are described by [ds].
    *)

val remove : Name.t -> t -> t
(** [remove rn ds] describes database that have no relation associated with the
    name [rn] and whose other associations are described by [ds]. *)

val lookup : Name.t -> t -> Relschema.t
(** [lookus rn ds] returns the relational schema that describes the relations
    associated with [rn] in [ds].
    @raise Relation_not_found if [ds] describes databases that have no
    relation associated with [rn].  *)

val mem : Name.t -> t -> bool
(** [mem rn ds] returns [true] if the databases described by [ds] must
    associate the name [rn] with some relation. *)

val member : Db.t -> t -> bool
(** [member d ds] checks whether [d] satisfies [ds]. *)

val includes : t -> t -> bool
(** [includes ds1 ds2] is [true] if every database [d] that satisfies [ds2]
    also satisfies [ds1]. *)

val equiv : t -> t -> bool
(** [equiv ds1 ds2] is [true] if they describe the same set of relations. *)

