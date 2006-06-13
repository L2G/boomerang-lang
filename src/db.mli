(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: db.mli 1756 2006-05-31 15:08:03Z bohannon $ *)


module Relation : sig
(** A module for manipulating relational data. *)

  type record = Name.t Name.Map.t
  (** The type of records (elements in a relation). *)

  module Pred : sig
  (** A module implementing predicates on records. *)

    type t =
      | True
      | False
      | EqAttAtt of Name.t * Name.t
      | EqAttVal of Name.t * Name.t
      | Not of t
      | Conj of t * t
      | Disj of t * t
      | Impl of t * t

    val format_t : t -> unit
    (** Pretty-printing of predicates to [stdout]. *)

    val of_record : record -> t
    (** Returns a predicate that describes the given record and all extensions
        of it. *)

    val ranges_over : t -> Name.Set.t -> bool
    (** Check whether a predicate is compatible with a record domain. *)

    val ignores : t -> Name.Set.t -> bool
    (** Check whether a predicate is indifferent about the values
        associated with the given attributes.  The function may not necessarily
        be complete, i.e., it may return [false] when an answer of [true] would
        be appropriate. *)

    val member : record -> t -> bool
    (** Check whether a record is a member of the set described by the
        predicate. *)

    val includes : t -> t -> bool
    (** [includes p q] checks whether the set of records described by [q] is a
        subset of the set described by [p]. *)

    val equiv : t -> t -> bool
    (** Checks whether the sets of records described by two predicates are
        equal. *)

    val normalize : t -> t
    (** Convert a predicate to some normal form.  (Good for printing.) *)

  end

  type t
  (** The type of relations, with both field names and data elements of type
      [string]. *)

  val format_t : t -> unit
  (** Pretty-prints a [t] to [stdout]. *)

  val create : string list -> t
  (** Create a empty relation with the given set of field names.  As the
      semantics of relations require, the order of the fields given is
      unimportant (except for printing).
      @raise Error.Harmony_error if the list contains duplicate strings. *)

  val insert : record -> t -> t
  (** Add a record to a relation.  The record must have the correct set of
      fields.
      @raise Error.Harmony_error if the domain of the record does not match the
      domain of the relation. *)

  val insert_tuple : (string list) -> t -> t
  (** Add a record to a relation.  The record must have the correct set of
      fields.
      @raise Error.Harmony_error if the domain of the record does not match the
      domain of the relation. *)

  val rename : string -> string -> t -> t
  (** [rename m n r] renames the field [m] in [r] to [n].
      @raise Error.Harmony_error if [m] does not belong to the domain of [r].
      @raise Error.Harmony_error if [n] belongs to the domain of [r] minus [m].
      *)

  val select : Pred.t -> t -> t
  (** Create a new relation by selecting the records that satisfy the given
      predicate.
      @raise Error.Harmony_error if the predicate does not range over the domain
      of the relation.  *)

  val project : string list -> t -> t
  (** Create a new relation by projecting on the listed fields.
      @raise Error.Harmony_error if one of the fields given does not belong to
      the relation.  One of the fields that does not belong is reported.
      @raise Error.Harmony_error if the list contains duplicate strings. *)

  val join : t -> t -> t
  (** Return the natural join of the two arguments. *)

  val union : t -> t -> t
  (** Return the union of two relations.
      @raise Error.Harmony_error if the relations do not have the same domain.
      *)

  val inter : t -> t -> t
  (** Return the intersection of two relations.
      @raise Error.Harmony_error if the relations do not have the same domain.
      *)

  val diff : t -> t -> t
  (** Return the difference of two relations.
      @raise Error.Harmony_error if the relations do not have the same domain.
      *)

  val fields : t -> string list
  (** Return the field names of a relation in the same order that they were
      given to the function [create] or [project]. *)

  val fold : (record -> 'a -> 'a) -> t -> 'a -> 'a
  (** Perform a fold operation over the records in a relation. *)

  val choose : t -> record
  (** Return an arbitrary record from a relation.
      @raise Not_found if the relation is empty. *)

  val equal : t -> t -> bool
  (** Check two relations for equality. *)
end

type t

val format_t : t -> unit
val equal : t -> t -> bool
val empty : t
val extend : Name.t -> Relation.t -> t -> t
val remove : Name.t -> t -> t
val lookup : Name.t -> t -> Relation.t
val fold : (Name.t -> Relation.t -> 'a -> 'a) -> t -> 'a -> 'a

