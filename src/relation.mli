(** A module for manipulating relational data. *)

type t
(** The type of relations, with both field names and data elements of type
    [string]. *)

type record = (string * string) list
(** The type of records (elements in a relation). *)

exception Type_error of string
(** An exception raised when an operation is called on a relation with
    an inappropriate set of fields. *)

val create : string list -> t
(** Create a empty relation with the given set of field names.  As the
    semantics of relations require, the order of the fields given is
    unimportant.  Duplicate field names will be silently ignored. *)

val fields : t -> string list
(** Return the field names of a relation. *)

val insert : record -> t -> t
(** Add a record to a relation.  The record must have the correct set of
    fields. *)

val fold : (record -> 'a -> 'a) -> t -> 'a -> 'a
(** Perform a fold operation over the records in a relation. *)

val rename : string -> string -> t -> t
(** [rename m n r] renames the field [m] in [r] to [n].  The field name [m] must
    exist in the relation and the name [n] must not exist; otherwise,
    {!Relation.Type_error} will be thrown. *)

val project : string list -> t -> t
(** Create a new relation by projecting on the listed fields.  Duplicate field
    names will be ignored.  The exception {!Relation.Type_error} will be thrown
    if the fields given are not a subset of the actual fields of the
    relation. *)

val union : t -> t -> t
(** Create the union of two relations.  They must have the same field names. *)

val inter : t -> t -> t
(** Create the intersection of two relations.  They must have the same field 
    names. *)

val diff : t -> t -> t
(** Create the difference of two relations.  They must have the same field 
    names. *)

val equal : t -> t -> bool
(** Check two relations for equality. *)

