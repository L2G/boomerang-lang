(** A module for manipulating relational data. *)

type t
(** The type of relations, with both field names and data elements of type
    [string]. *)

type record = (string * string) list
(** The type of records (elements in a relation). *)

exception Unequal_domains of string list * string list
(** An exception raised when an operation expects objects with equivalent
    domains.  The two domains are reported. *)

exception Domain_excludes of string list * string
(** An exception raised when an operation expects a name that does belong
    to a domain.  The domain and name are reported. *)

exception Domain_includes of string list * string
(** An exception raised when an operation expects a name that does {e not}
    belong to a domain.  The domain and name are reported. *)

val create : string list -> t
(** Create a empty relation with the given set of field names.  As the
    semantics of relations require, the order of the fields given is
    unimportant.  Duplicate field names will be silently ignored. *)

val fields : t -> string list
(** Return the field names of a relation. *)

val insert : record -> t -> t
(** Add a record to a relation.  The record must have the correct set of
    fields.
    @raise Unequal_domains if the domain of the record does not match the
    domain of the relation. *)

val fold : (record -> 'a -> 'a) -> t -> 'a -> 'a
(** Perform a fold operation over the records in a relation. *)

val rename : string -> string -> t -> t
(** [rename m n r] renames the field [m] in [r] to [n].
    @raise Domain_excludes if [m] does not belong to the domain of [r].
    @raise Domain_includes if [n] belongs to the domain of [r]. *)

val project : string list -> t -> t
(** Create a new relation by projecting on the listed fields.  Duplicate field
    names will be silently ignored.
    @raise Domain_excludes if one of the fields given does not belong to the
    relation.  One of the fields that does not belong is reported. *)

val union : t -> t -> t
(** Create the union of two relations.
    @raise Unequal_domains if the relations do not have the same domain. *)

val inter : t -> t -> t
(** Create the intersection of two relations.
    @raise Unequal_domains if the relations do not have the same domain. *)

val diff : t -> t -> t
(** Create the difference of two relations.
    @raise Unequal_domains if the relations do not have the same domain. *)

val equal : t -> t -> bool
(** Check two relations for equality. *)

val dump_stderr : t -> unit

