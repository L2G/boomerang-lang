(** Views *)

(* --------------------------------------------------------------------- *)
(**{2 The Type of Views} *)

type t
(** A view is a tree of names.  Formally, a [V.t] can be thought of as a
    partial function from [Name.t]'s to [V.t]'s. *)

module Hash : Hashtbl.S with type key = t

(* --------------------------------------------------------------------- *)
(**{2 Accessors} *)

val dom : t -> Name.Set.t
(** [dom v] returns the domain of [v]. *)

val singleton_dom : t -> Name.t
(** [atomic_dom v] returns the name of the single child of the singleton view
    [v]. Raises [Illformed] if [v] is not a singleton. *)

val get : t -> Name.t -> t option
(** [get v k] yields [Some kid] if [v(k) = kid], or [None] if [v] is undefined
    at [k]. *)

val get_required : t -> Name.t -> t 
(** [get_required v k] returns [v(k)], raising [Illformed] if [k] not in
    [dom v]. *) 

val is_empty : t -> bool
(** [is_empty v] returns true if [dom v] is the empty set. *)

val equal : t -> t -> bool
(** [equal v1 v2] is true v1 and v2 are identical trees. *)

val equal_opt : t option -> t option -> bool
(** [equal_opt v1 v2] returns true if [v1 = None] and [v2 = None] or if [v1 =
    Some v1'], [v2 = Some v2'] and [v1' = v2'].  Otherwise it returns false. *)

val is_singleton : t -> bool
val same_root_sort : t -> t -> bool
val is_list : t -> bool
val is_empty_list : t -> bool

val to_list : t -> (Name.t * t) list

(* --------------------------------------------------------------------- *)
(** {2 Creators} *)

exception Illformed of string * (t list)
(** Raised if someone attempts to create a view that violates
    well-formedness constraints. *)

val empty : t

val empty_list: t

val from_list : (Name.t * t) list -> t

val set : t -> Name.t -> t option -> t

val set_star : t -> (Name.t * t option) list -> t

val create_star : (Name.t * t option) list -> t
(** [create_star binds = set_star empty binds] *)

type desc =
    V of (Name.t * desc) list   (* a view *)
  | L of desc list              (* a list of views *)
  | Val of Name.t               (* a value *)  
  | In of t                     (* an existing view *)
  | E                           (* the empty view *)

val from_desc : desc -> t


(* --------------------------------------------------------------------- *)
(** {2 Special forms of views} *)

val singleton : Name.t -> t -> t
(** [singleton n v] returns a new view with [n] mapping to [v]. *)

val new_value : Name.t -> t
(** Returns a value made from the given name.  A value is a view with a
    single child and no grandchildren. *)

val get_value : t -> Name.t
(** [get_value v] returns the single element of [dom v] if [v] is a value and
    raises [Illformed] otherwise. *)

val is_value : t -> bool
(** Test whether a view is a value. *)

val get_field_value : t -> Name.t -> Name.t
(** [get_field_value v k] assumes that [v] has a child named [k], and that [k]
    has a child that is a value; returns the value **)

val get_field_value_option : t -> Name.t -> Name.t option
(** [get_field_value v k] returns [None] if [v] does not have a child named
    [k], otherwise returns [Some (get_field_value v k)] **)    

val set_field_value : t -> Name.t -> Name.t -> t
(** [set_field_value v k s] creates a value from [s], and stores it under
    [k] in [v] *)

val field_value : Name.t -> Name.t -> t
(** [field_value k s = set_field_value empty k s] *)

(* --------------------------------------------------------------------- *)
(** {2 Views representing lists} *)

val cons : t -> t -> t

val empty_list : t

val structure_from_list : t list -> t

val list_from_structure : t -> t list

val list_length : t -> int

val is_cons : t -> bool

(* raises Illformed if no head or no tail *)
val head : t -> t
val tail : t -> t

(* --------------------------------------------------------------------- *)
(** {2 Utility functions} *)

val map : (t -> (t option)) -> t -> t

val mapi : (Name.t -> t -> (t option)) -> t -> t

(** returns true if the given predicate is true for all child views *)
val for_all : (t -> bool) -> t -> bool

(** returns true if the given predicate is true for all name&child view pairs *)
val for_alli : (Name.t -> t -> bool) -> t -> bool

val fold : (Name.t -> t -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (Name.t -> t -> unit) -> t -> unit

val split : (Name.t -> bool) -> t -> t * t

val concat : t -> t -> t
(* raises [IllFormed] in case of domain collision *)

(* --------------------------------------------------------------------- *)
(** {2 Printing functions} *)

val format : t -> unit

val format_option : t option -> unit

val string_of_t : t -> string

type msg = [`String of string | `Name of Name.t | `Break | `View of t
          | `View_opt of t option | `Open_box | `Close_box ] 

exception Error of msg list

val format_msg : msg list -> unit

val error_msg : msg list -> 'a

val show_diffs : t -> t -> unit

(* --------------------------------------------------------------------- *)
(**{2 Names used to encode cons cells as trees} *)

val hd_tag : Name.t
val tl_tag : Name.t
val nil_tag : Name.t
