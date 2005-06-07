(** Views (trees) *)

(* --------------------------------------------------------------------- *)
(**{2 The Type of Views} *)

type t
(** A view is a tree of names.  Formally, a [V.t] can be thought of as a
    partial function from [Name.t]'s to [V.t]'s. *)

(* --------------------------------------------------------------------- *)
(**{2 Accessors} *)

val get : t -> Name.t -> t option
(** [get v k] yields [Some kid] if [v(k) = kid], or [None] if [v] is undefined
    at [k]. *)

val get_required : t -> Name.t -> t 
(** [get_required v k] returns [v(k)].
    @raise Illformed if the required child [k] is missing in [v].*)

val dom : t -> Name.Set.t
(** [dom v] returns the domain of [v]. *)

val singleton_dom : t -> Name.t
(** [singleton_dom v] returns the name of the single child of the singleton view [v].
    @raise Illformed if [v] is not a singleton. *)

val to_list : t -> (Name.t * t) list
(** [to_list v] returns the list of the children of [v], associated to their names. *)

(* --------------------------------------------------------------------- *)
(** {2 Creators} *)

exception Illformed of string * (t list)
(** Raised if someone attempts to create a view that violates well-formedness constraints. *)

val empty : t
(** The empty view *)

val from_list : (Name.t * t) list -> t
(** Being the converse function of [to_list], [from_list l] returns the view with
    the children described in the association list [l] *)

val set : t -> Name.t -> t option -> t
(** [set v k kid] sets the children under name [k] in [v] to [t] if [kid = Some t],
    and removes any previously existing entry under [k] if [kid = None] *)

val create_star : (Name.t * t option) list -> t
(** [create_star binds = set_star empty binds] *)

(** A description type for views. A view can then be created from a description using [from_desc] *)
type desc =
    V of (Name.t * desc) list   (** A view with names and associated children in a list *)
  | L of desc list              (** A list of views *)
  | Val of Name.t               (** A view representing a single value *)  
  | In of t                     (** An existing view *)
  | E                           (** The empty view *)

val from_desc : desc -> t
(** [from_desc d] creates the view corresponding to the description [d] *)

(* --------------------------------------------------------------------- *)
(**{2 Tests} *)

(* only one external occurrence in type.ml, perhaps not necessary ? *)
val is_empty : t -> bool
(** [is_empty v] returns true if [dom v] is the empty set. *)

val equal : t -> t -> bool
(** [equal v1 v2] is true v1 and v2 are identical trees. *)

(* val equal_opt : t option -> t option -> bool *)
(* (\** [equal_opt v1 v2] returns true if [v1 = None] and [v2 = None] or if [v1 = *)
(*     Some v1'], [v2 = Some v2'] and [v1' = v2'].  Otherwise it returns false. *\) *)

(* val is_singleton : t -> bool *)
(* (\** [is_singleton v] returns true if [dom v] only has one element, and false otherwise. *\) *)

(* val same_root_sort : t -> t -> bool *)
(* (\** [same_root_sort v u] returns true if the domains of [v] and [u] are equal. *\) *)


(* --------------------------------------------------------------------- *)
(** {2 Special forms of views} *)

(* val singleton : Name.t -> t -> t *)
(* (\** [singleton n v] returns a new view with [n] mapping to [v]. *\) *)

val new_value : Name.t -> t
(** Returns a value made from the given name.  A value is a view with a
    single child and no grandchildren. *)

val get_value : t -> Name.t
(** [get_value v] returns the single element of [dom v] if [v] is a value.
    @raise Illformed otherwise. *)

val is_value : t -> bool
(** Test whether a view is a value. *)

(* val get_field_value : t -> Name.t -> Name.t *)
(* (\** [get_field_value v k] assumes that [v] has a child named [k], and that [k] is a value ; returns this value. *)
(*     @raise Illformed if there is no child named [k], or if it is not a value *\) *)

(* val get_field_value_option : t -> Name.t -> Name.t option *)
(* (\** [get_field_value v k] returns [None] if [v] does not have a child named *)
(*     [k], otherwise returns [Some (get_field_value v k)] *)
(*     @raise Illformed if [v] has a child named [k], but [k] is not a value *\) *)    

(* val set_field_value : t -> Name.t -> Name.t -> t *)
(* (\** [set_field_value v k s] creates a value from [s], and stores it under *)
(*     [k] in [v] *\) *)

val field_value : Name.t -> Name.t -> t
(** [field_value k s] creates a value from [s], stores it under [k] and returns the
    corresponding view *)

(* --------------------------------------------------------------------- *)
(** {2 Views representing lists} *)

val cons : t -> t -> t
(** [cons v t] returns the view representing the list of head [v] and tail [t] *)

val empty_list : t
(** The view representing the empty list *)

val is_empty_list : t -> bool
(** [is_empty_list v] returns true if and only if [v] is the view representing the empy list. *)

val structure_from_list : t list -> t
(** [structure_from_list tl] returns the view representing the list of views [tl] *)

val list_from_structure : t -> t list
(** [list_from_structure t] assumes [t] is a list and returns an ocaml list of the views in [t].
    @raise Illformed if [t] is not a list *)

val list_length : t -> int
(** [list_length t] assumes [t] is a list, and returns its length.
    @raise Illformed if [t] is not a list *)

val hd_tag : Name.t
(** The tag name for the head of a list *)

val tl_tag : Name.t
(** The tag name for the tail of a list *)

val nil_tag : Name.t
(** The tag name for the empty list *)


(* --------------------------------------------------------------------- *)
(** {2 Utility functions} *)

(* val map : (t -> (t option)) -> t -> t *)
(* (\** [map f v] returns a view with same domain as [v], where the associated value [a] of all *)
(*     bindings of [v] has been replaced by the result of the application of [f] to [a]. The  *)
(*     bindings are passed to [f] in increasing order with respect to the alphabetical order *)
(*     on the names in the domain *\) *)

(* val mapi : (Name.t -> t -> (t option)) -> t -> t *)
(* (\** Same as [map], but the function receives as arguments both the name and the associated *)
(*     view for each child in the view. *\) *)

(* val for_all : (t -> bool) -> t -> bool *)
(* (\** returns true if the given predicate is true for all child views *\) *)

(* val for_alli : (Name.t -> t -> bool) -> t -> bool *)
(* (\** returns true if the given predicate is true for all name&child view pairs *\) *)

val fold : (Name.t -> t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f v] a computes [(f kN tN ... (f k1 t1 a)...)], where [k1 ... kN] are
    the names of all children in [v]  (in increasing order), and [t1 ... tN]
    are the associated trees. *)

(* val iter : (Name.t -> t -> unit) -> t -> unit *)
(* (\** [iter f v] applies [f] to all children in [v]. [f] receives the name as first *)
(*     argument, and the associated tree as second argument. The names are passed to *)
(*     [f] in alphabetical order. *\) *)

val split : (Name.t -> bool) -> t -> t * t
(** [split p v] splits the view [v] according to the predicate [p] on names.
    Returns the pair of views [(v1,v2)], such all names in [dom v1] verify [p]. *)

val is_list : t -> bool
  (** [is_list v] returns true iff [v] is a view that encodes a list. *)

val concat : t -> t -> t
(** [concat v1 v2] yields the view containing all the children of [v1] and [v2].
    @raise Illformed in case of domain collision *)

(* --------------------------------------------------------------------- *)
(** {2 Pretty-printing of views} *)

val format : t -> unit
(** The view passed to [format] is formatted to a string and printed to the standard output. *)

val string_of_t : t -> string
(** [string_of_t v] returns the formatted string representing [v] without actually printing it. *)

val show_diffs : t -> t -> unit
(** Shows the differences between the two views passed as arguments. *)

(** {2 Formatting of error messages} *)
(**  A type for easy formatting of error messages *)
type msg = [`String of string | `Name of Name.t | `Break | `View of t
           | `View_opt of t option | `Open_box | `Open_vbox | `Close_box ]

(* exception Error of msg list *)
(** General exception for errors in Harmony *)

val format_msg : msg list -> unit
(** Prints a message list to the standard error output. *)

val format_msg_as_string : msg list -> string
(** Prints a message list to a string. *)

val error_msg : msg list -> 'a
(** @raise Error with the message list passed as argument *)

(* --------------------------------------------------------------------- *)
(**{2 Pre-built hashtables of views} *)

(** Hash tables with views as keys *)
module Hash : Hashtbl.S with type key = t

