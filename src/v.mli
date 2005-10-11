(** Trees *)

(* --------------------------------------------------------------------- *)
(**{2 The Type of Trees} *)

type t
(** A [V.t] is a tree of names.  Formally, a [V.t] can be thought of as a
    partial function from [Name.t]'s to [V.t]'s. *)

(* --------------------------------------------------------------------- *)
(**{2 Accessors} *)

val get : t -> Name.t -> t option
(** [get v k] yields [Some kid] if [v(k) = kid], or [None] if [v] is undefined
    at [k]. *)

val get_required : ?msg:string -> t -> Name.t -> t 
(** [get_required v k] returns [v(k)].
    @raise an exception if the required child [k] is missing in [v].*)

val dom : t -> Name.Set.t
(** [dom v] returns the domain of [v]. *)

val singleton_dom : t -> Name.t
(** [singleton_dom v] returns the name of the single child of the singleton tree [v].
    @raise an exception if [v] is not a singleton. *)

val to_list : t -> (Name.t * t) list
(** [to_list v] returns the list of the children of [v], associated to their names. *)

(* --------------------------------------------------------------------- *)
(** {2 Creators} *)

val empty : t
(** The empty tree *)

val from_list : (Name.t * t) list -> t
(** Being the converse function of [to_list], [from_list l] returns the tree with
    the children described in the association list [l] *)

val set : t -> Name.t -> t option -> t
(** [set v k kid] sets the children under name [k] in [v] to [t] if [kid = Some t],
    and removes any previously existing entry under [k] if [kid = None] *)

val create_star : (Name.t * t option) list -> t
(** [create_star binds = set_star empty binds] *)

(** A description type for trees. A tree can then be created from a description using [from_desc] *)
type desc =
    V of (Name.t * desc) list   (** A tree with names and associated children in a list *)
  | L of desc list              (** A list of trees *)
  | Val of Name.t               (** A tree representing a single value *)  
  | In of t                     (** An existing tree *)
  | E                           (** The empty tree *)

val from_desc : desc -> t
(** [from_desc d] creates the tree corresponding to the description [d] *)

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

val compare : t -> t -> int
(** [compare v1 v2] can be used as a comparison function on trees. *)

(* val is_singleton : t -> bool *)
(* (\** [is_singleton v] returns true if [dom v] only has one element, and false otherwise. *\) *)

(* val same_root_sort : t -> t -> bool *)
(* (\** [same_root_sort v u] returns true if the domains of [v] and [u] are equal. *\) *)


(* --------------------------------------------------------------------- *)
(** {2 Special forms of trees} *)

(* val singleton : Name.t -> t -> t *)
(* (\** [singleton n v] returns a new tree with [n] mapping to [v]. *\) *)

val new_value : Name.t -> t
(** Returns a value made from the given name.  A value is a tree with a
    single child and no grandchildren. *)

val get_value : t -> Name.t
(** [get_value v] returns the single element of [dom v] if [v] is a value.
    @raise an exception otherwise. *)

val is_value : t -> bool
(** Test whether a tree is a value. *)

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
    corresponding tree *)

(* --------------------------------------------------------------------- *)
(** {2 Trees representing lists} *)

val cons : t -> t -> t
(** [cons v t] returns the tree representing the list of head [v] and tail [t] *)

val spined_cons : t -> t -> t
(** [spined_cons v t] returns the tree representing the spined list of head [v] and tail [t] *)

val empty_list : t
(** The tree representing the empty list *)

val is_empty_list : t -> bool
(** [is_empty_list v] returns true if and only if [v] is the tree representing the empy list. *)

val structure_from_list : t list -> t
(** [structure_from_list tl] returns the tree representing the list of trees [tl] *)

val list_from_structure : t -> t list
(** [list_from_structure t] assumes [t] is a list and returns an ocaml list of the trees in [t].
    @raise an exception if [t] is not a list *)

val list_length : t -> int
(** [list_length t] assumes [t] is a list, and returns its length.
    @raise an exception if [t] is not a list *)

val hd_tag : Name.t
(** The tag name for the head of a list *)

val tl_tag : Name.t
(** The tag name for the tail of a list *)

val nil_tag : Name.t
(** The tag name for the empty list *)


(* --------------------------------------------------------------------- *)
(** {2 Utility functions} *)

(* val map : (t -> (t option)) -> t -> t *)
(* (\** [map f v] returns a tree with same domain as [v], where the associated value [a] of all *)
(*     bindings of [v] has been replaced by the result of the application of [f] to [a]. The  *)
(*     bindings are passed to [f] in increasing order with respect to the alphabetical order *)
(*     on the names in the domain *\) *)

(* val mapi : (Name.t -> t -> (t option)) -> t -> t *)
(* (\** Same as [map], but the function receives as arguments both the name and the associated *)
(*     tree for each child in the tree. *\) *)

(* val for_all : (t -> bool) -> t -> bool *)
(* (\** returns true if the given predicate is true for all child trees *\) *)

(* val for_alli : (Name.t -> t -> bool) -> t -> bool *)
(* (\** returns true if the given predicate is true for all name&child tree pairs *\) *)

val fold : (Name.t -> t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f v] a computes [(f kN tN ... (f k1 t1 a)...)], where [k1 ... kN] are
    the names of all children in [v]  (in increasing order), and [t1 ... tN]
    are the associated trees. *)

(* val iter : (Name.t -> t -> unit) -> t -> unit *)
(* (\** [iter f v] applies [f] to all children in [v]. [f] receives the name as first *)
(*     argument, and the associated tree as second argument. The names are passed to *)
(*     [f] in alphabetical order. *\) *)

val split : (Name.t -> bool) -> t -> t * t
(** [split p v] splits the tree [v] according to the predicate [p] on names.
    Returns the pair of trees [(v1,v2)], such all names in [dom v1] verify [p]. *)

val is_list : t -> bool
  (** [is_list v] returns true iff [v] is a tree that encodes a list. *)

val concat : t -> t -> t
(** [concat v1 v2] yields the tree containing all the children of [v1] and [v2].
    @raise an exception in case of domain collision *)

(* --------------------------------------------------------------------- *)
(** {2 Pretty-printing of trees} *)

val format_t : t -> unit
(** The tree passed to [format] is formatted to a string and printed to the standard output. *)

val raw : bool Prefs.t

val string_of_t : t -> string
(** [string_of_t v] returns the formatted string representing [v] without actually printing it. *)

val show_diffs : t -> t -> unit
(** Shows the differences between the two trees passed as arguments. *)

(** {2 Formatting of error messages} *)
(**  A type for easy formatting of error messages *)
type msg = [`String of string | `Name of Name.t | `Break | `Space | `SpaceOrIndent | `Tree of t
           | `Tree_opt of t option | `Prim of (unit -> unit) 
           | `Open_box | `Open_vbox | `Close_box ]

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

