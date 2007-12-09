(** Trees *)

(* --------------------------------------------------------------------- *)
(**{2 The Type of Trees} *)

type t
(** A [Tree.t] is a tree of names.  Formally, a [Tree.t] can be thought of as a
    partial function from [Name.t]'s to [Tree.t]'s. *)

val hash : t -> int
(** hash function for Tree.t *)

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

(* val create_star : (Name.t * t option) list -> t *)
(** [create_star binds = set_star empty binds] *)

(** A description type for trees. A tree can then be created from a description using [from_desc] *)
type desc =
    Tree of (Name.t * desc) list   (** A tree with names and associated children in a list *)
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
(** [equal v1 v2] is true if v1 and v2 are identical trees. *)

val included_in : t -> t -> bool
(** [included_in v1 v2] is true if every path defined in v1 is also defined in v2. *)

val compare : t -> t -> int
(** [compare v1 v2] can be used as a comparison function on trees. *)

(* --------------------------------------------------------------------- *)
(** {2 Special forms of trees} *)

val mk_value : Name.t -> t
(** Returns a value made from the given name.  A value is a tree with a
    single child and no grandchildren. *)

val get_value : t -> Name.t
(** [get_value v] returns the single element of [dom v] if [v] is a value.
    @raise an exception otherwise. *)

val is_value : t -> bool
(** Test whether a tree is a value. *)

(* --------------------------------------------------------------------- *)
(** {2 Trees representing lists} *)

val cons : t -> t -> t
(** [cons v t] returns the tree representing the list of head [v] and tail [t] *)

val empty_list : t
(** The tree representing the empty list *)

val is_empty_list : t -> bool
(** [is_empty_list v] returns true if and only if [v] is the tree representing the empy list. *)

val is_cons : t -> bool
(** [is_cons v] returns true if and only if [v] is a tree representing a cons cell. *)

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

val fold : (Name.t -> t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f v] a computes [(f kN tN ... (f k1 t1 a)...)], where [k1 ... kN] are
    the names of all children in [v]  (in increasing order), and [t1 ... tN]
    are the associated trees. *)

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
