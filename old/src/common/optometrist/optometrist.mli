(* The optometrist handles high-level management of lenses *)

type feature_tree_type = Syntax.ctx * Syntax.typ 
type type_desc = string list

val type_desc_as_string : type_desc -> string
(** [type_desc_list_as_string vt] returns a string representation of the
    type desc [vt]. *)

val type_desc_list_as_string : type_desc list -> string
(** [type_desc_list_as_string vts] returns a string representation of the set
    of type descs [vts]. *)

exception Bad_lens of string

val register_lens : type_desc -> type_desc -> feature_tree_type -> Lens.t -> unit
(** [register_lens concrete abstract ftt lens] registers a new lens which takes
    views of type [concrete] to type [abstract].
    @raise Bad_lens when the lens causes an ambiguity within the optometrist. *)

val can_sync_as : type_desc list -> (type_desc * (Lens.t list * feature_tree_type) list) list
(** [can_sync_as types] returns the set of all type descs which all of [types]
    can be synchronized as. *)

val can_sync_as1 : type_desc -> (type_desc * (Lens.t list * feature_tree_type)) list
(** convenience function for passing a singleton to (and getting one back
    from) can_sync_as. *)

val can_sync_as2 : type_desc * type_desc ->
  (type_desc * ((Lens.t list * feature_tree_type) * (Lens.t list * feature_tree_type))) list

(** convenience function for passing two type descs to can_sync_as. *)
  
