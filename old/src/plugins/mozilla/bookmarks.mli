(** Generic helper functions for bookmark lenses *)
(*
val focus_item : Name.t -> Lens.t
(** Given a folder, [focus_item k] yields just the item named [k] (i.e.,
    the sub-folder or link whose [name] field equals [k]) from this
    folder. *)
 *)
val top_level_folder_name : Name.t
(** A conventional name for the top-level folder in a bookmark file *)

val toolbar_folder_name : Name.t
(** A conventional name for the folder containing toolbar bookmarks *)

val is_bookmarks : V.t -> bool
(** Determines if the given view is an abstract bookmarks view or not. *)
