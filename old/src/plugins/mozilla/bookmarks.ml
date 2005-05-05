open Pervasives
(*
let pivot_item_name =
  compose [
    promote1 "name";
    pivot "name";
  ]

let focus_item k =
  tracepoint "focus_item" [
    hoist "folder";
    focus "contents" V.empty;  (* Not right *)
    map_list pivot_item_name;
    flatten;
    focus k V.empty;
  ]
 *)
let top_level_folder_name = "Top-level bookmark folder"
let toolbar_folder_name = "Abstract toolbar folder"

(* --- Abstract view type-checking:  is it a bookmarks view? --- *)
(* this will get replaced by a real schema/typing system of some sort.. *)

(* for this one, i wish i could write something like:
  v lookslike [ "name" => v' which satisfies V.is_value;
                "url" => v' which satisfies V.is_value] *)
let is_link v =
  Safelist.length (Name.Set.elements (V.dom v)) = 2 &
  V.for_alli
  (fun key v' -> match key with
    | "name" | "url" -> V.is_value v'
    | s -> print_endline("[bookmarks] found something other than name and url "
                       ^ "in link: " ^ s); false)
  v

(* for this one, i wish i could write something like:
  v lookslike [ list of v' which all satisfy is_element ] *)
let rec is_contents v =
  (V.for_alli
  (fun key v' -> match key with
    (* want to say: must be BOTH *h and *t, with given prodicates *)
      "*h" -> is_element v'
    | "*t" -> (V.is_empty v') or (is_contents v')
    | s -> print_endline("[bookmarks] found something other than *h and *t "
                       ^ "in contents: " ^ s); false)
  v)

and is_folder v =
  (V.for_alli
    (fun key v' -> match key with
      (* want to say: must be BOTH contents and name, with given predicates *)
        "contents" -> is_contents v'
      | "name" -> V.is_value v'
      | s -> print_endline("[bookmarks] found something other than contents"
                         ^ " and name in folder: " ^ s); false)
  v)

and is_element v =
  (V.for_alli
    (fun key v' -> match key with
        "link" -> is_link v'
      | "folder" -> is_folder v'
      | s -> print_endline("[bookmarks] found something other than link and "
                         ^ "folder in element: " ^ s); false)
  v)

(** Determines whether the given view is an abstract bookmarks view or not. *)
(* Note: this implementation ensures that there is no extraneous information in
the view, but it does NOT ensure that all of the required info is there.  *)
let is_bookmarks v =
  (V.is_empty v) or
  (V.for_alli
    (fun key v' -> match key with
        "folder" -> is_folder v'
      | s -> print_endline ("[bookmarks] found something other than folder in "
                          ^ "toplevel bookmarks: " ^ s); false)
  v)
