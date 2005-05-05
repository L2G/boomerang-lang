(** Miscellaneous useful stuff *)

(* Hashtbl stuff *)
val safe_hash_add : ('a,'b) Hashtbl.t -> 'a -> 'b -> unit

(* List stuff *)
val enum : 'a list -> (int * 'a) list
val uniq : 'a list -> bool
val union : 'a list -> 'a list -> 'a list
val remove : 'a -> 'a list -> 'a list
val safeheadtail : 'a list -> 'a option * 'a list
val safetail : 'a list -> 'a list
val map_option : ('a -> 'b) -> 'a option -> 'b option
val fold_left2 :
  ('a -> 'b option -> 'c option -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val fold_left3 :
  ('a -> 'b option -> 'c option -> 'd option -> 'a) ->
  'a -> 'b list -> 'c list -> 'd list -> 'a
val fold_left2_with_pad :
  ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'b -> 'c -> 'a
val map2opt : ('a option -> 'b option -> 'c) -> 'a list -> 'b list -> 'c list
val map2_with_pad :
  ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'a -> 'b -> 'c list
val zip_with_pad : 'a -> 'b -> 'a list -> 'b list -> ('a * 'b) list
val iter_with_sep : ('a -> unit) -> (unit -> 'b) -> 'a list -> unit
val rev_and_flatten : 'a list list -> 'a list
val safeassoc : ('a * 'a) list -> 'a -> 'a
val partition : int -> 'a list -> 'a list * 'a list
val composel : ('a -> 'a) list -> ('a -> 'a)
(** composes a list of functions.  applies the leftmost function first. *)

(* Exception stuff *)
exception Bad of string
val bad : string -> 'a
exception Unimplemented of string

(* String stuff *)
val escape : (char -> string) -> string -> string
val unescaped : string -> string (* inverse of String.escaped *)
val generic_escape_char : string -> char -> string
val generic_unescape : string -> string
val index_rec_nonescape : string -> int -> char -> int
val split_nonescape : char -> string -> string list
val is_blank : string -> bool
val bounded_index_from : string -> int -> int -> char -> int
val findsubstring : string -> string -> int option
val trimLeadingSpaces : string -> string
val replace_substring : string -> string -> string -> string
val replace_substrings : string -> (string * string) list -> string
val whack : string -> string
val unwhack : string -> string
val hexify_string : string -> string
val splitIntoWords : string -> char -> string list
val filename_extension : string -> string

type color = Black | Red | Green | Yellow | Blue | Pink | Cyan | White
val color : string -> ?bold:bool -> color -> string
(** ansi-colors a string.  defaults to not bold. *)

(* File handling stuff *)
val is_dir : string -> bool
(** [is_dir f] evaluates to true iff the file at location f is a directory *)
val mkdir_forsure : string -> unit
(** [mkdir_forsure d] checks if a directory exists.  If not, it tries to make
   it. *)
val read_dir : string -> string list
(** [read_dir dir] evaluates to the list of files within [dir]. *)
val in_dir : string -> (unit -> 'a) -> 'a
(** [in_dir d f] evaluates the function [f] with the working directory set to
   [d], and restores the original working directory before returning. *)
val remove_file_or_dir : string -> unit
val read : string -> string
val write : string -> string -> unit
val backup : string -> unit
val tempFileName : string -> string

(* IO stuff *)
val read_char : unit -> char

(* Dynamically scoped variables *)
val dynamic_var : 'a -> 'a ref
val dynamic_lookup : 'a ref -> 'a
val dynamic_bind : 'a ref -> 'a -> (unit -> 'b) -> 'b
