type t

(* constants *)
val epsilon : t 
val anything : t
val empty : t

(* constructors *)
val mk_cset : (int * int) list -> t
val mk_neg_cset : (int * int) list -> t
val mk_string : string -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_diff : t -> t -> t
val mk_complement: t -> t
val mk_inter : t -> t -> t
val mk_reverse : t -> t

(* pretty printing *)
type r = 
  | Urnk           (* union *)
  | Drnk           (* diff *)
  | Irnk           (* inter *)
  | Crnk           (* concat *)
  | Srnk           (* star *)
  | Arnk           (* atomic *)
val lpar : r -> r -> bool
val rpar : r -> r -> bool
val rank : t -> r
val format_t : t -> unit
val string_of_t : t -> string

(* core operations *)
val is_empty : t -> bool
val is_singleton : t -> bool
val disjoint_cex : t -> t -> string option
val disjoint : t -> t -> bool
val equiv : t -> t -> bool
val representative : t -> string option

(* string matching *)
val match_string : t -> string -> bool
val match_string_positions : t -> string -> Int.Set.t
val match_string_reverse_positions : t -> string -> Int.Set.t

(* ambiguity *)
val splittable_cex : t -> t -> string option
val splittable : t -> t -> bool
val iterable_cex : t -> string option
val iterable : t -> bool

(* splitting *)
val split_positions : t -> t -> string -> Int.Set.t
val split_bad_prefix : t -> string -> string * string
val seq_split : t -> t -> string -> (string * string) option
val star_split : t -> string -> string list

val init : unit -> unit
