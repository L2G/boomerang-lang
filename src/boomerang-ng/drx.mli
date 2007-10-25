(* regular expressions -- abstract type *)
type t

type string = Fclstr.FS.t
type char = Fclstr.FS.elt 

(* constructors *)
val epsilon : t 
val empty : t
val mk_str : bool -> string -> t
val mk_cset : bool -> (char * char) list -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_rep : int -> int option -> t -> t
val mk_complement : t -> t
val mk_diff : t -> t -> t
val mk_inter : t -> t -> t

val mk_lowercase : t -> t
val mk_uppercase : t -> t

val is_empty : t -> bool
val equiv : t -> t -> bool
val representative: t -> string 

val determinize : t -> t

val match_str : t -> string -> bool
val find_exit_automaton : t -> string -> (Fclint.S.t * bool)
val match_prefix : t -> string -> Fclint.S.t

val split_positions : t -> t -> string -> Fclint.S.t

val unambig_split : t -> t -> string -> (string * string) option
val unambig_star_split : t -> string -> string list

type dual_single_split = { dss_example : string;
			   dss_cut1 : int;
			   dss_cut2 : int}

val example_of_dss : dual_single_split -> (string * string) * (string * string) 

type not_ambig = NA_true | NA_false of dual_single_split

type dual_multi_split = { dms_example : string;
			  dms_cut1 : int list;
			  dms_cut2 : int list}

val example_of_dms : dual_multi_split -> string

type not_star_ambig = NSA_true | NSA_empty_word | NSA_false | NSA_false_ce of dual_multi_split

val unambig_seq : t -> t -> not_ambig
val unambig_rep : t -> int -> int option -> not_star_ambig

val count_unambig_star_split : t -> string -> int
val print_multi_split : int list -> string -> unit 
val print_split : int -> string -> unit 

val print_stat_trim : unit -> unit
