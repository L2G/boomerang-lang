val print_stat_trim : unit -> unit

type t

val format: t -> unit

(* constructors *)
val mk_str : bool -> Rstring.t -> t
val mk_cset : bool -> (Rstring.sym * Rstring.sym) list -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_complement : t -> t
val mk_diff : t -> t -> t
val mk_inter : t -> t -> t
val mk_reverse : t -> t
val mk_lowercase : t -> t
val mk_uppercase : t -> t

(* operations *)
val representative : t -> Rstring.t (* raises Not_found *)
val is_empty : t -> bool

val trim : t -> t
val determinize : t -> t

type dual_single_split = { dss_example : Rstring.t;
			   dss_cut1 : int;
			   dss_cut2 : int}

val example_of_dss : dual_single_split -> (Rstring.t * Rstring.t) * (Rstring.t * Rstring.t)

type not_ambig = NA_true of t | NA_false of dual_single_split

type dual_multi_split = { dms_example : Rstring.t;
			  dms_cut1 : int list;
			  dms_cut2 : int list}

val example_of_dms : dual_multi_split -> Rstring.t

type not_star_ambig = NSA_true of t | NSA_empty_word | NSA_false | NSA_false_ce of dual_multi_split


val unambig_seq : t -> t -> not_ambig
val unambig_star : t -> not_star_ambig

val match_str : t -> Rstring.t -> bool
val match_prefix : t -> Rstring.t -> Rint.Set.t
val find_exit_automaton : t -> Rstring.t -> (Rint.Set.t * bool)
val equiv : t -> t -> bool

val split_positions: t -> t -> Rstring.t -> Rint.Set.t
val unambig_split : t -> t -> Rstring.t -> (Rstring.t * Rstring.t) option
val unambig_star_split : t -> Rstring.t -> Rstring.t list
val count_unambig_star_split : t -> Rstring.t -> int
val print_multi_split : int list -> Rstring.t -> unit 
val print_split : int -> Rstring.t -> unit 

val epsilon : t
val empty : t

val easily_splitable : t -> t -> bool

val easy_seq : t -> t -> t option

val easy_star : t-> t option

val easy_split : t -> Wic.t -> (Rstring.t option * Wic.t)

