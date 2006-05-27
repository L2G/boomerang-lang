(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(********************************************************************)
(* $Id$ *)

(** Run-time representations of Focal values *)

(** The type of run-time representations of values *)
type l_chk = Schema.t -> Schema.t
type lens_checker =     
    BIJ of l_chk * l_chk (* we need both directions for bijective lenses to check invert *)
  | VWB of l_chk
  | WB of l_chk

type t = 
    N of Name.t                           (* names *)
  | L of (V.t, V.t) Lens.t * lens_checker (* lenses *)
  | S of Schema.t               (* schemas *)
  | V of V.t                    (* trees *)
  | F of Syntax.sort * (t -> t) (* functions *)
  | D of Syntax.sort * Syntax.qid (* dummy *)

val parse_qid : string -> Syntax.qid
(** Returns a [Syntax.qid] qualified name from a string *)

val parse_sort : string -> Syntax.sort
(** Returns a [Syntax.sort] from a string *)

val focal_type_error : Info.t -> Syntax.sort -> t -> 'a
(** [focal_type_error i es v] raise an error at location [i], saying the expected sort [es] does not match the sort of the value [t] *)

val get_schema : Info.t -> t -> Schema.t
(** [get_schema i v] extracts the [s] from [v], converting it from [V.t] if needed. Prints an error message at [i] on failure. *)

val get_tree : Info.t -> t -> V.t
(** [get_tree i v] extracts the [V.t] from [v]. Prints an error message at [i] on failure. *)

val get_lens : Info.t -> t -> (V.t, V.t) Lens.t * lens_checker
(** [get_lens i v] extracts the [(V.t, V.t) Lens.t] from [v]. Prints an error message at [i] on failure. *)

val get_name : Info.t -> t -> Name.t
(** [get_name i v] extracts the [Name.t] from [v]. Prints an error message at [i] on failure. *)

val mk_sfun : string -> string -> (Schema.t -> t) -> t
(** [mk_sfun return_sort msg f] create a function from schemas to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_nfun : string -> string -> (Name.t -> t) -> t
(** [mk_nfun return_sort msg f] create a function from names to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_vfun : string -> string -> (V.t -> t) -> t
(** [mk_vfun return_sort msg f] create a function from trees to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_lfun : string -> string -> ((V.t,V.t) Lens.t * lens_checker -> t) -> t
(** [mk_lfun return_sort msg f] create a function from lenses to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_ffun : string -> string -> string -> ((t -> t) -> t) -> t
(** [mk_ffun arg_sort return_sort msg f] create a function from functions of sort [arg_sort] to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val format_t : t -> unit
(** [format_t v] pretty prints [v] *)

val dummy : Syntax.sort -> Syntax.qid -> t
(** [dummy s] returns a dummy value of sort [s]. For sorts, see [Syntax.sort].
  @param the first optional argument is used as an error message for a dummy lens;
  its default is "". *)

val is_dummy : t -> bool
(** [is_dummy v] returns true iff [v] is a dummy value *)

val memoize : t -> t
(** Nate, could you fill in that one please ? I'm not sure of what it does *)
