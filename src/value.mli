(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(********************************************************************)
(* $Id$ *)

(** Run-time representations of Focal values *)

(** The type of run-time representations of values *)
(*type l_chk = Schema.t -> Schema.t*)
type ('a, 'b) lens_checker =     
  | BIJ of ('a -> 'b) * ('b -> 'a) (* we need both directions for bijective lenses to check invert *)
  | VWB of ('a -> 'b)
  | WB of ('a -> 'b)

type t = 
  | N of Name.t                                                (* names *)
  | L of (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker (* lenses *)
  | S of Schema.t                                              (* schemas *)
  | P of Db.Relation.Pred.t                                    (* relational predicates *)
  | FD of Dbschema.Relschema.Fd.Set.t                          (* functional dependencies *)
  | V of V.t                                                   (* trees *)
  | M of ((V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker) Name.Map.t (* fmaps *)
  | F of Syntax.sort * (t -> t)                                (* functions *)
  | D of Syntax.sort * Syntax.qid                              (* dummies *)

val equal : t -> t -> bool
(** Checks whether two values are equal.
    @raise Error.Harmony_error on lenses, functions, and dummies. *)

val parse_qid : string -> Syntax.qid
(** Returns a [Syntax.qid] qualified name from a string *)

val parse_sort : string -> Syntax.sort
(** Returns a [Syntax.sort] from a string *)

val focal_type_error : Info.t -> Syntax.sort -> t -> 'a
(** [focal_type_error i es v] raise an error at location [i], saying the expected sort [es] does not match the sort of the value [t] *)

val get_schema : Info.t -> t -> Schema.t
(** [get_schema i v] extracts the [s] from [v], converting it from [V.t] if needed. Prints an error message at [i] on failure. *)

val get_view : Info.t -> t -> V.t
(** [get_view i v] extracts the [V.t] from [v]. Prints an error message at [i] on failure. *)

val get_tree : Info.t -> t -> Tree.t
(** [get_tree i v] extracts the [Tree.t] from [v]. Prints an error message at [i] on failure. *)

val get_lens : Info.t -> t -> (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker
(** [get_lens i v] extracts the [(V.t, V.t) Lens.t] from [v]. Prints an error message at [i] on failure. *)

val get_name : Info.t -> t -> Name.t
(** [get_name i v] extracts the [Name.t] from [v]. Prints an error message at [i] on failure. *)

val get_pred : Info.t -> t -> Db.Relation.Pred.t
(** [get_pred i v] extracts the [Relation.Pred.t] from [v]. Prints an error
message at [i] on failure. *)

val get_fds : Info.t -> t -> Dbschema.Relschema.Fd.Set.t
(** [get_fds i v] extracts the [Dbschema.Relschema.Fd.Set.t] from [v]. Prints
an error message at [i] on failure. *)

val get_fmap : Info.t -> t -> ((V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker) Name.Map.t
(** [get_fmap i v] extracts the finite map from [v]. Prints an error message at [i] on failure. *)

val mk_sfun : Syntax.sort -> string -> (Schema.t -> t) -> t
(** [mk_sfun return_sort msg f] create a function from schemas to
  [return_sort] using [f], displaying [msg] if runtime failure occurs.
  (The  [return_sort] is represented as a string rather than a Syntax.sort
  for readability at call sites.) *)

val mk_nfun : Syntax.sort -> string -> (Name.t -> t) -> t
(** [mk_nfun return_sort msg f] creates a function from names to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_vfun : Syntax.sort -> string -> (V.t -> t) -> t
(** [mk_vfun return_sort msg f] creates a function from views to
    [return_sort] using [f], displaying [msg] if runtime failure
    occurs. *)

val mk_lfun : Syntax.sort -> string -> ((V.t,V.t) Lens.t * (Schema.t, Schema.t) lens_checker -> t) -> t
(** [mk_lfun return_sort msg f] creates a function from lenses to
    [return_sort] using [f], displaying [msg] if runtime failure
    occurs. *)

val mk_pfun : Syntax.sort -> string -> (Db.Relation.Pred.t -> t) -> t
(** [mk_pfun return_sort msg f] creates a function from predicates to
[return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_fdfun : Syntax.sort -> string -> (Dbschema.Relschema.Fd.Set.t -> t) -> t
(** [mk_fdfun return_sort msg f] creates a function from sets of functional
dependencies to [return_sort] using [f], displaying [msg] if runtime failure
occurs *)

val mk_fmfun : Syntax.sort -> string -> (((V.t,V.t) Lens.t * (Schema.t, Schema.t) lens_checker) Name.Map.t -> t) -> t
(** [mk_mfun return_sort msg f] creates a function from maps to
    [return_sort] using [f], displaying [msg] if runtime failure
    occurs. *)

val mk_ffun : Syntax.sort -> Syntax.sort -> string -> ((t -> t) -> t) -> t
(** [mk_ffun arg_sort return_sort msg f] creates a function from functions
    of sort [arg_sort] to [return_sort] using [f], displaying [msg]
    if a runtime failure occurs. *)

val v_of_tree_lens :
  (Tree.t, Tree.t) Lens.t * (Treeschema.t, Treeschema.t) lens_checker ->
    (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker

val v_of_db_lens :
  (Db.t, Db.t) Lens.t * (Dbschema.t, Dbschema.t) lens_checker ->
    (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker

val tree_of_v_lens :
  (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker ->
    (Tree.t, Tree.t) Lens.t * (Treeschema.t, Treeschema.t) lens_checker

val db_of_v_lens :
  (V.t, V.t) Lens.t * (Schema.t, Schema.t) lens_checker ->
    (Db.t, Db.t) Lens.t * (Dbschema.t, Dbschema.t) lens_checker

val format_t : t -> unit
(** [format_t v] pretty prints [v] *)

val dummy : Syntax.sort -> Syntax.qid -> t
(** [dummy s] returns a dummy value of sort [s]. For sorts, see [Syntax.sort].
    @param the first optional argument is used as an error message
    for a dummy lens; the default is "". *)

val is_dummy : t -> bool
(** [is_dummy v] returns true iff [v] is a dummy value *)

val memoize : t -> t
(** Nate, could you fill in that one please ? I'm not sure of what it does *)
