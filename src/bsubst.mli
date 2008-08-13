open Bsyntax
open Bident

val subst_sort : (Id.t * sort) list -> sort -> sort

val subst_exp_in_sort : (Qid.t * exp) list -> sort -> sort

val subst_exp : (Qid.t * exp) list -> exp -> exp

val free_sort_vars : sort -> Id.Set.t

val free_exp_vars_in_sort : sort -> Qid.Set.t

val free_locs_sort : sort -> int list
val free_locs_exp  : exp  -> int list

val erase_sort : sort -> sort 

val expose_sort : sort -> sort

val qualify_sort : (Qid.t -> bool) -> Qid.t list -> Id.t -> sort -> sort
(** [qualify_sort registered bound m s0] qualifies unbound, unqualified ids in s0
    with the module m.  For top-level calls, bound should be empty. *)

val syneq_exp : exp -> exp -> bool

val syneq_sort : sort -> sort -> bool

val gensym : Info.t -> exp -> Id.t

