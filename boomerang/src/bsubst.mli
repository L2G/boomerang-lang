open Bsyntax

val subst_sort : (Id.t * sort) list -> sort -> sort

val subst_exp_in_sort : (Qid.t * exp) list -> sort -> sort

val free_sort_vars : sort -> Id.Set.t

val free_exp_vars_in_sort : sort -> Qid.Set.t

val erase_sort : sort -> sort 

