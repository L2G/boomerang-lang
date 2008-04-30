open Bsyntax

(** {2 Sort Unification} *)

val fresh_svar : sbnd -> svar
(** [fresh_svar b] returns a new sort variable bound to [b]. *)

val fresh_sort : sbnd -> sort 
(** [fresh_svar b] returns a new sort containing a sort variable bound to [b]. *)

val svl_of_sl : Info.t -> sort list -> svar list
(** [svl_of_sl i sl] converts a list [sl] of sorts to a list of sort
    variables; [i] is used to report errors. *)

val svs_of_sl : Info.t -> sort list -> SVSet.t
(** [svs_of_sl i sl] converts a list [sl] of sorts to a set of sort
    variables; [i] is used to report errors. *)

val svs_of_svl : svar list -> SVSet.t
(** [svs_of_svl i sl] converts a list [svl] of sort variables to a set of sort
    variables; [i] is used to report errors. *)

val subst_sort : (Id.t * sort) list -> (sort -> Id.t -> bool) -> sort -> sort
(** [subst_sort al eq s] substitutes variables in [s0] according to [al] and [eq]. *)

val generalize : Info.t -> SVSet.t -> sort -> scheme
(** [generalize i env_svs s] constructs a new sort scheme from [s]
    where all the free type variables of [s] and not in [env_svs] are
    generalized. *)

val instantiate_cases : Info.t -> 
  Qid.t * (svar list * (Qid.t * sort option) list) -> 
  sort * (Qid.t * sort option) list
(** [instantiate_cases i (qx,(svl,cl))] constructs a new sort and a new list
    of constructor cases by picking fresh sort variables for each variable
    in [svl].*)

val instantiate : Info.t -> svar list * sort -> (sort list * sort)
(** [instantiate i (svs,s)] constructs a new sort by picking fresh
    sort variables for each variable in [svs].*)

val unify : Info.t -> sort -> sort -> bool
(** [unify i s1 s2] unifies [s1] and [s2]. *)
