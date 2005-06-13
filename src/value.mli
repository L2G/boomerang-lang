(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(********************************************************************)
(* $Id$ *)

(** Run-time representations of Focal values *)

(** The type of run-time representations of values *)
type t = 
    N of Name.t                 (* names *)
  | L of (V.t, V.t) Lens.t      (* lenses *)      
  | T of ty                     (* types *)
  | V of V.t                    (* trees *)
  | F of Syntax.sort * (t -> t)               (* functions *)
(** The type of run-time representations of types *)
and ty = 
    Empty of Info.t
  | Any of Info.t
  | Var of Info.t * Syntax.qid * thunk
  | App of Info.t * t * t * thunk
  | Atom of Info.t * string * ty
  | Star of Info.t * (string list) * ty
  | Bang of Info.t * (string list) * ty
  | Cat of Info.t * ty list 
  | Union of Info.t * ty list 
and thunk = unit -> t

val info_of_ty : ty -> Info.t
(** [info_of_ty ty] returns the parsing information associated with [ty] *)

val parse_qid : string -> Syntax.qid
(** Returns a [Syntax.qid] qualified name from a string *)

val parse_sort : string -> Syntax.sort
(** Returns a [Syntax.sort] from a string *)

val focal_type_error : Info.t -> Syntax.sort -> t -> 'a
(** [focal_type_error i es v] raise an error at location [i], saying the expected sort [es] does not match the sort of the value [t] *)

val get_type : Info.t -> t -> ty
(** [get_type i v] extracts the [ty] from [v], converting it from [V.t] if needed. Prints an error message at [i] on failure. *)

val get_tree : Info.t -> t -> V.t
(** [get_tree i v] extracts the [V.t] from [v]. Prints an error message at [i] on failure. *)

val get_lens : Info.t -> t -> (V.t, V.t) Lens.t
(** [get_lens i v] extracts the [(V.t, V.t) Lens.t] from [v]. Prints an error message at [i] on failure. *)

val get_name : Info.t -> t -> Name.t
(** [get_name i v] extracts the [Name.t] from [v]. Prints an error message at [i] on failure. *)

val mk_tfun : string -> string -> (ty -> t) -> t
(** [mk_tfun return_sort msg f] create a function from types to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_nfun : string -> string -> (Name.t -> t) -> t
(** [mk_nfun return_sort msg f] create a function from names to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_vfun : string -> string -> (V.t -> t) -> t
(** [mk_vfun return_sort msg f] create a function from trees to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_lfun : string -> string -> ((V.t,V.t) Lens.t -> t) -> t
(** [mk_lfun return_sort msg f] create a function from lenses to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val mk_ffun : string -> string -> string -> ((t -> t) -> t) -> t
(** [mk_ffun arg_sort return_sort msg f] create a function from functions of sort [arg_sort] to [return_sort] using [f], displaying [msg] if runtime failure occurs *)

val string_of_t : t -> string
(** [string_of_t v] returns a formatted string representing [v] *)

val string_of_ty : ty -> string
(** [string_of_t v] returns a formatted string representing [v] *)

val dummy : ?msg:string -> Syntax.sort -> t
(** [dummy s] returns a dummy value of sort [s]. For sorts, see [Syntax.sort].
  @param the first optional argument is used as an error message for a dummy lens;
  its default is "". *)

val memoize : t -> t
(** Nate, could you fill in that one please ? I'm not sure of what it does *)
