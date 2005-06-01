(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* registry.mli - interface for library of Focal values             *)
(*                                                                  *)
(********************************************************************)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)

(** Module for library of Focal values *)

(** {2 Registry values} *)

type rv 
(** The type for registry values. A registry value can be thought of as a sort and a value. *)

val make_rv : Syntax.sort -> Value.t -> rv
(** [make_rv s v] returns a registry value of sort [s] and value [v]. *)

val value_of_rv : rv -> Value.t
(** [value_of_rv rv] returns the value of the registry value [rv]. *)

val sort_of_rv : rv -> Syntax.sort
(** [sort_of_rv rv] returns the sort of the registry value [rv]. *)

(** {2 Environments} *)
type env
(** The type for environments *)

val empty : unit -> env 
(** A function returning an empty environment. *)

val update : 'a -> ('a -> env) -> ('a -> env -> 'a) -> Syntax.qid -> rv -> 'a
(** [update oev get_ev put_ev q r] updates the environment passed through [oev] with a binding
    between fully qualified name [qid] and registry value [rv]. [oev] is not an actual
    environment but can be any value : [get_ev] is used to retrieve an environment of type [env]
    from [oev], and conversely, [put_ev] is used after updating the environment. *)

val overwrite : 'a -> ('a -> env) -> ('a -> env -> 'a) -> Syntax.qid -> rv -> 'a
(** Same as update, but if a binding already exists, the value is updated without creating a new
    reference, as [update] would. (Do we need that ??? the result is the same in both cases...) *)

val lookup : 'a -> ('a -> env) -> Syntax.qid -> rv option
(** [lookup ev get_ev qid] returns [Some rv] where [rv] is the registry value bound to [qid] in
    [get_ev(ev)], [None] if [qid] is unbound in the environment. *)
    
val string_of_rv : rv -> string
(** Returns a formatted string representing the registry value passed as argument. *)

val string_of_env : env -> string
(** Returns a string representing the environment passed as argument. *)

(** {2 Library} *)

val parse_qid : string -> Syntax.qid
(** Returns a [Syntax.qid] qualified name from a string *)

val pre_ctx : Syntax.qid list

val reset : unit -> unit
(** Resets the library. *)

val get_library : unit -> env
(** Returns the library, as an environment. *)

val register_env : env -> Syntax.qid -> unit
(** ?? *)

val register_native : string -> string -> Value.t -> unit
(** ?? *)

val lookup_oev : 'a -> ('a -> env) -> ('a -> Syntax.qid list) -> Syntax.qid -> rv option
(** ?? *)

val lookup_library : Syntax.qid -> rv option
(** [lookup_library qid = lookup () get_library] *)
  
(**/**)
(* backpatch hack *)
val compile_file_impl : (string -> Syntax.id -> unit) ref
