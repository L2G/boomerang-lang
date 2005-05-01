(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* registry.mli - interface for library of Focal values             *)
(*                                                                  *)
(* $Id: value.ml,v 1.5 2005/04/21 03:27:42 jnfoster Exp $ *)
(*                                                                  *)
(********************************************************************)

(* registry values *)
type rv 
val make_rv : Syntax.sort -> Value.t -> rv
val value_of_rv : rv -> Value.t
val sort_of_rv : rv -> Syntax.sort

(* environments *)
type env 
val empty : env 
val update : 'a -> ('a -> env) -> ('a -> env -> 'a) -> Syntax.qid -> rv -> 'a
val overwrite : 'a -> ('a -> env) -> ('a -> env -> 'a) -> Syntax.qid -> rv -> 'a
val lookup : 'a -> ('a -> env) -> Syntax.qid -> rv option
val string_of_rv : rv -> string
val string_of_env : env -> string

(* library *)
val parse_qid : string -> Syntax.qid 
val pre_ctx : Syntax.qid list
val get_library : unit -> env 
val register_env : env -> Syntax.qid -> unit
val register_native : string -> string -> Value.t -> unit
val lookup_library : 'a -> ('a -> env) -> ('a -> Syntax.qid list) -> Syntax.qid -> rv option
val lookup_lens : Syntax.qid -> Lens.t option
  
(* backpatch hack *)
val compile_file_impl : (string -> Syntax.id -> unit) ref
