(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.mli - interface for run-time representations               *)
(*                                                                  *)
(* $Id$ *)
(*                                                                  *)
(********************************************************************)

val compile_file_impl : (string -> string -> unit) ref

type n = string
type qn = n list * n

val qn_of_n : n -> qn
val n_of_id : Syntax.id -> n
val qn_of_qid : Syntax.qid -> qn
val qn_of_id : Syntax.id -> qn

val dot : qn -> qn -> qn 
val dot_id : qn -> Syntax.id -> qn

type t = 
    N of Name.t                 (* names *)
  | L of Lens.t                 (* lenses *)      
  | T of Type.t                 (* types *)
  | V of V.t                    (* views *)
  | F of (t -> t)               (* functions *)

type rtv = Syntax.sort * t

type env

val dummy : t
val dummy_rtv : rtv
  
val t_of_rtv : rtv -> t
val sort_of_rtv : rtv -> Syntax.sort 

val load : qn -> unit
    
val empty : env 
val overwrite : env -> qn -> rtv -> env
val overwrite_id : env -> Syntax.id -> rtv -> env
val update : env -> qn -> rtv -> env
val update_id : env -> Syntax.id -> rtv -> env
val lookup : env -> qn ->  rtv option
val lookup_qid : env -> Syntax.qid -> rtv option
val lookup_id : env -> Syntax.id -> rtv option

val lookup_in_ctx : env -> qn list -> qn -> rtv option

val register : qn -> Syntax.sort -> t -> unit 
val register_native : string -> string -> t -> unit 
val register_env : env -> qn -> unit
  
val fold : (qn -> rtv ref -> 'a -> 'a) -> env -> 'a -> 'a
val memoize : t -> t



val get_library : unit -> env 
