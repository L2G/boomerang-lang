(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* value.mli - interface for run-time representations               *)
(*                                                                  *)
(* $Id$ *)
(*                                                                  *)
(********************************************************************)

type n = string
type qn = n list * n

val qn_of_n : n -> qn
val n_of_id : Syntax.id -> n
val qn_of_qid : Syntax.qid -> qn
val qn_of_id : Syntax.id -> qn

val dot : qn option -> qn -> qn 
val dot_id : qn option -> Syntax.id -> qn

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
    
val empty : env 
val overwrite : env -> qn -> rtv -> env
val overwrite_id : env -> Syntax.id -> rtv -> env
val update : env -> qn -> rtv -> env
val update_id : env -> Syntax.id -> rtv -> env
val lookup : env -> qn -> rtv option
val lookup_qid : env -> Syntax.qid -> rtv option
val lookup_id : env -> Syntax.id -> rtv option
  
val fold : (qn -> rtv ref -> 'a -> 'a) -> env -> 'a -> 'a
val memoize : t -> t

val get_library : unit -> env 
val pre_register_native : string -> t -> string -> unit
