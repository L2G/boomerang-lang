(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)

type t 
val format_t : t -> unit

val treeschema : Treeschema.t -> t
val dbschema : Dbschema.t -> t
val treeschema_of : Info.t -> t -> Treeschema.t
val dbschema_of : Info.t -> t -> Dbschema.t

(* -------------- generic operations --------------- *)
val equivalent : t -> t -> bool
val subschema : t -> t -> bool
val member : V.t -> t -> bool

(* -------------- tree constructors --------------- *)
val mk_any   : t
val mk_atom  : Name.t -> t -> t
val mk_cat   : t list -> t
val mk_union : t list -> t
val mk_var   : string -> t
val mk_wild  : Name.Set.t -> int -> bool -> t -> t
val mk_neg   : t -> t
val mk_isect : t list -> t
val mk_diff  : t -> t -> t 
val mk_nil   : t
val mk_cons  : t -> t -> t

val t_of_tree : Tree.t -> t 

(* -------------- state ------------------- *)
(* AWB: Should these be pushed down to the just the Treeschema interface? *)

val mark_tvars : (string * Info.t) list -> unit
val finalize : unit -> unit
val update : string -> t -> unit


