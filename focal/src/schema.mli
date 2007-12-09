(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id: treeschema.mli 1756 2006-05-31 15:08:03Z bohannon $ *)

type t =
    T of Treeschema.t
  | D of Dbschema.t

val format_t : t -> unit

val treeschema : Treeschema.t -> t
val dbschema : Dbschema.t -> t
val treeschema_of : Info.t -> t -> Treeschema.t
val dbschema_of : Info.t -> t -> Dbschema.t
val is_treeschema : t -> bool
val is_dbschema : t -> bool

(* -------------- generic operations --------------- *)
val equivalent : t -> t -> bool
val subschema : t -> t -> bool
val member : V.t -> t -> bool

