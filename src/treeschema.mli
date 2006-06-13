(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(*********************************************************)
(* $Id$ *)

type t 
val format_t : t -> unit

(* -------------- state ------------------- *)

val mark_tvars : (string * Info.t) list -> unit
val finalize : unit -> unit
val update : string -> t -> unit

(* -------------- constructors --------------- *)
val mk_any   : t
val mk_empty : t
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

(* -------------- operations --------------- *)
val empty : t -> bool
val equivalent : t -> t -> bool
val subschema : t -> t -> bool
val member : Tree.t -> t -> bool
val dom_member : Name.Set.t -> t -> bool
val project : Name.t -> t -> t option
val project_all : t -> t option
val inject : t -> t -> t
val inject_map : t -> t Name.Map.t -> t
val restrict : Name.Set.t -> t -> t*t
