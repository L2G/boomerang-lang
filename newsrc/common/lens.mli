(** Type of lenses *)
type t = { 
  get: V.t -> V.t;
  put: V.t -> V.t option -> V.t
}

(* extract components of a lens *)
val get : t -> V.t -> V.t
val put : t -> V.t -> V.t option -> V.t

val native : (V.t -> V.t) -> (V.t -> V.t option -> V.t) -> t
(** Convert a pair of host-language functions (for which the
    programmer has manually checked the lens laws!) into a lens *)

(* ------------------------------------------------------------------------- *)
(** {2 Debugging support} *)

type stackframe

(** to display a stackframe *)
val dumpframe: stackframe -> V.msg list

(** raise an error from a lens *)
val error : V.msg list -> 'a

val probe : string -> t
(** [probe s] behaves the same as [id], but dumps out the views it sees to the
    debugging stream, along with the identifying tag [s]. *)

val tracepoint : string -> t -> t

val trap_errors_in : ('a -> 'b) -> 'a -> 'b
(** [trap_errors_in f] behaves like [f] except that IllFormed exceptions are
    trapped and printed *)

val probe2 : string -> (string -> V.t -> stackframe list -> unit)
  -> (string -> V.t -> V.t option -> stackframe list -> unit) -> t

(* (\* ------------------------------------------------------------------------- *\) *)
(* (\** {2 Recursion support} *\) *)

(* val memoize : t -> t *)
(* (\** Speed improvement of a lens *\)   *)

(* val named : ?hashtable:((string, t) Hashtbl.t) -> string -> t *)
(* (\** Recursive lens definition  *\) *)

(* val fix : (string -> t) -> t *)
(* (\** recursive lens definition *\) *)
