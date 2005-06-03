(** The module for Lenses *)

(** Type of lenses *)
type ('a, 'b) t = { 
  get: 'a -> 'b;
  put: 'b -> 'a option -> 'a
}
(** A lens can be viewed as its two functions, get and put, and is
    parameterized ty the types of the concrete and abstract domains. *)

val get : ('a, 'b) t -> 'a -> 'b
(** [get l c] returns the result of applying [l.get] to the view [v]. *)

val put : ('a, 'b) t -> 'b -> 'a option -> 'a
(** [put l a c] returns the result of applying [l.put] to the abstract view
    [a] and the concrete [c] *)

val native : ('a -> 'b) -> ('b -> 'a option -> 'a) -> ('a, 'b) t
(** Convert a pair of host-language functions (for which the
    programmer has manually checked the lens laws!) into a lens *)

(* ------------------------------------------------------------------------- *)
(** {2 Debugging support} *)

type stackframe

(** to display a stackframe *)
val dumpframe: stackframe -> V.msg list

(** raise an error from a lens *)
val error : V.msg list -> 'a

val tracepoint : string -> (V.t, V.t) t -> (V.t, V.t) t

val trap_errors_in : ('a -> 'b) -> 'a -> 'b
(** [trap_errors_in f] behaves like [f] except that IllFormed exceptions are
    trapped and printed *)

val probe2 : string -> (string -> V.t -> stackframe list -> unit)
  -> (string -> V.t -> V.t option -> stackframe list -> unit) -> (V.t, V.t) t

val memoize_lens : (V.t, V.t) t -> (V.t, V.t) t
(** Speed improvement for applying a lens *)

(* (\* ------------------------------------------------------------------------- *\) *)
(* (\** {2 Recursion support} *\) *)

(* val named : ?hashtable:((string, t) Hashtbl.t) -> string -> t *)
(* (\** Recursive lens definition  *\) *)

(* val fix : (string -> t) -> t *)
(* (\** recursive lens definition *\) *)
