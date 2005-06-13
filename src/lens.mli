(** Lenses (basic definitions and infrastructure) *)

(** Note that this module is just infrastructure.  Actual definitions of primitive 
  lenses can be found in [prelude.ml], etc... *) 

(* ------------------------------------------------------------------------- *)
(** {2 Basics} *)

(** The type of lenses *)
type ('a, 'b) t = { 
  get: 'a -> 'b;
  put: 'b -> 'a option -> 'a }
(** A lens comprises two functions, [get] and [put], and is
    parameterized by the types of the concrete and abstract domains. *)

val get : ('a, 'b) t -> 'a -> 'b
(** [get l c] returns the result of applying [l.get] to the tree [c]. *)

val put : ('a, 'b) t -> 'b -> 'a option -> 'a
(** [put l a c] returns the result of applying [l.put] to the abstract tree
    [a] and the concrete [c] *)

val native : ('a -> 'b) -> ('b -> 'a option -> 'a) -> ('a, 'b) t
(** Convert a pair of host-language functions (for which the
    programmer has manually checked the lens laws!) into a lens. *)

(* ------------------------------------------------------------------------- *)
(** {2 Memoization} *)

val memoize_lens : (V.t, V.t) t -> (V.t, V.t) t
(** [memoize_lens l] returns a memoized version of [l]. *)


(* ------------------------------------------------------------------------- *)
(** {2 Debugging support} *)

(** [tracepoint "foo" l] yields a lens that behaves just like [l] except that,
  during evaluation of its [get] and [put] functions, a [stackframe] is pushed onto
  a stack of current tracepoints.  This stack can be printed if an error occurs during
  evaluation of [l]. *)
val tracepoint : string -> (V.t, V.t) t -> (V.t, V.t) t

(** raise an error from a lens *)
val error : V.msg list -> 'a

val trap_errors_in : ('a -> 'b) -> 'a -> 'b
(** [trap_errors_in f] behaves like [f] except that [IllFormed] exceptions are
    trapped and printed *)


(* ------------------------------------------------------------------------- *)
(** {2 Visualizer support} *)

(** The lens visualizer needs lower-level access to the lens stack. *)

(** The type of stack frames. *)
type stackframe

(** Display a stackframe *)
val dumpframe: stackframe -> V.msg list

(** Build a lens that, when invoked, will call us back with some tracing informa *)
val probe2 :
     string
  -> (string -> V.t -> stackframe list -> unit)
  -> (string -> V.t -> V.t option -> stackframe list -> unit)
  -> (V.t, V.t) t

(* (\* ------------------------------------------------------------------------- *\) *)
(* (\** {2 Recursion support} *\) *)

(* val named : ?hashtable:((string, t) Hashtbl.t) -> string -> t *)
(* (\** Recursive lens definition  *\) *)

(* val fix : (string -> t) -> t *)
(* (\** recursive lens definition *\) *)
