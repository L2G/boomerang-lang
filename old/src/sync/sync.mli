(** Core synchronization logic *)

(* $Id: sync.mli,v 1.5 2005/02/27 04:53:15 jnfoster Exp $ *)

type action
(** the type of worklists, indicating transfer instructions and conflicts *)

val format : action -> unit
(** print out a worklist *)

val format_without_equal: action -> unit
(** print out a worklist, omitting "MarkEqual" except at the very top*)

val get_action_name : action -> string

val conflict_free : action -> bool
(** [conflict_free a] is true if [a] does not signal any conflicts *)

val schema_conflict : action -> bool
(** [schema_conflict a] is true if [a] is a SchemaConflict *)

val find_conflict : action -> action option

val sync : (Syntax.ctx * Syntax.typ) -> V.t option -> V.t option -> V.t option ->
              (action * V.t option * V.t option * V.t option)
(** [sync o a b] synchronizes archive o with replicas a and b, and returns 
   a worklist and the resulting new archive and replicas respectively *)

val propagate : V.t option -> V.t option -> V.t option -> action ->
                  (V.t option * V.t option * V.t option)
(** [propagate o a b action] generates a new archive and replicas based on
   the instructions detailed by action *)
