(* $Id: lockfile.mli,v 1.1 2004/09/15 14:54:58 schmitta Exp $ *)

type lock

(** init should be called once at program initialization, before any*)
(** locks are acquired *)
val init : unit -> unit

(** if your program is going to be catching or ignoring any signals, *)
(** register them here. *)
val keep_locks_thru_signals : int list -> unit

(** attempts to acquire a lock on the given file name. Returns Some of a*)
(** lock if successful, None on failure. *)
val try_lock_file : string -> lock option

(** raised if must_lock_file times out without obtaining the lock *)
exception Timeout

(** must_lock_file fname period num_tries attempts to acquire a lock on*)
(** fname, sleeping for period seconds between attempts, for a maximum of*)
(** num_tries attempts. raises Timeout if it cannot succeed *)
val must_lock_file : string -> int -> int -> lock

(** unlock a previously-locked file. Subsequent unlocks of an *)
(** already-unlocked lock will have no effect. *)
val unlock_file : lock -> unit

