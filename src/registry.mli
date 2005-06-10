(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id$ *)

(** {2 Registry of Focal values } *)

type rv 
(** The type of registry values; just a sort and a value. *)

val make_rv : Syntax.sort -> Value.t -> rv
(** [make_rv s v] returns a registry value of sort [s] and value [v]. *)

val value_of_rv : rv -> Value.t
(** [value_of_rv r] returns the value from [r]. *)

val sort_of_rv : rv -> Syntax.sort
(** [sort_of_rv r] returns the sort from [r]. *)
    
val string_of_rv : rv -> string
(** [string_of_rv r] Returns a string representing [r] *)

(** {2 Library} *)

val pre_ctx : Syntax.qid list

val reset : unit -> unit
(** Resets the library. *)

val get_library : unit -> rv Env.t
(** Returns the library, as an environment. *)

val register_env : rv Env.t -> Syntax.qid -> unit
(** ?? *)

val register_native : string -> string -> Value.t -> unit
(** ?? *)

val load : string -> bool
(** ?? *)

val find_filename : string -> string option
(** ?? *)

val lookup_library_ctx : Syntax.qid list -> Syntax.qid -> rv option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library : Syntax.qid -> rv option
(** [lookup_library q] looks up [q] from the library *)

(**/**)
val compile_file_impl : (string -> string -> unit) ref
