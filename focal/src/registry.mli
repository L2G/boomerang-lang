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

val format_rv : rv -> unit
(** [format_rv r] pretty prints [r] *)

(** {2 Library} *)

module REnv : sig 
  type t
  val empty : unit -> t
  val lookup : t -> Syntax.qid -> rv option
  val update : t -> Syntax.qid -> rv -> t
  val overwrite : t -> Syntax.qid -> rv -> unit
  val iter : (Syntax.qid -> rv -> unit) -> t -> unit
end

val reset : unit -> unit
(** Resets the library. *)

val pre_ctx : Syntax.qid list
(** the initial naming context, i.e., [''Prelude''] *)

val get_library : unit -> REnv.t
(** Returns the library, as an environment. *)

val register_env : REnv.t -> Syntax.qid -> unit
(** ?? *)

val register_native : string -> Syntax.sort -> Value.t -> unit
(** ?? *)

val load : string -> bool
(** ?? *)

val find_filename : string -> string list -> string option
(** ?? *)

val lookup_library_ctx : Syntax.qid list -> Syntax.qid -> rv option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library : Syntax.qid -> rv option
(** [lookup_library q] looks up [q] from the library *)

(**/**)
val compile_file_impl : (string -> string -> unit) ref
val compile_fcl_str_impl : (string -> string -> unit) ref