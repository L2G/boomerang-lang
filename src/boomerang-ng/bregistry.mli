(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*********************************************************)
(* $Id$ *)

(** {2 Registry of Boomerang values } *)

type rv 
(** The type of registry values; just a sort and a value. *)

val make_rv : Bsyntax.sort -> Bvalue.t -> rv
(** [make_rv s v] returns a registry value of sort [s] and value [v]. *)

val value_of_rv : rv -> Bvalue.t
(** [value_of_rv r] returns the value from [r]. *)

val sort_of_rv : rv -> Bsyntax.sort
(** [sort_of_rv r] returns the sort from [r]. *)

val format_rv : rv -> unit
(** [format_rv r] pretty prints [r] *)

(** {2 Library} *)

module REnv : sig 
  type t
  val empty : unit -> t
  val lookup : t -> Bsyntax.qid -> rv option
  val update : t -> Bsyntax.qid -> rv -> t
  val overwrite : t -> Bsyntax.qid -> rv -> unit
  val iter : (Bsyntax.qid -> rv -> unit) -> t -> unit
end

val reset : unit -> unit
(** Resets the library. *)

val pre_ctx : Bsyntax.qid list
(** the initial naming context, i.e., [''Prelude''] *)

val get_library : unit -> REnv.t
(** Returns the library, as an environment. *)

val register_env : REnv.t -> Bsyntax.qid -> unit
(** ?? *)

val register_native : string -> Bsyntax.sort -> Bvalue.t -> unit
(** ?? *)

val load : string -> bool
(** ?? *)

val find_filename : string -> string list -> string option
(** ?? *)

val lookup_library_ctx : Bsyntax.qid list -> Bsyntax.qid -> rv option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library : Bsyntax.qid -> rv option
(** [lookup_library q] looks up [q] from the library *)

(**/**)
val compile_file_impl : (string -> string -> unit) ref
val compile_boom_str_impl : (string -> string -> unit) ref
