(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/registry.mli                                                 *)
(* Boomerang run-time registry interface                                       *)
(* $Id$                                                                        *)
(*******************************************************************************)

(** {2 Registry of Boomerang values } *)

type rv 
(** The type of registry values; just a scheme and a value. *)

val make_rv : Bsyntax.scheme -> Bvalue.t -> rv
(** [make_rv s v] returns a registry value of scheme [s] and value [v]. *)

val value_of_rv : rv -> Bvalue.t
(** [value_of_rv r] returns the value from [r]. *)

val scheme_of_rv : rv -> Bsyntax.scheme
(** [scheme_of_rv r] returns the scheme from [r]. *)

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
  val fold : (Bsyntax.qid -> rv -> 'a -> 'a) -> t -> 'a -> 'a
end

val reset : unit -> unit
(** Resets the library. *)

val pre_ctx : Bsyntax.id list
(** the initial naming context, i.e., [''Prelude''] *)

val get_library : unit -> REnv.t
(** Returns the library, as an environment. *)

val register_env : REnv.t -> Bsyntax.id -> unit
(** ?? *)

val register_native_qid: Bsyntax.qid -> Bsyntax.scheme -> Bvalue.t -> unit
(** ?? *)

val register_native : string -> Bsyntax.scheme -> Bvalue.t -> unit
(** ?? *)

val load : string -> bool
(** ?? *)

val find_filename : string -> string list -> string option
(** ?? *)

val lookup_library_ctx : Bsyntax.id list -> Bsyntax.qid -> rv option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library : Bsyntax.qid -> rv option
(** [lookup_library q] looks up [q] from the library *)

(**/**)
val compile_file_impl : (string -> string -> unit) ref
val compile_boom_str_impl : (string -> string -> unit) ref
