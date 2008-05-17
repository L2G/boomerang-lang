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
(* $Id$ *)
(*******************************************************************************)

(** {2 Registry of Boomerang values } *)

type rs = 
  | Sort of Bsyntax.sort      
  | Unknown 
(** The type of registry sorts: either a sort, a scheme, or unknown *)

type rv = rs * Bvalue.t
(** The type of registry values: an [rs] and a value. *)

val value_of_rv : rv -> Bvalue.t
(** [value_of_rv r] returns the value from [r]. *)

val format_rv : rv -> unit
(** [format_rv r] pretty prints [r] *)

type tcon = Bsyntax.Qid.t * Bsyntax.sort option
type tspec = Bsyntax.Id.t list * tcon list 

(** {2 Library} *)
module REnv : sig 
  type t
  val empty : unit -> t
  val lookup : t -> Bsyntax.Qid.t -> rv option
  val lookup_type: t -> Bsyntax.Qid.t -> (Bsyntax.Qid.t * tspec) option
  val lookup_con : t -> Bsyntax.Qid.t -> (Bsyntax.Qid.t * tspec) option
  val update : t -> Bsyntax.Qid.t -> rv -> t
  val update_type : t -> Bsyntax.Id.t list -> Bsyntax.Qid.t -> tcon list -> t
  val overwrite : t -> Bsyntax.Qid.t -> rv -> unit
  val iter : (Bsyntax.Qid.t -> rv -> unit) -> t -> unit
  val iter_type : (Bsyntax.Qid.t -> tspec -> unit) -> t -> unit
  val fold : (Bsyntax.Qid.t -> rv -> 'a -> 'a) -> t -> 'a -> 'a
end

val reset : unit -> unit
(** Resets the library. *)

val pre_ctx : Bsyntax.Qid.t list
(** the initial naming context, i.e., ["Core" ; "Prelude" ] *)

val get_library : unit -> REnv.t
(** Returns the library, as an environment. *)

val register_env : REnv.t -> Bsyntax.Id.t -> unit
(** ?? *)

val register_native_qid : Bsyntax.Qid.t -> Bsyntax.sort -> Bvalue.t -> unit
(** ?? *)

val register_native : string -> Bsyntax.sort -> Bvalue.t -> unit
(** ?? *)

val load : string -> bool
(** ?? *)

val find_filename : string -> string list -> string option
(** ?? *)

val lookup_library_ctx : Bsyntax.Qid.t list -> Bsyntax.Qid.t -> rv option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library : Bsyntax.Qid.t -> rv option
(** [lookup_library q] looks up [q] from the library *)

val lookup_type_library_ctx : Bsyntax.Qid.t list -> Bsyntax.Qid.t -> (Bsyntax.Qid.t * tspec) option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_type_library : Bsyntax.Qid.t -> (Bsyntax.Qid.t * tspec) option
(** [lookup_library q] looks up [q] from the library *)

val lookup_con_library_ctx : Bsyntax.Qid.t list -> Bsyntax.Qid.t -> (Bsyntax.Qid.t * tspec) option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_con_library : Bsyntax.Qid.t -> (Bsyntax.Qid.t * tspec) option
(** [lookup_library q] looks up [q] from the library *)

(**/**)
val compile_file_impl : (string -> string -> unit) ref
val compile_boom_str_impl : (string -> string -> unit) ref
