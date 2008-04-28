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
(* /boomerang/src/bprint.mli                                                   *)
(* Boomerang pretty printing interface                                         *)
(* $Id$ *)
(*******************************************************************************)
open Bsyntax

(** {2 Pretty Printing} *)

val reset_name_ctxt : unit -> unit
  (** Pretty printing is based on the approach used in
      OCaml. Internally, sort variables are represented by integer
      uids. When we pretty print a sort variable, we use a string like
      "'a" and "'b". Thus, all all of the pretty printing are relative
      to a global context mapping uids to names. [reset_name_ctxt ()]
      resets this context. The next svar will be printed as "'a". *)

val format_sort : sort -> unit 
  (** [format_sort s] pretty prints [s] using [Util.format]. *)

val format_svar : bool -> svar -> unit
(** [format_svar b x] pretty prints [x] using [Util.format]. If [b] is
    true, then constraints on [x] are printed. *)

val format_base_sort : base_sort -> unit
(** [format_base_sort s] pretty prints [s] using [Util.format]. *)

val format_scheme : scheme -> unit
(** [format_scheme s] pretty prints [s] using [Util.format]. *)

val format_pat : pat -> unit
(** [format_pat p] pretty prints [p] using [Util.format]. *)

val format_param : param -> unit
(** [format_param p] pretty prints [p] using [Util.format]. *)

val format_binding : binding -> unit
(** [format_binding b] pretty prints [b] using [Util.format]. *)

val format_exp : exp -> unit
(** [format_exp e] pretty prints [e] using [Util.format]. *)

val format_test_result : test_result -> unit
(** [format_test_result tr] pretty prints [tr] using [Util.format]. *)

val format_decl : decl -> unit
(** [format_decl d] pretty prints [d] using [Util.format]. *)

val format_module : modl -> unit
(** [format_module m] pretty prints [m] using [Util.format]. *)

val string_of_sort : sort -> string
(** [string_of_sort s] pretty prints [s] to a string. *)

val string_of_scheme : scheme -> string
(** [string_of_scheme s] pretty prints [s] to a string. *)

val string_of_pat : pat -> string
(** [string_of_pat p] pretty prints [p] to a string. *)

val string_of_param : param -> string
(** [string_of_param p] pretty prints [p] to a string. *)
