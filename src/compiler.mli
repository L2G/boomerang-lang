(********************************************************************)
(* The Harmony Project                                              *)
(* harmony@lists.seas.upenn.edu                                     *)
(*                                                                  *)
(* compiler.mli - interface for Focal compiler                      *)
(********************************************************************)
(* $Id$ *)

(** The Focal compiler *)

val compile_file : string -> string -> unit
(** [compile_file filename module_name] compiles the module [module_name], which
    must be found in the Focal program [filename]. *)

val compile_string : string -> string -> unit
(** [compile_string fake_name str] compiles the [str] as being a Focal program,
    using [fake_name] as a fake file name to report errors. *)

val init : unit -> unit
(** [init ()] forces this module to be loaded, when used in a dynamically linked library *)
