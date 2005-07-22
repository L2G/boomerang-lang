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

val init : unit -> unit
(** [init ()] forces this module to be loaded, when used in a dynamically linked library *)
