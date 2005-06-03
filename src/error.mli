(*****************************************************)
(* The Harmony Project                               *)
(* harmony@lists.seas.upenn.edu                      *)
(*                                                   *)
(* error.mli - interface for run-time exceptions     *)
(*****************************************************)
(* $Id$ *)

(** {2 Harmony Exceptions} **)

exception Compile_error of Info.t * string * string 
(** A [Compiler_error i file_name msg] is raised when an error occurs
at a location in a Focal program. Examples include lexing, parse, sort
checking, unit testing errors, and failed assertions. **)

exception Native_error of string
(** [Native_error msg] is raised by implementation so native lenses. **)

exception Fatal_error of string 
(** [Fatal_error msg] is raised when unexpected situations arise in
Focal programs. **)

val fail_on_error : (unit -> 'a) -> 'a
(** [fail_on_error f] runs [f ()] and handles errors by printing a
description of the error and exiting. **)
