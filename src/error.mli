(*****************************************************)
(* The Harmony Project                               *)
(* harmony@lists.seas.upenn.edu                      *)
(*                                                   *)
(* error.mli - interface for run-time exceptions     *)
(*****************************************************)
(* $Id$ *)

(** Exceptions used throughout Harmony *)

exception Compile_error of Info.t * string * string 
(** A [Compiler_error i file_name msg] is raised when an error occurs
  at a location in a Focal program. Examples include lexing, parse, sort
  checking, unit testing errors, and failed assertions. *)

exception Native_error of string
  (** [Run_error msg] is raised by ML functions in the back end. *)

exception Fatal_error of string 
(** [Fatal_error msg] is raised when unexpected situations arise in
    Focal programs. *)

val string_of_file_info : string -> Info.t -> string
  (** [string_of_file_info fn i] returns a string describing location
      [i] in file [fn]. *)

val fail_on_error : (unit -> 'a) -> 'a
  (** [fail_on_error f] runs [f ()] and handles errors by printing a
      description of the error and exiting. *)
