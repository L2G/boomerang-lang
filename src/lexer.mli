(*************************************************)
(* The Harmony Project                           *)
(* harmony@lists.seas.upenn.edu                  *)
(*                                               *)
(* lexer.mli - interface for Focal lexer         *)
(*************************************************)
(* $Id $ *)

(** The Focal lexer *)

val setup : string -> unit
(** [setup fn] resets the lexer state, using [fn] as the file name when reporing errors *)

val finish : unit -> unit
(** [finish] restores the lexer to its previous state, before the last [setup] was called *)

val filename : unit -> string
(** [filename ()] returns the name of the file being lexed *)

val info : Lexing.lexbuf -> Info.t
(** [info lexbuf] extracts an [Info.t] of [lexbuf] *)

val main : Lexing.lexbuf -> Parser.token
(** [main lexbuf] lexes [Parser.token]s from [lexbuf] *)
