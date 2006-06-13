(** Low-level registration of lenses, viewers, etc. *)

type encoding_key = string

type encoding_test = string -> string option -> bool
(** [encoding_test filename contents] is true if the supplied information
    indicates a file of some encoding. *)

type content_desc =
    FromString of string
  | FromFile of string

type encoding = {
  description: string;          (** "long" description *)
  encoding_test: encoding_test; (** id function *)
  reader: content_desc -> V.t;  (** reads data in the given encoding *)
  writer: V.t -> string -> unit;(** writes data to the specified file *)
}

val simple_reader : (string -> Tree.t) -> (content_desc -> V.t)
(** Convert a string-to-tree function to a reader. *)

val simple_writer : (Tree.t -> string) -> V.t -> string -> unit
(** Convert a tree-to-string function to a writer. *)

val register_encoding : encoding_key -> encoding -> unit
(** [register_encoding key enc] registers [enc] associated with [key] with
    the surveyor. *)

(* 
val find_encodings : string -> string option -> encoding_key list
(** [find_encodings filename contents] finds the set of encodings which
    identify with the supplied information. *)

val get_all_encodings : unit -> encoding_key list
(** [get_all_encodings ()] returns every encoding that is registered with
    the surveyor. *)

val get_encoding : encoding_key -> encoding
(** [get_encoding k] gets the encoding associated with [k]. *)
*)

val get_reader : encoding_key -> content_desc -> V.t
(** [get_reader k] gets the reader associated with [k]. *)

val get_writer : encoding_key -> V.t -> string -> unit
(** [get_writer k] gets the writer associated with [k]. *)

val get_description : encoding_key -> string
(** [get_description k] gets the description associated with [k]. *)

val print_description : encoding_key -> unit
(** [print_description k] prints a description associated with [k]. *)

val parse_filename : string -> string * (string option)
(** [parse_filename fn] splits a filename into the real filename and an optional annotation to select the viewer *)

val get_ekey : string option -> string -> string option -> encoding_key
(** [get_ekey eko fn contents_opt] returns eko if it is registered or else looks up the encoding for fn and contents *)

val tree_of_file : string -> (content_desc -> V.t) -> V.t option
(** Given a filename [fn] and a reader [r], [tree_of_file fn r] reads the contents of the file, pass it to the reader, and returns the result. If an error was encountered, [None] is returned. *)

