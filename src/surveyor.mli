(** The surveyor handles low-level registration of plugins. *)

type type_desc = string list
type encoding_key = string

type encoding_test = string -> string option -> bool
(** [encoding_test filename contents] is true if the supplied information
    indicates a file of some encoding. *)

type encoding = {
  description: string;          (** "long" description *)
  encoding_test: encoding_test; (** id function *)
  reader: string -> V.t;        (** reads data in the given encoding *)
  writer: V.t -> string;        (** writes data to the given encoding *)
}

val register_encoding : encoding_key -> encoding -> unit
(** [register_encoding key enc] registers [enc] associated with [key] with
    the surveyor. *)

val find_encodings : string -> string option -> encoding_key list
(** [find_encodings filename contents] finds the set of encodings which
    identify with the supplied information. *)

val get_all_encodings : unit -> encoding_key list
(** [get_all_encodings ()] returns every encoding that is registered with
    the surveyor. *)

val get_encoding : encoding_key -> encoding
(** [get_encoding k] gets the encoding associated with [k]. *)

val get_reader : encoding_key -> string -> V.t
(** [get_reader k] gets the reader associated with [k]. *)

val get_writer : encoding_key -> V.t -> string
(** [get_writer k] gets the writer associated with [k]. *)

val get_description : encoding_key -> string
(** [get_description k] gets the description associated with [k]. *)

val print_description : encoding_key -> unit
(** [print_description k] prints a description associated with [k]. *)

