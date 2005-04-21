(** html.ml uses this to output xml.. *)
val to_string : V.t -> string

(** A simple preproccessing function for stripping the DOCTYPE declaration.
   This is generally a good idea, and is default with gen_reader. *)
val strip_doctype : string -> string

(** gen_reader takes an optional preprocess function and tidy flag, and
   produces a reader.
   Defaults to strip_doctype, and no tidy. *)
val gen_reader : ?preproc: (string -> string) -> ?tidy: bool -> unit -> V.reader

(** gen_writer takes an optional postprocess function and produces a writer.
   Defaults to the identity postprocessor. *)
val gen_writer : ?postproc: (string -> string) -> unit -> V.writer

(** reader with defaults (strip the doctype, don't use tidy) *)
val simple_reader : V.reader

(** writer with defaults (no postprocessing) *)
val simple_writer : V.writer

(** Reader from a string *)
val from_string : string -> V.t option

(** Writer to a string *)
val to_string : V.t -> string

val escapeXml : Name.t -> Name.t
(** used by xmlarchive *)
