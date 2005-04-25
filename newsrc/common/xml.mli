(** A simple preproccessing function for stripping the DOCTYPE declaration.
   This is generally a good idea, and is default with gen_reader. *)
val strip_doctype : string -> string

(** Reader from a string *)
val reader : string -> V.t

(** Writer to a string *)
val writer : V.t -> string

val escapeXml : Name.t -> Name.t
(** used by xmlarchive *)
