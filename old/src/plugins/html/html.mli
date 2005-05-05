(** gen_reader takes an optional preprocess function and tidy flag, and
    produces a reader.
    Defaults to the identity preprocessor, and no tidy. *)
val gen_reader : ?preproc: (string -> string) -> ?tidy: bool -> unit -> V.reader

(** gen_writer takes an optional postprocess function and produces a writer.
    Defaults to the identity postprocessor. *)
val gen_writer : ?postproc: (string -> string) -> unit -> V.writer

(** Reader from a string *)
val from_string : string -> V.t option

(** Writer to a string *)
val to_string : ?postproc:(string -> string) ->  V.t -> string

(** reader with defaults *)
val simple_reader : V.reader

(** writer with defaults *)
val simple_writer : V.writer
