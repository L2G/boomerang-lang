(** a chunked input.  note that Lex.parse_chunk : Lex.chunk -> ... ->
    Oracle.chunk, effectively
*)
type chunk = Lex.token list

(** a list of chunks: a sample set *)
type chunks = chunk list

(**
   possible prophecies for a set of chunks

   a BaseProphecy indicates that all of the chunks can be described by
   a single token (in particular, a RegexToken -- MetaTokens are
   broken into structures)

   an EmptyProphecy is issued when all of the given chunks are empty

   a StructProphecy indicates that all of the chunks have a similar
   structure, broken down into a list of fields.  for each field, the
   StructProphecy contains that field from each chunk

   a ListProphecy indicates that all of the chunks are lists, with a
   possible preamble and postamble

   a UnionProphecy indicates that each chunk matches one of several
   possible variants.  for each variant, the UnionProphecy contains
   the chunks that match that variant
*)
type prophecy = 
    BaseProphecy of Lex.token
      (*              fields *)
  | EmptyProphecy
  | StructProphecy of chunks list
      (*          preamble * entries * postamble *)
  | ListProphecy of chunks * chunks * chunks
      (*             variants *)
  | UnionProphecy of chunks list

val string_of_prophecy : prophecy -> string

(** the oracle analyzes the input sample set and produces a prophecy,
    analyzing the structure of the data

    hopefully nobody minds that the function is called prophesy and
    the type is called prophecy.  if it does bother you, notice that
    prophesy is a verb but prophecy is a noun, so, "Ezra prophesied
    prophecy."
*)
val prophesy : chunks -> prophecy
