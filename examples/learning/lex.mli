type 'a table_entry = string * 'a
(** ordered tables, implemented as assoc lists *)
type 'a table = 'a table_entry list
(** tables of (uncompiled) regular expression strings *)
type regex_table = string table
(** tables of pairs of delimiter strings, such as "(" and ")" *)
type delimiter_table = (string * string) table

val regex_table : (string * string) list
val delimiter_table : (string * (string * string)) list

(* given a regex name, looks it up and returns the length of the regex *)
val regex_length : string -> int

(** the tokens matched in tokenization 

    these tokens are like a lexing phase -- we identify potential
    delimiters, but the parsing phase and the token type resolve actual
    nesting
*)
type match_token

(** the tokens produced by parsing

    the metatokens exhibit the tree structure implied by the
    match_tokens

    in RegexToken, the first string is the raw data, the second string
    is the regex name 

    in MetaToken, the first string is the left delimiter, followed by
    a list of sub tokens, the right delimiter, and the delimiter name
*)
type token = 
    RegexToken of string * string 
  | MetaToken of string * token list * string * string

(** equality over token types -- it ignores the first part of each
    token type, comparing only the name of the regex or delimiter *)
val token_equal : token -> token -> bool

(** a chunk of input -- just a string *)
type chunk = string

val string_of_match_token : match_token -> string
val string_of_token : token -> string

(** breaks up a chunk into a list of token matches, plus some
    leftovers *)
val tokenize :
  chunk -> delimiter_table -> regex_table -> match_token list * string

(** given a list of match tokens, scans them for matching left/right
    and either/either pairs and condense them into metatokens. *)
val parse : match_token list -> token list

(** given a string and appropriate delimiter and regex tables,
    produces a parse: a list of tokens.  if tokenize returns any extra
    bits of string, they are ignored *)
val parse_chunk : chunk -> delimiter_table -> regex_table -> token list


