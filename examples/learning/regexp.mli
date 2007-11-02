(** given a regex name, looks it up and returns the length of the regex *)
val regexp_length : string -> int

type match_token =
    (* raw string * name *)
    RegexMatch of string * string
  | EitherMatch of string * string
  | LeftMatch  of string * string
  | RightMatch of string * string

(** the matching of a regular expression on the prefix of a string:
      a match_token and the remaining string *)
type match_prefix = match_token * string

(** tries to match a prefix of the given string with the start or end
    of a given delimiter.  it detects when the two delimiters are
    equal (single quote, for example) and produces an EitherMatch
    structure; the rest of the time, it will produce LeftMatch and
    RightMatches, or None if no match is found
*)
val find_delimiter_match : string -> match_prefix option

(** tries to match a prefix of the given string with the given regex,
    tagging any matches as RegexMatch *)
val find_regexp_match : string -> match_prefix option
