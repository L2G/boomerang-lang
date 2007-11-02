type 'a table_entry = string * 'a

(** ordered tables, implemented as assoc lists *)
type 'a table = 'a table_entry list

(** tables of (uncompiled) regular expression strings *)
type regexp_table = string table

(** tables of pairs of delimiter strings, such as "(" and ")" *)
type delimiter_table = (string * string) table

let regexp_table =
  [("alpha", "[a-zA-Z]+");
   ("digits", "[0-9]+");
   ("newline", "\n");
   ("ws", "[ \n\t]+");
   ("comma", ",");
   ("colon", ":");
   ("semi-colon", ";");
   ("dash", "-");
   ("period", "\\.");
   ("other", ".")]

let delimiter_table =
  [("parens", ("(", ")"));
   ("squares", ("\\[", "\\]"));
   ("curlies", ("{", "}"));
   ("angles", ("<", ">"));
   ("dquotes", ("\"", "\""));
   ("squotes", ("'", "'"))]

let regexp_length name =
  let re_str =
    try 
      List.assoc name regexp_table
    with Not_found -> 
      let l, r = List.assoc name delimiter_table in
	l ^ r
  in
    String.length re_str

(** the tokens matched in tokenization 

    these tokens are like a lexing phase -- we identify potential
    delimiters, but the parsing phase and the token type resolve actual
    nesting
*)
type match_token =
    (* raw string * name *)
    RegexMatch of string * string
  | EitherMatch of string * string
  | LeftMatch  of string * string
  | RightMatch of string * string

let string_of_match_token t =
  match t with
      RegexMatch (s, name) -> "RegexMatch (" ^ s ^ ", " ^ name ^ ")"
    | EitherMatch (s, name) -> "EitherMatch(" ^ s ^ ", " ^ name ^ ")"
    | LeftMatch (s, name) -> "LeftMatch(" ^ s ^ ", " ^ name ^ ")"
    | RightMatch (s, name) -> "RightMatch(" ^ s ^ ", " ^ name ^ ")"

type match_prefix = match_token * string

(** tries to match a prefix of the given string with the given regex.
    if the string is successfuly matched, then Some (tag
    (the-string-match)) is returned.  otherwise, the function returns
    None. *)
let match_prefix (s : string) (p : string) (tag : string -> 'a) : match_prefix option =
  if Str.string_partial_match (Str.regexp ("\\("^ p ^"\\)")) s 0
  then 
    let token = tag (Str.matched_string s) in
    let left_over = Str.string_after s (Str.match_end ()) in
      Some (token, left_over) 
  else None

(** tries to match a prefix of the given string with the start or end
    of a given delimiter.  it detects when the two delimiters are
    equal (when using ', for example) and produces an EitherMatch
    structure; the rest of the time, it will produce LeftMatch and
    RightMatches, or None if no match is found
*)
let delim_match (s : string) (name, (l, r)) : match_prefix option =
  if l = r
  then match_prefix s l (fun s -> EitherMatch (s, name))
  else 
    let l_match = match_prefix s l (fun s -> LeftMatch (s, name)) in
    let r_match = match_prefix s r (fun s -> RightMatch (s, name)) in
      match l_match, r_match with
          (None, None) -> None
	| (Some _, None) -> l_match
	| (None, Some _) -> r_match
	| (Some _, Some _) -> failwith "both delimiters matched...why aren't their regular expressions disjoint?"

(** tries to match a prefix of the given string with the given regex,
    tagging any matches as RegexMatch *)
let regexp_match (s : string) (name, re) : match_prefix option =
  match_prefix s re (fun s -> RegexMatch (s, name))

(** given a string, a table, and a function to lookup, tries to match
    each entry of the table.  the first successful match is taken*)
let find_match (s : string) (t : 't table) 
    (match_fun : string -> 't table_entry -> match_prefix option)
    : match_prefix option =
  List.fold_left
    (fun r m -> 
       if r = None
       then match_fun s m 
       else r)
    None
    t

let find_delimiter_match s = find_match s delimiter_table delim_match
let find_regexp_match s = find_match s regexp_table regexp_match
