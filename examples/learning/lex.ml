type 'a table_entry = string * 'a

(** ordered tables, implemented as assoc lists *)
type 'a table = 'a table_entry list

(** tables of (uncompiled) regular expression strings *)
type regex_table = string table

(** tables of pairs of delimiter strings, such as "(" and ")" *)
type delimiter_table = (string * string) table

let regex_table =
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

let regex_length name =
  let re_str =
    try 
      List.assoc name regex_table
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

(** the tokens produced by parsing

    the metatokens exhibit the tree structure implied by the
    match_tokens
*)
type token =
           (* raw string * name *)
    RegexToken of string * string
                (* left * sub-tokens * right  * name *)
  | MetaToken of string * token list * string * string

let token_equal t1 t2 =
  match t1, t2 with
      RegexToken (_, n1), RegexToken (_, n2)
    | MetaToken (_, _, _, n1), MetaToken (_, _, _, n2) when n1 = n2 -> true
    | _ -> false

(** a chunk of input -- just a string *)
type chunk = string

let string_of_match_token t =
  match t with
      RegexMatch (s, name) -> "RegexMatch (" ^ s ^ ", " ^ name ^ ")"
    | EitherMatch (s, name) -> "EitherMatch(" ^ s ^ ", " ^ name ^ ")"
    | LeftMatch (s, name) -> "LeftMatch(" ^ s ^ ", " ^ name ^ ")"
    | RightMatch (s, name) -> "RightMatch(" ^ s ^ ", " ^ name ^ ")"

let rec string_of_token t =
  match t with
      RegexToken (s, name) -> "RegexToken ('" ^ s ^ "', " ^ name ^ ")"
    | MetaToken (l, ts, r, name) -> 
	let string_of_ts = List.map string_of_token ts in
          "MetaToken ('" ^ l ^ "', [" ^ (String.concat ";" string_of_ts) ^ "], '" ^ r ^ "', " ^ name ^ ")"

(** the lexing of a matched token, returning a match_token and the
   remaining string *)
(* matched token * leftover string *)
type match_token_lex = (match_token * chunk) option

(** tries to match a prefix of the given string with the given regex.
    if the string is successfuly matched, then Some (tag
    (the-string-match)) is returned.  otherwise, the function returns
    None. *)
let match_prefix (s : chunk) (p : string) (tag : chunk -> match_token) =
  if Str.string_partial_match (Str.regexp ("\\("^p^"\\)")) s 0
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
let delim_match (s : chunk) (name, (l, r)) : match_token_lex =
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
let regex_match (s : chunk) (name, re) : match_token_lex =
  match_prefix s re (fun s -> RegexMatch (s, name))

(** given a string, a table, and a function to lookup, tries to match
    each entry of the table.  the first successful match is taken*)
let find_match (s : chunk) (t : 'a table) 
    (match_fun : chunk -> 'a table_entry -> match_token_lex) 
    : match_token_lex =
  List.fold_left
    (fun r m -> 
       if r = None
       then match_fun s m 
       else r)
    None
    t

(** breaks up a chunk into a list of token matches, plus some leftovers *)
let rec tokenize (s : chunk)
    (dt : delimiter_table)
    (rt : regex_table)
    : match_token list * chunk =
  let run_match s t match_fun (k : unit -> match_token list * chunk) =
    if s = ""
    then [], ""
    else match find_match s t match_fun with
        Some (t, s) -> let ts, s = tokenize s dt rt in t::ts, s
      | None -> k ()
  in
    (* if only there were some sort of if syntax for option types!
       argh! *)
    run_match s dt delim_match 
      (fun () -> run_match s rt regex_match 
	 (fun () -> [], s))

exception Unmatched_delimiter

(** given a LeftMatch or EitherMatch as a starting delimiter, scans a
    list of match_tokens for the matching ending delimiter.  it favors
    child delimiters over parent delimiters: ((...) will parse as
    RegexToken(MetaToken([...])).
*)
let rec scan (ts : match_token list) 
    (d : match_token) 
    (sub_ts : token list) 
    : token * match_token list =
  let left, delim = match d with
      LeftMatch (left, delim)
    | EitherMatch (left, delim) -> left, delim
    | _ -> 
        failwith "expected either LeftMatch or EitherMatch as initial delimiter"
  in
    match ts with
	[] -> raise Unmatched_delimiter
      | (RegexMatch (s, name))::ts -> scan ts d (RegexToken (s, name)::sub_ts)
	  
      (* we treat an EitherMatch like a LeftMatch when it's for
	 another delimiter *)
      | (EitherMatch (s, name))::ts when name <> delim ->
	  scan ((LeftMatch (s, name))::ts) d sub_ts
	    
      | (LeftMatch (s, name))::ts ->
	  begin
            try let (t, ts) = scan ts (LeftMatch (s, name)) [] in
              scan ts d (t::sub_ts)
            with Unmatched_delimiter -> scan ts d (RegexToken (s, name)::sub_ts)
	  end
	    
      (* we treat an EitherMatch like a RightMatch when it's for the
	 current delimiter *)
      | (EitherMatch (right, name))::ts
      | (RightMatch (right, name))::ts ->
	  if name = delim
	  then MetaToken (left, List.rev sub_ts, right, delim), ts
	    (* EitherMatch for our current delimiter should never get
	       here, guarded clause above catches it.  therefore it's
	       safe to just keep on scanning, ignoring this delimiter *)
	  else scan ts d ((RegexToken (right, name))::sub_ts) 

(** given a list of match tokens, uses scan to find matching
    left/right and either/either pairs and condense them into
    metatokens. *)
let rec parse (ts : match_token list) : token list =
  match ts with
      [] -> []
	
    (* non-scanning terms -- just leave it *)
    | (RightMatch (s, name))::ts
    | (RegexMatch (s, name))::ts -> RegexToken (s, name)::(parse ts)
	
    (* start of a scan *)
    | (EitherMatch (s, name))::ts
    | (LeftMatch (s, name))::ts ->
	try let t, ts = scan ts (LeftMatch (s, name)) [] in
          t::(parse ts)
	with Unmatched_delimiter -> RegexToken (s, name)::(parse ts)

(** given a string and appropriate delimiter and regex tables,
    produces a parse: a list of tokens.  if tokenize returns any extra
    bits of string, they are ignored *)
let parse_chunk s dt rt =
  let ts, _ = tokenize s dt rt in
    parse ts
