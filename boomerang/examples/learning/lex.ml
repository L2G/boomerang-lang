open Regexp

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

let rec string_of_token t =
  match t with
      RegexToken (s, name) -> "RegexToken ('" ^ s ^ "', " ^ name ^ ")"
    | MetaToken (l, ts, r, name) -> 
	let string_of_ts = List.map string_of_token ts in
          "MetaToken ('" ^ l ^ "', [" ^ (String.concat ";" string_of_ts) ^ "], '" ^ r ^ "', " ^ name ^ ")"

(** breaks up a chunk into a list of token matches, plus some leftovers *)
let rec tokenize (s : chunk)
    : match_token list * chunk =
  let run_match s match_fun (k : unit -> match_token list * chunk) =
    if s = ""
    then [], ""
    else match match_fun s with
        Some (t, s) -> let ts, s = tokenize s in t::ts, s
      | None -> k ()
  in
    (* if only there were some sort of if syntax for option types!
       argh! *)
    run_match s find_delimiter_match
      (fun () -> run_match s find_regexp_match
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
      | (RegexMatch (s, name))::ts -> 
	  scan ts d (RegexToken (s, name)::sub_ts)
	  
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
let parse_chunk s =
  let ts, _ = tokenize s in
    parse ts
