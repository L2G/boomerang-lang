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

(** REGEXP PARSING, from ocaml 3.09 otherlibs/str/str.ml *)

(** Representation of character sets **)

module Charset =
  struct
    type t = string (* of length 32 *)

    let empty = String.make 32 '\000'
    let full = String.make 32 '\255'

    let make_empty () = String.make 32 '\000'

    let add s c =
      let i = Char.code c in
      s.[i lsr 3] <- Char.chr(Char.code s.[i lsr 3] lor (1 lsl (i land 7)))

    let add_range s c1 c2 =
      for i = Char.code c1 to Char.code c2 do add s (Char.chr i) done

    let singleton c =
      let s = make_empty () in add s c; s

    let range c1 c2 =
      let s = make_empty () in add_range s c1 c2; s

    let complement s =
      let r = String.create 32 in
      for i = 0 to 31 do
        r.[i] <- Char.chr(Char.code s.[i] lxor 0xFF)
      done;
      r

    let union s1 s2 =
      let r = String.create 32 in
      for i = 0 to 31 do
        r.[i] <- Char.chr(Char.code s1.[i] lor Char.code s2.[i])
      done;
      r

    let disjoint s1 s2 =
      try
        for i = 0 to 31 do
          if Char.code s1.[i] land Char.code s2.[i] <> 0 then raise Exit
        done;
        true
      with Exit ->
        false

    let iter fn s =
      for i = 0 to 31 do
        let c = Char.code s.[i] in
        if c <> 0 then
          for j = 0 to 7 do
            if c land (1 lsl j) <> 0 then fn (Char.chr ((i lsl 3) + j))
          done
      done

    let expand s =
      let r = String.make 256 '\000' in
      iter (fun c -> r.[Char.code c] <- '\001') s;
      r

    let fold_case s =
      let r = make_empty() in
      iter (fun c -> add r (Char.lowercase c); add r (Char.uppercase c)) s;
      r

  end

(** Abstract syntax tree for regular expressions *)

type re_syntax =
    Char of char
  | String of string
  | CharClass of Charset.t
  | Seq of re_syntax list
  | Alt of re_syntax * re_syntax
  | Star of re_syntax
  | Plus of re_syntax
  | Option of re_syntax
  | Group of int * re_syntax

(* Efficient buffering of sequences *)

module SeqBuffer = struct

  type t = { sb_chars: Buffer.t; mutable sb_next: re_syntax list }

  let create() = { sb_chars = Buffer.create 16; sb_next = [] }

  let flush buf =
    let s = Buffer.contents buf.sb_chars in
    Buffer.clear buf.sb_chars;
    match String.length s with
      0 -> ()
    | 1 -> buf.sb_next <- Char s.[0] :: buf.sb_next
    | _ -> buf.sb_next <- String s :: buf.sb_next

  let add buf re =
    match re with
      Char c -> Buffer.add_char buf.sb_chars c
    | _ -> flush buf; buf.sb_next <- re :: buf.sb_next

  let extract buf =
    flush buf; Seq(List.rev buf.sb_next)

end

let dotclass = Charset.complement (Charset.singleton '\n')

(* Parse a regular expression *)

let parse_re s =
  let len = String.length s in
  let group_counter = ref 1 in

  let rec regexp0 i =
    let (r, j) = regexp1 i in
    regexp0cont r j
  and regexp0cont r1 i =
    if i + 2 <= len && s.[i] = '\\' && s.[i+1] = '|' then
      let (r2, j) = regexp1 (i+2) in
      regexp0cont (Alt(r1, r2)) j
    else
      (r1, i)
  and regexp1 i =
    regexp1cont (SeqBuffer.create()) i
  and regexp1cont sb i =
    if i >= len
    || i + 2 <= len && s.[i] = '\\' && (let c = s.[i+1] in c = '|' || c = ')')
    then
      (SeqBuffer.extract sb, i)
    else
      let (r, j) = regexp2 i in
      SeqBuffer.add sb r;
      regexp1cont sb j
  and regexp2 i =
    let (r, j) = regexp3 i in
    regexp2cont r j
  and regexp2cont r i =
    if i >= len then (r, i) else
      match s.[i] with
        '?' -> regexp2cont (Option r) (i+1)
      | '*' -> regexp2cont (Star r) (i+1)
      | '+' -> regexp2cont (Plus r) (i+1)
      |  _  -> (r, i)
  and regexp3 i =
    match s.[i] with
      '\\' -> regexpbackslash (i+1)
    | '['  -> let (c, j) = regexpclass0 (i+1) in (CharClass c, j)
    | '^'  -> failwith "Beginning of line (^) unsupported; use \\n and context; use \\^ for a literal caret"
    | '$'  -> failwith "End of line ($) unsupported; use \\n; use \\$ for a literal dollar sign"
    | '.'  -> (CharClass dotclass, i+1)
    | c    -> (Char c, i+1)
  and regexpbackslash i =
    if i >= len then (Char '\\', i) else
      match s.[i] with
        '|' | ')' ->
          assert false
      | '(' ->
          let group_no = !group_counter in
          if group_no < 32 then incr group_counter;
          let (r, j) = regexp0 (i+1) in
          if j + 1 < len && s.[j] = '\\' && s.[j+1] = ')' then
            if group_no < 32
            then (Group(group_no, r), j + 2)
            else (r, j + 2)
          else
            failwith "\\( group not closed by \\)"
      | '1' .. '9' ->
          failwith "Dependent regular expressions (via group reference) are unsupported"
      | 'b' ->
	  failwith "Word boundaries are not supported -- specify characters explicitly"
      | c ->
          (Char c, i + 1)
  and regexpclass0 i =
    if i < len && s.[i] = '^'
    then let (c, j) = regexpclass1 (i+1) in (Charset.complement c, j)
    else regexpclass1 i
  and regexpclass1 i =
    let c = Charset.make_empty() in
    let j = regexpclass2 c i i in
    (c, j)
  and regexpclass2 c start i =
    if i >= len then failwith "[ class not closed by ]";
    if s.[i] = ']' && i > start then i+1 else begin
      let c1 = s.[i] in
      if i+2 < len && s.[i+1] = '-' && s.[i+2] <> ']' then begin
        let c2 = s.[i+2] in
        Charset.add_range c c1 c2;
        regexpclass2 c start (i+3)
      end else begin
        Charset.add c c1;
        regexpclass2 c start (i+1)
      end
    end in

  let (r, j) = regexp0 0 in
  if j = len then r else failwith "spurious \\) in regular expression"

let lensable re_str =
  try 
    let _ = parse_re re_str in
      true
  with exn -> false

let _ =
  List.iter 
    (fun (name, re) ->
       print_string name; print_string ": ";
       print_endline (if lensable re
		      then "lensable"
		      else "not lensable"))
    regexp_table;
  List.iter
    (fun (name, (l, r)) ->
              print_string name; print_string ": ";
       print_endline (if lensable l && lensable r
		      then "lensable"
		      else "not lensable"))
    delimiter_table

let _ = Bsyntax.EString (Info.M "foo", Bstring.t_of_string "testing")
