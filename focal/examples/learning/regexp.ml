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
    let matched = Str.matched_string s in
      if matched <> ""
      then 
	let token = tag matched in
	let left_over = Str.string_after s (Str.match_end ()) in
	  Some (token, left_over) 
      else
	None
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
       (* negated * ranges *)
    type t = bool * (char * char) list 

    let empty = (false, [])
    let full = (true, [])

      (* N.B. no check is done to see whether this range is already
	 included *)
    let add_range (neg, ranges) c1 c2 =
      (neg, (c1, c2)::ranges)

    let add s c = add_range s c c

    let singleton c = add empty c

    let complement (neg, ranges) = (not neg, ranges)
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
    let c = Charset.empty in
    let (c, j) = regexpclass2 c i i in
      (c, j)
  and regexpclass2 c start i =
    if i >= len then failwith "[ class not closed by ]";
    if s.[i] = ']' && i > start then (c, i+1) else begin
      let c1 = s.[i] in
      if i+2 < len && s.[i+1] = '-' && s.[i+2] <> ']' then begin
        let c2 = s.[i+2] in
	let c = Charset.add_range c c1 c2 in
          regexpclass2 c start (i+3)
      end else begin
        let c = Charset.add c c1 in
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

open Bsyntax

(* dummy parse info *)
let i = Info.M "regular expression"

let rec re_to_lens re =
  match re with
      Char c -> EString (i, Bstring.make 1 (Bstring.sym_of_char c))
    | String s -> EString (i, Bstring.t_of_string s)

    | CharClass (neg, ranges) ->
	let sym_ranges = 
	  List.map 
	    (fun (c1, c2) -> 
	       Bstring.sym_of_char c1, Bstring.sym_of_char c2)
	    ranges
	in
	  ECSet (i, neg, sym_ranges)	  

    | Seq [] -> failwith "empty seq"
    | Seq [re] -> re_to_lens re
    | Seq (re::res) -> ECat (i, re_to_lens re, re_to_lens (Seq res))

    | Alt (re1, re2) -> EUnion (i, re_to_lens re1, re_to_lens re2)
    | Star re -> EStar (i, re_to_lens re)
    | Plus re -> (* (re)+ ==> re . (re)* 
		    TODO let l = re in
		           l . (l)* 
		 *)
	let l = re_to_lens re in
	  ECat (i, l, EStar (i, l))
    | Option re -> EUnion (i, EString (i, Bstring.t_of_string ""), re_to_lens re)
    | Group (_, re) -> re_to_lens re

(* do we need this?  these should always get replaced with constants... *)
let delimiter_to_lens name = 
  let l, r = List.assoc name delimiter_table in
    re_to_lens (parse_re l), re_to_lens (parse_re r)

let regexp_to_lens name =
  let re = List.assoc name regexp_table in
    re_to_lens (parse_re re)

(*
let _ =
  let format_re_lens re_str = 
    Bsyntax.format_exp (re_to_lens (parse_re re_str)) 
  in
    List.iter 
      (fun (name, re) ->
	 Util.format "@[%s:@ " name;
	 format_re_lens re;
	 Util.format "@]@\n")
      regexp_table;
    List.iter
      (fun (name, (l, r)) ->
	 Util.format "@[%s:@ " name;
	 format_re_lens l;
	 Util.format ",@ ";
	 format_re_lens r;
	 Util.format "@]@\n")
      delimiter_table
*)
