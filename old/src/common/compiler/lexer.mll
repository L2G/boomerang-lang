{
open Parser
open Error

module LE = Lexing

let lexeme = LE.lexeme
let linestart = ref 0
let lineno = ref 1

let newline lexbuf : unit = 
  linestart := LE.lexeme_start lexbuf;
  incr lineno

let info lexbuf : info = 
  let c1 = LE.lexeme_start lexbuf in
  let c2 = LE.lexeme_end lexbuf in
  (!lineno, c1 - !linestart),(!lineno, c2 - !linestart)

let error lexbuf s =
  let (l1,c1),(l2,c2) = info lexbuf in
  let t = lexeme lexbuf in
  let s = Printf.sprintf "%d:%d-%d: %s" l1 c1 c2 s in
  if t = "" then raise (Syntax_error (s,((l1,c1),(l2,c2))))
  else raise (Syntax_error (s^ ": " ^ t,((l1,c1),(l2,c2))))
 
}

let blank = [' ' '\t']+
let newline = "\n"
let symbol = [^ '"' '=' '{' '}' '[' ']' '(' ')' '<' '>' ';' ',' '\n' '\r' ' ' '\t']+
let type_ident = [^ '"' '=' '{' '}' '[' ']' '(' ')' '!' '*' '.' '?' '\\' '\n' '\r' ' ' '\t']+
let string = '"' [^'"']* '"'
let anyline = [^'\n']* '\n'

rule token = parse
| blank		{ token lexbuf }
| "->"          { Arr (info lexbuf) }
| "let"         { Let (info lexbuf) }
| "in"          { In (info lexbuf) }
| "rec"         { Rec (info lexbuf) }
| "and"         { And (info lexbuf) }
| "do"          { Do (info lexbuf) }
| "in"          { Do (info lexbuf) }
| "function"    { Fun (info lexbuf) }
| "\""		{ String ((string lexbuf),(info lexbuf)) }
| "="		{ Equal (info lexbuf) }
| "{"		{ Lbrace (info lexbuf) }
| "}"		{ Rbrace (info lexbuf) }
| "["		{ Lbrack (info lexbuf) }
| "]"		{ Rbrack (info lexbuf) }
| "("		{ Lparen (info lexbuf) }
| ")"		{ Rparen (info lexbuf) }
| "<"		{ Langle (info lexbuf) }
| ">"		{ Rangle (info lexbuf) }
| ";"           { Semi (info lexbuf) }
| ","           { Comma (info lexbuf) }
(* generic stuff *)
| symbol	{ let i = (info lexbuf) in Ident ((lexeme lexbuf), i) }
| newline       { newline lexbuf; token lexbuf }
| eof		{ Eof (info lexbuf) }
| "(*"          { comment lexbuf; token lexbuf }
(* symbol covers the rest of the alphabets *)
| _		{ error lexbuf "Unknown token" }

and type_token = parse
  | blank      	{ type_token lexbuf }
  | "type"      { Type(info lexbuf) }
  | "empty"     { Empty(info lexbuf) }
  | "="	       	{ Equal (info lexbuf) }
  | "{"	       	{ Lbrace (info lexbuf) }
  | "}"	       	{ Rbrace (info lexbuf) }
  | "["	       	{ Lbrack (info lexbuf) }
  | "]"	       	{ Rbrack (info lexbuf) }
  | "("	       	{ Lparen (info lexbuf) }
  | ")"	       	{ Rparen (info lexbuf) }
  | '.'         { Dot (info lexbuf) }
  | "*"         { Star (info lexbuf) } 
  | '!'         { Bang (info lexbuf) }
  | '?'         { QMark (info lexbuf) }
  | '|'         { Bar (info lexbuf) }
  | "\""       	{ Ident ((string lexbuf),(info lexbuf)) }
  | '\\'        { Slash (info lexbuf) }
  | type_ident  { let i = (info lexbuf) in Ident ((lexeme lexbuf), i) }
  | newline     { newline lexbuf; type_token lexbuf }
  | eof	       	{ Eof (info lexbuf) }
  | "(*"        { comment lexbuf; type_token lexbuf }
  | _           { error lexbuf "Unknown token" }
(* | '-'        { Minus (info lexbuf) } *) (* unimplemented for now *)
(* | '&'        { Amp (info lexbuf) } *)   (* unimplemented for now *)

and string = parse
| "\\"		{ let s = escape lexbuf in s ^ string lexbuf }
| "\""		{ "" }
| newline	{ newline lexbuf; let s = lexeme lexbuf in s ^ string lexbuf }
| _		{ let s = lexeme lexbuf in s ^ string lexbuf }
| eof		{ error lexbuf "Unmatched '\"'"}

and escape = parse
| "'"		{ "'" }
| "\""		{ "\"" }
| "\\"		{ "\\" }
| "b"		{ "\008" }
| "n"		{ "\010" }
| "r"		{ "\013" }
| "t"		{ "\009" }
| eof		{ error lexbuf "EOF when reading escape" }
| _		{ error lexbuf "Unknown escape" }

and comment = parse
| "(*"             { comment lexbuf; comment lexbuf }
| "*)"             { () }
| newline          { newline lexbuf; comment lexbuf }
| eof		   { error lexbuf "Unmatched '(*'" }
| _                { comment lexbuf }
