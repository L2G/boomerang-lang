{

open Metay
open Error

module LE = Lexing

let lexeme = LE.lexeme
let linestart = ref 0
let lineno = ref 1

let reset () = 
  linestart := 0; 
  lineno := 1

let newline lexbuf : unit = 
  linestart := LE.lexeme_start lexbuf;
  incr lineno

let info lexbuf : Info.t = 
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
let notsymbol = [^ ' ' '\t' '\n' '\"' '{' '}' '[' ']' '=' ',' ':']+

rule token = parse
| blank		{ token lexbuf }
| "="		{ EQUAL (info lexbuf) }
| "{"		{ LBRACE (info lexbuf) }
| "}"		{ RBRACE (info lexbuf) }
| "["		{ LBRACK (info lexbuf) }
| "]"		{ RBRACK (info lexbuf) }
| ","           { COMMA (info lexbuf) }
| ":"           { COLON (info lexbuf) }
| "\""		{ IDENT ((info lexbuf), (string lexbuf)) }
| notsymbol	{ let i = (info lexbuf) in IDENT (i, (lexeme lexbuf)) }
| newline       { newline lexbuf; token lexbuf }
| eof		{ EOF (info lexbuf) }
| _		{ error lexbuf "Unknown token" }

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
