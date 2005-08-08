{

open Metay
open Error

module LE = Lexing

let lexeme = LE.lexeme
let linestart = ref 0
let lineno = ref 1

let info_stk = ref []

let filename () = match !info_stk with 
    [] -> Error.simple_error "Lexer.filename : info stack is empty."
  | (fn,_,_)::_ -> fn
      
let lineno () = match !info_stk with 
    [] -> Error.simple_error "Lexer.lineno : info stack is empty."
  | (_,l,_)::_ -> l

let linestart () = match !info_stk with 
    [] -> Error.simple_error "Lexer.linestart : info stack is empty."
  | (_,_,c)::_ -> c

let set_filename fn = match !info_stk with 
    [] -> Error.simple_error "Lexer.set_filename : info stack is empty."
  | (_,l,c)::t -> info_stk := (fn,l,c)::t
      
let set_lineno l = match !info_stk with 
    [] -> Error.simple_error "Lexer.set_lineno : info stack is empty."
  | (fn,_,c)::t -> info_stk := (fn,l,c)::t
      
let set_linestart c = match !info_stk with 
    [] -> Error.simple_error "Lexer.set_linestart : info stack is empty."
  | (fn,l,_)::t -> info_stk := (fn,l,c)::t

let setup fn = info_stk := (fn,1,0)::!info_stk

let finish () = match !info_stk with
    [] -> Error.simple_error "Lexer.finish : info stack is empty."
  | _::t -> info_stk := t

let newline lexbuf : unit = 
  set_linestart (LE.lexeme_start lexbuf);
  set_lineno (lineno () + 1)

let info lexbuf : Info.t = 
  let c1 = LE.lexeme_start lexbuf in
  let c2 = LE.lexeme_end lexbuf in
    Info.I (filename (), 
	    (lineno (), c1 - linestart()),
	    (lineno (), c2 - linestart ()))

let error lexbuf msg =
  let i = info lexbuf in
  let t = lexeme lexbuf in   
  let s = Printf.sprintf "%s: lexing error %s at %s." 
    (Info.string_of_t i)
    msg 
    t in
    Error.simple_error s
}

let blank = [' ' '\t' '\r']+
let newline = "\n"
let notsymbol = [^ ' ' '\t' '\n' '\"' '{' '}' '[' ']' '=' ',' ':']+

rule token = parse
| blank		{ token lexbuf }
| newline       { newline lexbuf; token lexbuf }
| "="		{ EQUAL (info lexbuf) }
| "{"		{ LBRACE (info lexbuf) }
| "}"		{ RBRACE (info lexbuf) }
| "["		{ LBRACK (info lexbuf) }
| "]"		{ RBRACK (info lexbuf) }
| ","           { COMMA (info lexbuf) }
| ":"           { COLON (info lexbuf) }
| "\""		{ IDENT (Syntax.mk_id (info lexbuf) (string lexbuf)) }
| notsymbol	{ let i = (info lexbuf) in IDENT (Syntax.mk_id i (lexeme lexbuf)) }
| eof		{ EOF (info lexbuf) }
| _		{ error lexbuf "Unknown token" }

and string = parse
| "\\"		{ let s = escape lexbuf in s ^ string lexbuf }
| "\""		{ "" }
| newline	{ newline lexbuf; let s = lexeme lexbuf in s ^ string lexbuf }
| _		{ let s = lexeme lexbuf in s ^ string lexbuf }
| eof		{ error lexbuf "Unmatched '\"'"}

and escape = parse
| "\""		{ "\"" }
| "\\"		{ "\\" }
| "n"		{ "\010" }
| "t"		{ "\009" }
| eof		{ error lexbuf "EOF when reading escape" }
| _		{ error lexbuf "Unknown escape" }

