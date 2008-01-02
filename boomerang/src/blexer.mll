(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/blexer.mll                                                   *)
(* Boomerang lexer                                                             *)
(* $Id$                                                                        *)
(*******************************************************************************)
{
open Bparser

module LE = Lexing

let lexeme = LE.lexeme

let sprintf = Printf.sprintf

(* We track of lexing information using a stack. A stack is required
   because the lexer is invoked, on-demand, from the registry to parse
   strings into sorts and qualified identifiers. *)
let info_stk = ref []

let filename () = match !info_stk with 
    [] -> Error.simple_error "Blexer.filename : info stack is empty."
  | (fn,_,_)::_ -> fn

let lineno () = match !info_stk with 
    [] -> Error.simple_error "Blexer.lineno : info stack is empty."
  | (_,l,_)::_ -> l

let linestart () = match !info_stk with 
    [] -> Error.simple_error "Blexer.linestart : info stack is empty."
  | (_,_,c)::_ -> c

let set_filename fn = match !info_stk with 
    [] -> Error.simple_error "Blexer.set_filename : info stack is empty."
  | (_,l,c)::t -> info_stk := (fn,l,c)::t

let set_lineno l = match !info_stk with 
    [] -> Error.simple_error "Blexer.set_lineno : info stack is empty."
  | (fn,_,c)::t -> info_stk := (fn,l,c)::t

let set_linestart c = match !info_stk with 
    [] -> Error.simple_error "Blexer.set_linestart : info stack is empty."
  | (fn,l,_)::t -> info_stk := (fn,l,c)::t

let setup fn = info_stk := (fn,1,0)::!info_stk

let finish () = match !info_stk with
    [] -> Error.simple_error "Blexer.finish : info stack is empty."
  | _::t -> info_stk := t

let newline lexbuf : unit = 
  set_linestart (LE.lexeme_start lexbuf);
  set_lineno (lineno () + 1)

let info lexbuf : Info.t = 
  let c1 = LE.lexeme_start lexbuf in
  let c2 = LE.lexeme_end lexbuf in
  let l = lineno () in
  let c = linestart () in
    Info.I (filename(), (l, c1 - c - 1),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = lexeme lexbuf in   
  let s = Printf.sprintf "%s: lexing error %s at %s." 
    (Info.string_of_t i)
    msg 
    t in
    Error.simple_error s

let text = Lexing.lexeme

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))

let keywords = Hashtbl.create 17
let _ = 
  Safelist.iter (fun (kw,tok) -> Hashtbl.add keywords kw tok)
    [ ("module", (fun i -> MODULE i))
    ; ("open", (fun i -> OPEN i)) 
    ; ("end", (fun i -> END i)) 
    ; ("let", (fun i -> LET i)) 
    ; ("in", (fun i -> IN i))
    ; ("fun", (fun i -> FUN i))
    ; ("begin", (fun i -> BEGIN i))
    ; ("end", (fun i -> END i))
    ; ("test", (fun i -> TEST i))
    ; ("into", (fun i -> INTO i))
    ; ("get", (fun i -> GET i))
    ; ("put", (fun i -> PUT i))
    ; ("create", (fun i -> CREATE i))
    ; ("crt", (fun i -> CREATE i))
    ; ("match", (fun i -> MATCH i))
    ; ("with", (fun i -> WITH i))
    ; ("error", (fun i -> ERROR i))
    ; ("string", (fun i -> STRING i))
    ; ("regexp", (fun i -> REGEXP i))
    ; ("lens", (fun i -> LENS i))
    ; ("canonizer", (fun i -> CANONIZER i))
    ]
}

let whitespace = [' ' '\t']+
let newline = "\n"
let id_char_first = ['a'-'z' 'A'-'Z' '\'' '_' '-' '@']
let id_char_rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '-' '@']
let int_char = ['0' - '9']
let string = '"' [^'"']* '"'

rule main = parse
| whitespace         { main lexbuf }
| "("                { LPAREN(info lexbuf) }
| ")"                { RPAREN(info lexbuf) }
| ";"                { SEMI(info lexbuf) }
| ".get"             { DOTGET(info lexbuf) }
| ".put"             { DOTPUT(info lexbuf) }
| ".create"          { DOTCREATE(info lexbuf) }
| ".crt"             { DOTCREATE(info lexbuf) }
| "."                { DOT(info lexbuf) }
| "&"                { AMPERSAND(info lexbuf) }
| "*"                { STAR(info lexbuf) }
| "-"                { MINUS(info lexbuf) }
| "+"                { PLUS(info lexbuf) }
| "!"                { BANG(info lexbuf) }
| "->"               { ARROW(info lexbuf) }
| "--->"             { LONGARROW(info lexbuf) }
| "<--->"            { LONGDARROW(info lexbuf) }
| "<->"              { DARROW(info lexbuf) }
| "|"                { BAR(info lexbuf) }
| "="                { EQUAL(info lexbuf) }
| "{"                { LBRACE(info lexbuf) }
| "}"                { RBRACE(info lexbuf) }
| "["                { CSET(info lexbuf, cset lexbuf) }
| "[^"               { NSET(info lexbuf, cset lexbuf) }
| "<"                { LANGLE(info lexbuf) }
| "<|"               { LANGLEBAR(info lexbuf)}
| "<<<"              { STR(info lexbuf,bare lexbuf) }
| ">"                { RANGLE(info lexbuf) }
| "|>"               { BARRANGLE(info lexbuf) }
| ","                { COMMA(info lexbuf) }
| ":"                { COLON(info lexbuf) }
| "^"                { HAT(info lexbuf) }
| "~"                { TILDE(info lexbuf) }
| "\\"               { BACKSLASH(info lexbuf) }
| ","                { COMMA(info lexbuf) }
| "?"                { QMARK(info lexbuf) }
| "\""               { STR (info lexbuf, string lexbuf) }
| ">/"               { RANGLESLASH(info lexbuf) }
| "/<"               { SLASHLANGLE(info lexbuf) }
| id_char_first id_char_rest* as ident { 
      try let kw = Hashtbl.find keywords ident in
          kw (info lexbuf)
      with Not_found -> IDENT (info lexbuf, ident) }
| int_char+ as integ { INT(info lexbuf, int_of_string integ) }
| newline            { newline lexbuf; main lexbuf }
| eof                { EOF(info lexbuf) } 
| "(*"               { comment lexbuf; main lexbuf }
| _                  { error lexbuf "unknown token" }

and cset = parse
  | "\\\\"           { "\\\\" ^ cset lexbuf }
  | "\\]"            { "]" ^ cset lexbuf }
  | "]"              { "" }  
  | "\n"             { newline lexbuf; "\n" ^ cset lexbuf }
  | _                { let s = lexeme lexbuf in s ^ cset lexbuf }

and string = parse
| "\\"          { let s = escape lexbuf in s ^ string lexbuf }
| "\""          { "" }
| newline [' ' '\t']* "|" { newline lexbuf; "\n" ^ string lexbuf }
| newline       { newline lexbuf; let s = lexeme lexbuf in s ^ string lexbuf }
| eof           { error lexbuf "unmatched '\"'"}
| _             { let s = lexeme lexbuf in s ^ string lexbuf }

and bare = parse
  | newline [' ']* { newline lexbuf; 
                     let s = lexeme lexbuf in 
                     let n = String.length s - 1 in 
                       bare_indent n lexbuf }
  | ">>>"          { "" }
  | eof            { error lexbuf "unmatched '<<<'" }
  | _              { let s = lexeme lexbuf in s ^ bare_raw lexbuf }

and bare_indent_spaces n expected = parse
  | [' ']            { if expected=0 then let s = lexeme lexbuf in s ^ bare_indent n lexbuf 
                       else bare_indent_spaces n (pred expected) lexbuf }
  (* XXX: should we check that the number of spaces is <= n? *)
  | newline [' ']* ">>>" { "" } 
  | newline          { newline lexbuf; let s = lexeme lexbuf in s ^ bare_indent_spaces n n lexbuf }
  | _                { if expected=0 then 
                         let s = lexeme lexbuf in 
                           s ^ bare_indent n lexbuf 
                       else
                         error lexbuf (sprintf "expecting %d spaces after newline in string" n) }

and bare_indent n = parse
  | newline [' ']* ">>>" { newline lexbuf; "" }
  | newline          { newline lexbuf; 
                       let s = lexeme lexbuf in 
                       s ^ bare_indent_spaces n n lexbuf }
  | eof              { error lexbuf "unmatched '>>>'" }
  | _                { let s = lexeme lexbuf in s ^ bare_indent n lexbuf }
  
and bare_raw = parse
  | ">>>"       { "" }
  | newline     { newline lexbuf; let s = lexeme lexbuf in s ^ bare_raw lexbuf }
  | eof         { error lexbuf "unmatched '<<<'" }
  | _           { let s = lexeme lexbuf in s ^ bare_raw lexbuf }

and escape = parse
| "'"           { "'" }
| "\""          { "\"" }
| "\\"          { "\\" }
| "b"           { "\008" }
| "n"           { "\010" }
| "r"           { "\013" }
| "t"           { "\009" }
| _             { error lexbuf "in string escape sequence" }

and comment = parse
| "(*"             { comment lexbuf; comment lexbuf }
| "*)"             { () }
| newline          { newline lexbuf; comment lexbuf }
| eof              { error lexbuf "unmatched '(*'" }
| _                { comment lexbuf }
