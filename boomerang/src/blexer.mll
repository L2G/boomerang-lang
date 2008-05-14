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
    ; ("true", (fun i -> BOOLEAN(i,true)))
    ; ("false", (fun i -> BOOLEAN(i,false)))
    ; ("test", (fun i -> TEST i))
    ; ("match", (fun i -> MATCH i))
    ; ("with", (fun i -> WITH i))
    ; ("error", (fun i -> ERROR i))
    ; ("string", (fun i -> STRING i))
    ; ("regexp", (fun i -> REGEXP i))
    ; ("lens", (fun i -> LENS i))
    ; ("int", (fun i -> INT i))
    ; ("bool", (fun i -> BOOL i))
    ; ("canonizer", (fun i -> CANONIZER i))
    ; ("unit", (fun i -> UNIT i))
    ; ("type", (fun i -> TYPE i))
    ; ("of", (fun i -> OF i))
    ; ("into", (fun i -> INTO i))
    ; ("get", (fun i -> GET i))
    ; ("put", (fun i -> PUT i))
    ; ("create", (fun i -> CREATE i))
    ; ("where", (fun i -> WHERE i))
    ; ("forall", (fun i -> FORALL i))
    ]
}

let whitespace = [' ' '\t']+
let newline = "\n"
let uid_char = ['A'-'Z']
let id_char_first = ['a'-'z' 'A'-'Z' '\'' '_' '-' '@']
let id_char_rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '-' '@']
let int_char = ['0' - '9']
let string = '"' [^'"']* '"'

rule main = parse
| whitespace         { main lexbuf }
| "("                { LPAREN(info lexbuf) }
| ")"                { RPAREN(info lexbuf) }
| ";"                { SEMI(info lexbuf) }
| "."                { DOT(info lexbuf) }
| "&"                { AMPERSAND(info lexbuf) }
| "*"                { STAR(info lexbuf) }
| "-"                { MINUS(info lexbuf) }
| "_"                { UNDERLINE(info lexbuf) }
| "+"                { PLUS(info lexbuf) }
| "!"                { BANG(info lexbuf) }
| "->"               { ARROW(info lexbuf) }
| "=>"               { EQARROW(info lexbuf) }
| "<->"              { DARROW(info lexbuf) }
| "|"                { BAR(info lexbuf) }
| "="                { EQUAL(info lexbuf) }
| "{"                { LBRACE(info lexbuf) }
| "}"                { RBRACE(info lexbuf) }
| "#"                { HASH(info lexbuf) }
| "}["               { LLIST(info lexbuf) }
| "]"                { RBRACK(info lexbuf) }
| "["                { CSET(info lexbuf, cset lexbuf) }
| "[^"               { NSET(info lexbuf, cset lexbuf) }
| "<"                { LANGLE(info lexbuf) }
| "<<<"              { let i1 = info lexbuf in 
                       let i2,s = bare "" lexbuf in 
                       let i = Info.merge_inc i1 i2 in 
                       STR(i,s) }
| ">"                { RANGLE(info lexbuf) }
| ","                { COMMA(info lexbuf) }
| ":"                { COLON(info lexbuf) }
| "^"                { HAT(info lexbuf) }
| "~"                { TILDE(info lexbuf) }
| "/"                { let i1 = info lexbuf in 
                       let i2,s = rx_string "" lexbuf in 
                       let i = Info.merge_inc i1 i2 in 
                       RXSTR(i,s) }
| "\\"               { BACKSLASH(info lexbuf) }
| ","                { COMMA(info lexbuf) }
| "?"                { QMARK(info lexbuf) }
| "\""               { let i1 = info lexbuf in 
                       let i2,s = string "" lexbuf in 
                       let i = Info.merge_inc i1 i2 in 
                       STR(i,s) }

| '\'' (id_char_first id_char_rest* as ident) { 
    VIDENT(info lexbuf, ident)
  }
| id_char_first id_char_rest* as ident { 
      try let kw = Hashtbl.find keywords ident in
          kw (info lexbuf)
      with Not_found -> 
        if Char.uppercase ident.[0] = ident.[0] then 
          UIDENT (info lexbuf, ident)
        else 
          LIDENT (info lexbuf, ident) }
| (uid_char id_char_rest* ".")+ id_char_rest+ as qident {
    QIDENT(info lexbuf,qident)
  }
| int_char+ as integ { INTEGER(info lexbuf, int_of_string integ) }
| (int_char* "." int_char+) as flot 
                     { FLOAT(info lexbuf, float_of_string flot) } 
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

and rx_string acc = parse
  | "\\"        { let s = escape [("/","/")] lexbuf in 
                  rx_string (acc ^ s) lexbuf }
  | "/"         { (info lexbuf,acc) }
  | newline ([' ' '\t']* "|")? 
                { newline lexbuf;
                  rx_string (acc ^ "\n") lexbuf }
  | eof         { error lexbuf "unmatched '/'" }
  | _           { rx_string (acc ^ lexeme lexbuf) lexbuf }

and string acc = parse
| "\\"          { let s = escape [("\"","\"")] lexbuf in 
                  string (acc ^ s) lexbuf }
| "\""          { (info lexbuf,acc) }
| newline ([' ' '\t']* "|")? 
                { newline lexbuf; 
                  string (acc ^ "\n") lexbuf}
| eof           { error lexbuf "unmatched '\"'"}
| _             { string (acc ^ lexeme lexbuf) lexbuf }

and bare acc = parse
  | newline [' ']* { newline lexbuf; 
                     let s = lexeme lexbuf in 
                     let n = String.length s - 1 in 
                     bare_indent acc n lexbuf }
  | ">>>"          { (info lexbuf,acc) }
  | eof            { error lexbuf "unmatched '<<<'" }
  | _              { bare_raw (acc ^ lexeme lexbuf) lexbuf }

and bare_indent_spaces acc n expected = parse
  | [' ']            { if expected=0 then 
                         bare_indent (acc ^ lexeme lexbuf) n lexbuf
                       else 
                         bare_indent_spaces acc n (pred expected) lexbuf }
  (* XXX: should we check that the number of spaces is <= n? *)
  | newline [' ']* ">>>" { newline lexbuf;
                           (info lexbuf,acc) } 
  | newline          { newline lexbuf; 
                       bare_indent_spaces (acc ^ lexeme lexbuf) n n lexbuf }
  | _                { if expected=0 then 
                         bare_indent (acc ^ lexeme lexbuf) n lexbuf 
                       else
                         error lexbuf (sprintf "expecting %d spaces after newline in string" n) }

and bare_indent acc n = parse
  | newline [' ']* ">>>" { newline lexbuf; 
                           (info lexbuf,acc) }
  | newline          { newline lexbuf; 
                       bare_indent_spaces (acc ^ lexeme lexbuf) n n lexbuf }
  | eof              { error lexbuf "unmatched '>>>'" }
  | _                { bare_indent (acc ^ lexeme lexbuf) n lexbuf }
  
and bare_raw acc = parse
  | ">>>"       { (info lexbuf,acc) }
  | newline     { newline lexbuf; 
                  bare_raw (acc ^ lexeme lexbuf) lexbuf }
  | eof         { error lexbuf "unmatched '<<<'" }
  | _           { bare_raw (acc ^ lexeme lexbuf) lexbuf }

and escape el = parse
| "\\"          { "\\" }
| "'"           { "'" }
| "b"           { "\008" }
| "n"           { "\010" }
| "r"           { "\013" }
| "t"           { "\009" }
| _             { try Safelist.assoc (lexeme lexbuf) el 
                  with Not_found -> 
                    error lexbuf "in string escape sequence" }

and comment = parse
| "(*"             { comment lexbuf; comment lexbuf }
| "*)"             { () }
| newline          { newline lexbuf; comment lexbuf }
| eof              { error lexbuf "unmatched '(*'" }
| _                { comment lexbuf }
