{
open Parser

module LE = Lexing

let debug = Trace.debug "lexer"

let lexeme = LE.lexeme

type f_info = (string * int * int)
let info_stk = ref []

let filename () = match !info_stk with 
    [] -> raise (Error.Fatal_error("Lexer.filename : info stack is empty."))
  | (fn,_,_)::_ -> fn
      
let lineno () = match !info_stk with 
    [] -> raise (Error.Fatal_error("Lexer.lineno : info stack is empty."))
  | (_,l,_)::_ -> l

let linestart () = match !info_stk with 
    [] -> raise (Error.Fatal_error("Lexer.linestart : info stack is empty."))
  | (_,_,c)::_ -> c

let set_filename fn = match !info_stk with 
    [] -> raise (Error.Fatal_error("Lexer.set_filename : info stack is empty."))
  | (_,l,c)::t -> info_stk := (fn,l,c)::t
      
let set_lineno l = match !info_stk with 
    [] -> raise (Error.Fatal_error("Lexer.set_lineno : info stack is empty."))
  | (fn,_,c)::t -> info_stk := (fn,l,c)::t
      
let set_linestart c = match !info_stk with 
    [] -> raise (Error.Fatal_error("Lexer.set_linestart : info stack is empty."))
  | (fn,l,_)::t -> info_stk := (fn,l,c)::t

let setup fn = info_stk := (fn,1,0)::!info_stk

let finish () = match !info_stk with
    [] -> raise (Error.Fatal_error("Lexer.finish : info stack is empty."))
  | _::t -> info_stk := t
      
let newline lexbuf : unit = 
  set_linestart (LE.lexeme_start lexbuf);
  set_lineno (lineno () + 1)

let info lexbuf : Info.t = 
  let c1 = LE.lexeme_start lexbuf in
  let c2 = LE.lexeme_end lexbuf in
  let l = lineno () in
  let c = linestart () in
    (l, c1 - c - 1),(l, c2 - c - 1)
      
let error lexbuf msg =
  let i = info lexbuf in
  let t = lexeme lexbuf in   
  let s = Printf.sprintf "Lexing error: %s : %s" msg t in
    raise (Error.Compile_error(i, filename (), s))

let text = Lexing.lexeme

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))
    
(* dictionary of Focal keywords *)
let keywords = Hashtbl.create 17
let _ = 
  Safelist.iter 
    (fun (kw,tok) -> Hashtbl.add keywords kw tok )
    [ "let", (fun i -> LET i)
    ; "in", (fun i -> IN i)
    ; "fun", (fun i -> FUN i)
    ; "and", (fun i -> AND i)
    ; "module", (fun i -> MODULE i)
    ; "end", (fun i -> END i)
    ; "begin", (fun i -> BEGIN i)
    ; "open", (fun i -> OPEN i)
    ; "type", (fun i -> TYPE i)
    ; "lens", (fun i -> LENS i)
    ; "view", (fun i -> VIEW i)
    ; "name", (fun i -> NAME i)
    ; "test", (fun i -> TEST i)
    ; "sync", (fun i -> SYNC i)
    ; "with", (fun i -> WITH i)
    ; "at", (fun i -> AT i)
    ; "error", (fun i -> ERROR i)
    ; "missing", (fun i -> MISSING i)
    ]
}

let whitespace = [' ' '\t']+
let newline = "\n"
let id_char = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']
let string = '"' [^'"']* '"'

rule main = parse
| whitespace        { main lexbuf }
| "->"              { ARROW (info lexbuf) }
| "=>"              { DOUBLEARROW (info lexbuf) }
| "<"               { LANGLE (info lexbuf) }
| ">"               { RANGLE (info lexbuf) }
| "("               { LPAREN (info lexbuf) }
| ")"               { RPAREN (info lexbuf) }
| ";"               { SEMI (info lexbuf) }
| "."               { DOT (info lexbuf) }
| "*"               { STAR (info lexbuf) }
| "!"               { BANG (info lexbuf) }
| "|"               { BAR (info lexbuf) }
| "~"               { TILDE (info lexbuf) }
| "="	            { EQUAL (info lexbuf) }
| "{"	            { LBRACE (info lexbuf) }
| "}"	            { RBRACE (info lexbuf) }
| "["               { LBRACK (info lexbuf) }
| "]"	    	    { RBRACK (info lexbuf) }
| ","               { COMMA (info lexbuf) }
| ":"               { COLON (info lexbuf) }
| "\\"              { BACKSLASH (info lexbuf) }
| "/"               { SLASH (info lexbuf) }
| ","               { COMMA (info lexbuf) }
| ":"               { COLON (info lexbuf) }
| "`"               { BACKTICK (info lexbuf) }
| "\""              { STRING (Syntax.mk_id (info lexbuf) (string lexbuf)) }
| id_char+ as ident { try 
                        let kw = Hashtbl.find keywords ident in
                          kw (info lexbuf)
                      with Not_found ->
                        IDENT (Syntax.mk_id (info lexbuf) ident) }
| newline           { newline lexbuf; main lexbuf }
| eof		    { EOF (info lexbuf) } 
| "#line " ['0'-'9']+  { set_lineno (extractLineno (text lexbuf) 6 - 1); getFile lexbuf }
| "(*"              { comment lexbuf; main lexbuf }
| _		    { error lexbuf "unknown token" }

and string = parse
| "\\"		{ let s = escape lexbuf in s ^ string lexbuf }
| "\""		{ "" }
| newline [' ' '\t']* "|" { newline lexbuf; "\n" ^ string lexbuf }
| newline	{ newline lexbuf; let s = lexeme lexbuf in s ^ string lexbuf }
| _		{ let s = lexeme lexbuf in s ^ string lexbuf }
| eof		{ error lexbuf "unmatched '\"'"}

and escape = parse
| "'"		{ "'" }
| "\""		{ "\"" }
| "\\"		{ "\\" }
| "b"		{ "\008" }
| "n"		{ "\010" }
| "r"		{ "\013" }
| "t"		{ "\009" }
| eof		{ error lexbuf "EOF when reading escape" }
| _		{ error lexbuf "unknown escape" }

and comment = parse
| "(*"             { comment lexbuf; comment lexbuf }
| "*)"             { () }
| newline          { newline lexbuf; comment lexbuf }
| eof		   { error lexbuf "unmatched '(*'" }
| _                { comment lexbuf }

and getFile = parse
  " "* "\"" { getName lexbuf }

and getName = parse
  [^ '"' '\n']+ { set_filename (text lexbuf); finishName lexbuf }

and finishName = parse
  '"' [^ '\n']* { main lexbuf }

