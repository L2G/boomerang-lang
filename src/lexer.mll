{
open Parser

module LE = Lexing

let lexeme = LE.lexeme
let linestart = ref 0
let lineno = ref 1
let file_name = ref ""

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

let error lexbuf msg =
  let i = info lexbuf in
  let t = lexeme lexbuf in   
  let s = Printf.sprintf "%s : %s" msg t in
    raise (Error.Syntax_error(i, !file_name, s))

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
| "\""              { STRING ((info lexbuf), (string lexbuf)) }
| id_char+ as ident { try 
                        let kw = Hashtbl.find keywords ident in
                          kw (info lexbuf)
                      with Not_found ->
                        IDENT (info lexbuf, ident) }
| newline           { newline lexbuf; main lexbuf }
| eof		    { EOF (info lexbuf) } 
| "(*"              { comment lexbuf; main lexbuf }
| _		    { error lexbuf "Unknown token" }

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
