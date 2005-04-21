{
  open Ldapparser
   
  module LE = Lexing

  let lexeme = LE.lexeme
  let linestart = ref 0
  let lineno = ref 1
		 
  let newline lexbuf : unit = 
    linestart := LE.lexeme_start lexbuf;
    incr lineno
      
  let info lexbuf = 
    let c1 = LE.lexeme_start lexbuf in
    let c2 = LE.lexeme_end lexbuf in
    !lineno, c1 - !linestart, c2 - !linestart
    
  let error lexbuf s =
    let l,c1,c2 = info lexbuf in
    let t = lexeme lexbuf in
    let s = Printf.sprintf "%d:%d-%d: %s" l c1 c2 s in
    if t = "" then failwith s
  else failwith (s ^ ": " ^ t)
    
}

let number = [ '0' '1' '2' '3' '4' '5' '6' '7' '8' '9']+

let whitespace = [' ' '\t']

let eol = ['\n' '\r'] 

let str = [^ ';' ':' ]*

let word = [^ ';' ',' '=' ':' '\n' '\r' ' '] [ ^ ';' ',' '=' ':' '\n' '\r']*


rule token = parse
  | "version"                        { version lexbuf }
  | "dn"                             { DNVAL (dnval lexbuf,info lexbuf) }
  | (word as id)                     { word id lexbuf }
  | eol                              { newline lexbuf; token lexbuf}
  | eof                              { EOF(info lexbuf) }
  | whitespace                       { token lexbuf }
  | _                                { error lexbuf "Unknown token" }

and dnval = parse
  | "::"                   { ["ID",V.new_value (value "" lexbuf)] }
  | ':'                    { key lexbuf }
  | whitespace             { dnval lexbuf }
  | _                      { error lexbuf "Incorrect dn value" }

and key = parse
  | word as x              { keyval x lexbuf}
  | ',' (whitespace *) (word as k)  { keyval k lexbuf}
  | eol                    { [] }
  | whitespace             { key lexbuf }
  | _                      { error lexbuf "Incorrect dn specification" }

and keyval k = parse
  | '=' (whitespace *) (word as v) { [k,V.new_value v]@(key lexbuf) }
  | _                      { error lexbuf "No '=' found" }

and word id = parse
  | ':' (whitespace *)     { KWORD (id, value "" lexbuf) }
  | _                      { error lexbuf "No ':' found" }

and value s = parse
  | "\n "                  { newline lexbuf; value s lexbuf }
  | "\n"                   { s }
  | _ as ch                { value (s^(String.make 1 ch)) lexbuf }

and version = parse
  | whitespace             { version lexbuf }
  | ':'                    { numb lexbuf }
  | _                      { error lexbuf "Incorrect version specification" }

and numb = parse
  | number as i            { VERSION (i,info lexbuf) }
  | whitespace             { numb lexbuf }
  | _                      { error lexbuf "Incorrect version number" }



