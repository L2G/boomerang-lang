{
  open Cmdlineparser (* XXX fix filenames.. *)
}

let whitespace = [' ' '\t' '\n']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

let letter = lower | upper
(* let identifier = letter (letter | digit | '_')* *)
let filename_symbol = ['.' '-' '/' '+' '#']
let str = (letter | digit | '_' | filename_symbol)*

rule token = parse
    | whitespace              { token lexbuf }
    | "sync"                  { SYNC }
    | "show"                  { SHOW }
    | "updown"                { UPDOWN }
    | "upcreate"              { UPCREATE }
    | "identify"              { IDENTIFY }
    | "capabilities"          { CAPABILITIES }
    | "test"                  { TEST }
    | "as"                    { AS }
    | '@'                     { AT }
    | ':'                     { COLON }
    | ';'                     { SEMICOLON }
    | ','                     { COMMA }
    | '('                     { LPAREN }
    | ')'                     { RPAREN }
    | '['                     { LBRACK }
    | ']'                     { RBRACK }
    | str                     { STRING (Lexing.lexeme lexbuf) }
    | eof                     { EOF }
