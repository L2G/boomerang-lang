{
  open Vcardparser

  open Lexing

  let c = ref 1

  let parse_error _ =
    failwith  (Printf.sprintf "parse error at characters %d-%d line %d"
		 (Parsing.symbol_start ()) (Parsing.symbol_end ()) (!c) )


}



let whitespace = [' ' '\t']

let eol = ['\n' '\r']

let str = [^ ';' ':' '=' ',']*

let charac = [ ^ ';' '}' '{' '[' ']' '=' ':'] 

let word = [^';' '}' '{' '[' ']' ' ' '\t' '\n' '\r' '=' ':']charac*


rule token = parse
  | "BEGIN:VCARD"                                       { BEGINVCARD }
  | "END:VCARD"                                         { ENDVCARD }
  | (word as x)                                         { ITEM (x) }
(*
  | "LOGO"                                              { LOGO }
  | "PHOTO"                                             { PHOTO }
  | "LABEL"                                             { LABEL }
  | "FN"                                                { FN }
  | "TITLE"                                             { TITLE }
  | "SOUND"                                             { SOUND }
  | "VERSION"                                           { VERSION }
  | "TEL"                                               { TEL }
  | "EMAIL"                                             { EMAIL }
  | "TZ"                                                { TZ }
  | "GEO"                                               { GEO }
  | "NOTE"                                              { NOTE }
  | "URL"                                               { URL }
  | "BDAY"                                              { BDAY }
  | "ROLE"                                              { ROLE }
  | "REV"                                               { REV }
  | "UID"                                               { UID }
  | "KEY"                                               { KEY }
  | "MAILER"                                            { MAILER }
  | "X-" (word as x)                                    { X (x) }
  | "ADR"                                               { ADR }
  | "ORG"                                               { ORG }
  | "N"                                                 { N }
  | "NICKNAME"                                          { NICKNAME }
  | "AGENT"                                             { AGENT }
*)
  | ':'                                                 { VAL (elt [] "" lexbuf) }
  | ';' (str as x)                                      { PARAM (x) }
  | '=' (str as x)                                      { PARAM (x) }
  | ',' (str as x)                                      { PARAM (x) }
  | "\n\r"                                              { incr c; token lexbuf }
  | eol                                                 { incr c; token lexbuf }
  | eof                                                 { EOF }
  | whitespace                                          { token lexbuf }
(*
  | _ as c                                              { VAL [String.make 1 c] }
 *)

and elt l s = parse
  | eol ' '                                    { incr c; elt l s lexbuf }
  | eol                                        { incr c; l@[s] }
  | ';'                                        { elt (l@[s]) "" lexbuf }
  | ':'                                        { elt (l@[s]) "" lexbuf }
  | _ as c                                     { elt l (s^(String.make 1 c)) lexbuf}





