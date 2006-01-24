{
  let lexeme = Lexing.lexeme
  let sprintf = Printf.sprintf 

  let outc = ref stdout
  let emit s = output_string (!outc) s
}

let whitespace = [' ' '\t' '\n']+

rule bookmarks = parse
  | "</TITLE>" whitespace                     { emit "<DT>"; item lexbuf }
  | _                                         { bookmarks lexbuf }

and item = parse
  | whitespace                                { emit (lexeme lexbuf); item lexbuf }
  | "<HR>" | "<HR>" whitespace                { emit ("<HR></HR>\n"); item lexbuf }
  | "<p>" | "<DD>" [^ '\n']+ '\n'             { item lexbuf }
  | "</A>"                                    { emit (sprintf "%s</DT>" (lexeme lexbuf)); item lexbuf }
  | "</DL>"                                   { emit (sprintf "%s</DT>" (lexeme lexbuf)); item lexbuf }
  | eof                                       { () }   
  | _                                         { emit (lexeme lexbuf); item lexbuf }

{
  let go inc outc' = 
    outc := outc';
    bookmarks (Lexing.from_channel inc)
}
