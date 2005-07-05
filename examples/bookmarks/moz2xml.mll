{
  let lexeme = Lexing.lexeme
  let sprintf = Printf.sprintf 

  let emit s = print_string s
}

let whitespace = [' ' '\t' '\n']+

rule bookmarks = parse
  | "</TITLE>" whitespace                     { emit "<DT>"; item lexbuf }
  | _                                         { bookmarks lexbuf }
and item = parse
  | whitespace                                { emit (lexeme lexbuf); item lexbuf }
  | "<HR>" | "<HR>" whitespace | "<p>"        { item lexbuf }
  | "</A>"                                    { emit (sprintf "%s</DT>" (lexeme lexbuf)); item lexbuf }
  | "</DL>"                                   { emit (sprintf "%s</DT>" (lexeme lexbuf)); item lexbuf }
  | eof                                       { exit 0 }   
  | _                                         { emit (lexeme lexbuf); item lexbuf }

{
  let () = bookmarks (Lexing.from_channel stdin)
}
