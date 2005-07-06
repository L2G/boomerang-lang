{
  let lexeme = Lexing.lexeme
  let sprintf = Printf.sprintf 

  let emit s = print_string s
}

let whitespace = [' ' '\t' '\n']+
let opt_whitespace = [' ' '\t' '\n']*

rule xml = parse
  | whitespace                                { item lexbuf }
  | "<DT>" whitespace                         { item lexbuf }
and item = parse
  | whitespace                                { emit (lexeme lexbuf); item lexbuf }
  | "</A>" opt_whitespace "</DT>"             { emit "</A>"; item lexbuf }
  | "<DL>"                                    { emit "<DL><p>"; item lexbuf }
  | "</DL>"                                   { emit "\n</DL><p>"; skip_dt lexbuf }      
  | eof                                       { exit 0 } 
      
  | "<DT>" opt_whitespace                     { emit "<DT>"; item lexbuf }
  | _                                         { emit (lexeme lexbuf); item lexbuf }

and skip_dt = parse
  | opt_whitespace                            { emit (lexeme lexbuf); skip_dt2 lexbuf }
and skip_dt2 = parse
  | "</DT>"                                   { item lexbuf }

{
  let header =   "<!DOCTYPE NETSCAPE-Bookmark-file-1>"
               ^ "\n<!-- This is an automatically generated file."
               ^ "\n     It will be read and overwritten."
               ^ "\n     DO NOT EDIT! -->"
               ^ "\n<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">"
               ^ "\n<TITLE>Bookmarks</TITLE>"
               ^ "\n\n"

  let () = emit header; xml (Lexing.from_channel stdin)
}