{
  let lexeme = Lexing.lexeme
  let sprintf = Printf.sprintf 

  let outc = ref stdout

  let emit s = output_string (!outc) s
}

let whitespace = [' ' '\t' '\n']+
let opt_whitespace = [' ' '\t' '\n']*

rule xml = parse
  | whitespace                                { item lexbuf }
  | "<DT>" opt_whitespace                         { item lexbuf }
  | _                                         { emit "JUNK at start:"; emit (lexeme lexbuf);
                                                xml lexbuf }
and item = parse
  | whitespace                                { emit (lexeme lexbuf); item lexbuf }
  | "</A>" opt_whitespace "</DT>"             { emit "</A>"; item lexbuf }
  | "<DL>"                                    { emit "<DL><p>"; item lexbuf }
  | "</DL>" opt_whitespace                    { emit "\n</DL><p>"; skip_dt lexbuf }      
  | "<HR></HR>" opt_whitespace                { emit "\n<HR>\n"; item lexbuf }
  | eof                                       { () } 
  | "<DT>" opt_whitespace                     { emit "<DT>"; item lexbuf }
  | _                                         { emit (lexeme lexbuf); item lexbuf }

and skip_dt = parse
  | opt_whitespace                            { emit (lexeme lexbuf); skip_dt2 lexbuf }
  | "</DT>"                                   { item lexbuf }
  | _                                         { emit "JUNK:"; emit (lexeme lexbuf);
                                                skip_dt lexbuf }
and skip_dt2 = parse
  | "</DT>"                                   { item lexbuf }
  | _                                         { emit "JUNK2:"; emit (lexeme lexbuf);
                                                skip_dt lexbuf }

{
  let header =   "<!DOCTYPE NETSCAPE-Bookmark-file-1>"
               ^ "\n<!-- This is an automatically generated file."
               ^ "\n     It will be read and overwritten."
               ^ "\n     DO NOT EDIT! -->"
               ^ "\n<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">"
               ^ "\n<TITLE>Bookmarks</TITLE>"
               ^ "\n\n"

  let go inc outc' = 
    outc:= outc'; 
    emit header; 
    xml (Lexing.from_channel inc)
}
