{
module LE = Lexing
let lexeme = LE.lexeme
let sprintf = Printf.sprintf
let buf = Buffer.create 17 
let (=+) b s = Buffer.add_string b s
}

let newline = "\n"
let de = "DE   "
let line = [^'\n']*

rule main = shortest
  | de               { buf =+ (lexeme lexbuf); merge_de lexbuf }
  | line newline     { buf =+ (lexeme lexbuf); main lexbuf }
  | eof              { () }

and merge_de = parse 
  | (line as l) newline de   { buf =+ l; buf =+ " "; merge_de lexbuf }
  | line newline             { buf =+ (lexeme lexbuf); main lexbuf }
      
{
  let () = 
    if Array.length Sys.argv <> 2 then 
      (Printf.eprintf "usage: prepro [file]\n";
       exit 1);
    let fn = Sys.argv.(1) in 
    let fchan = open_in_bin fn in
    let _ = main (Lexing.from_channel fchan) in 
    let _ = close_in fchan in 
      Printf.printf "%s" (Buffer.contents buf)
}
