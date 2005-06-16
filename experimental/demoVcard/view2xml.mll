{
open Printf
exception Error of string
} 

rule main = parse
| eof {()}
|  "<" ("HOME" as arg) ">"
|  "<" ("WORK" as arg) ">" 
|  "<" ("POSTAL" as arg) ">" 
|  "<" ("PARCEL" as arg) ">" 
|  "<" ("DOM" as arg) ">" 
|  "<" ("INTL" as arg) ">" 
|  "<" ("PREF" as arg) ">" 
|  "<" ("VOICE" as arg) ">" 
|  "<" ("FAX" as arg) ">" 
|  "<" ("PAGER" as arg) ">" 
|  "<" ("MSG" as arg) ">" 
|  "<" ("CELL" as arg) ">" 
|  "<" ("VIDEO" as arg) ">"
|  "<" ("BBS" as arg) ">"
|  "<" ("MODEM" as arg) ">" 
|  "<" ("ISDN" as arg) ">" 
|  "<" ("PCS" as arg) ">" 
|  "<" ("INTERNET" as arg) ">" 
|  "<" ("X400" as arg) ">" 
    { 
      printf "<%s/>" arg; 
      main lexbuf 
    } 
|  "</" ("HOME" as arg) ">"
|  "</" ("WORK" as arg) ">" 
|  "</" ("POSTAL" as arg) ">" 
|  "</" ("PARCEL" as arg) ">" 
|  "</" ("DOM" as arg) ">" 
|  "</" ("INTL" as arg) ">" 
|  "</" ("PREF" as arg) ">" 
|  "</" ("VOICE" as arg) ">" 
|  "</" ("FAX" as arg) ">" 
|  "</" ("PAGER" as arg) ">" 
|  "</" ("MSG" as arg) ">" 
|  "</" ("CELL" as arg) ">" 
|  "</" ("VIDEO" as arg) ">"
|  "</" ("BBS" as arg) ">"
|  "</" ("MODEM" as arg) ">" 
|  "</" ("ISDN" as arg) ">" 
|  "</" ("PCS" as arg) ">" 
|  "</" ("INTERNET" as arg) ">" 
|  "</" ("X400" as arg) ">"
    { 
      main lexbuf
    }
| _ as c 
    { print_char c; 
      main lexbuf}


    {
(* Programme principal, ˆ conserver *)

let prerr_error filename lexbuf exc =
  let msg = match exc with
  | Error s -> s
  | _ -> "Exception non rattrapŽe: " ^ Printexc.to_string exc in
  let pos1 = Lexing.lexeme_start lexbuf  in
  let pos2 = Lexing.lexeme_end lexbuf  in
  prerr_endline
    ("File \""^filename^"\", line 1, characters "^
     string_of_int pos1^"-"^string_of_int pos2^
     "\nLexme ``"^Lexing.lexeme lexbuf^"'', "^msg)

let main filename =
  let chan =
    try open_in filename with
    | Sys_error s -> begin
        prerr_endline s ;
        exit 2
    end in
  
  let lexbuf = Lexing.from_channel chan in
  try
    main lexbuf ;
    flush stdout
  with
  | exc -> begin
      prerr_error filename lexbuf exc ;
      exit 2
  end

let usage () =
  prerr_endline ("Usage: "^Sys.argv.(0)^" [-v] filename") ;
  exit 2

let _ =
  let name = ref None in
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
       | s    ->
	if String.length s > 0 && s.[0] <> '-' then
	  name := Some s
	else
	  usage ()
  done ;
  match !name with
  | None ->
      usage ()
  | Some s ->
      main s

}
