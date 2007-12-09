{

open Printf
exception Error of string

let verbose = ref true
let inside = ref false

let list_tag = ref []
let maintag = ref ""
let subject = ref ""
let count = ref 0
let space = ref ""

let rec print_list list= 
  match list with
    [] -> printf "\n"
  | hd :: tl -> printf "%s " hd; print_list tl 

let rec remove tag list =
  match list with
    [] -> raise (Error("unmatch the tag "^tag^"\n"))
  | hd::tl -> 
      if ((Pervasives.compare hd tag) = 0) then	tl
      else hd:: (remove tag tl)

let traite_opentag tag = 
  if !inside && (List.length (!list_tag) = 1) then printf " %s" ( (!space)^ (!maintag)); 
  list_tag := tag :: (!list_tag);
  printf ";%s" tag

let traite_closetag tag =
  list_tag := remove tag (!list_tag); 
  ()
} 

let tag = [^'<''>''/']*
let all = [^'\n''<''>']*
let blank = [' ''\t''\n']


    rule main = parse
| blank* '<' (tag as arg) '>' blank* (*change to "VCARD" because it's not really neccessary*)
    { count := !count + 1;
      subject := arg;
      space := "";
      for i= 0 to !count do space := !space ^ "  " done; 
      printf "%sBEGIN:%s\n" !space arg;
      in_main lexbuf
    }
| _ 
    {
     raise (Error("Expect the main tag of file .xml"))
   }

and in_main = parse
| blank* "<AGENT>" blank*
    {
     printf " %sAGENT:\n" !space; 
     main lexbuf
   }
| blank* "</AGENT>" blank*
    {
     in_main lexbuf
   }
| blank*  '<' (tag as arg) ">" blank*
    {
     inside := false;
     maintag := arg;
     list_tag := arg :: !list_tag;
     printf " %s" ((!space)^arg);
     intag lexbuf
   }
|blank* "</" (tag as arg) '>'
    {
     if (Pervasives.compare arg !subject) != 0 then
       raise(Error("</"^(!subject)^"> is expected in the place of </"^arg^">"))
     else printf "%sEND:%s\n" !space arg;
     count := !count - 1;
     space := "";
     for i= 0 to !count do space := !space ^ "  " done;
     if !count <> 0 then in_main lexbuf
   }
| eof {()}

and intag = parse
| blank* '<' (tag as arg) ">" blank*
    {
     traite_opentag arg;
     inside := true;
     intag lexbuf
   }
| blank* (all as str) ";;" blank*
| blank* (all as str) blank*
    {
     printf ":%s\n" str ;
     intag lexbuf
   }
| blank* "</" (tag as arg) '>' blank*
    {
     traite_closetag arg;
     if List.length (!list_tag) = 0 then in_main lexbuf else intag lexbuf
   }

(* and in_agent = parse *)
(* | blank* "<EXTVAL>" (all as str) "</EXTVAL>" *)
(*     { *)
(*      printf " %sAGENT;EXTVAL:%s\n" (!space) str; *)
(*      in_main lexbuf; *)
(*    } *)
(* | _ *)
(*     { *)
(*      printf " %sAGENT here:\n" !space; *)
(*      main lexbuf *)
(*    } *)
    

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
    | "-v" -> verbose := true
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
