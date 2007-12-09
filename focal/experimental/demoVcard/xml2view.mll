{

open Printf
exception Error of string

let verbose = ref true

let count = ref 0 (*!count is the number of keywords we added in these Hash tables *)
and countCard = ref 0 (*the nth Vcard we are seeing*)

let nameTag = ref ""

let hashNum : (int,string) Hashtbl.t = Hashtbl.create 13
(*the purpose of hashNum is that it restores the keywords in order *)
and hashTag : (string,string) Hashtbl.t = Hashtbl.create 13
(*keyword-value *)
and hashTagNum : (string, int) Hashtbl.t = Hashtbl.create 13
(* information: keyword and nth Vcard*)

let add arg str = 
  try 
    let nb = Hashtbl.find hashTagNum arg in
    if (nb = !countCard) then
      let s = Hashtbl.find hashTag arg in
      Hashtbl.replace hashTag arg (s^"\n"^str)
    else
      begin
	Hashtbl.add hashTag arg str;
	Hashtbl.add hashTagNum arg (!countCard)
      end
  with Not_found ->
    begin
      count := !count + 1;
      Hashtbl.add hashNum (!count) arg;
      Hashtbl.add hashTag arg str;
      Hashtbl.add hashTagNum arg (!countCard)
    end

let traite info =
  (*printf "in traite\n";*)
  Decomposition.analyse info

let to_string = function
  | None -> ""
  | Some s -> sprintf "%s" s
} 

let blank = [' ''\t''\n']*
let str = [^'<''>''/']*
let nonambigous = [^'<''>']*

    rule main = parse 
| blank "<VCARD" str ">" 
    { 
      countCard := !countCard +1;
      in_main lexbuf 
    }
| eof {()}

and in_main = parse
| blank "<" ("ADR" as tag) ">" 
| blank "<" ("LABEL" as tag) ">" 
| blank "<" ("TEL" as tag) ">" 
| blank "<" ("EMAIL" as tag) ">" 
    {
     nameTag := tag;
     let arg = in_tag lexbuf in
     let str = traite arg in
     add tag str;
     in_main lexbuf
   }
| blank "<AGENT>"
    {
     in_agent lexbuf
   }
| blank "<" (str as tag1) ">"
    {
     nameTag := tag1;
     let arg = in_tag lexbuf in 
     add !nameTag arg;
     in_main lexbuf
   }
| blank "</VCARD>" blank "</AGENT>" 
    { 
      let s = ref "<VCARD>\n" in
      for i=1 to !count do
	let name = Hashtbl.find hashNum i in
	let nb = Hashtbl.find hashTagNum name in
	if nb = !countCard then
	  begin
	    let str = Hashtbl.find hashTag name in
	    s := (!s) ^ "<"^name^">\n" ^ str ^ "</"^name^">\n";
	    Hashtbl.remove hashTag name;
	    Hashtbl.remove hashTagNum name;
	  end
      done;
      s := !s ^ "</VCARD>\n";
      countCard := !countCard - 1;
      add "AGENT" (!s);
      in_main lexbuf
    }
| blank "</VCARD>" blank eof 
    {()}

and in_agent = parse 
| blank "<EXTVAL>" (nonambigous as arg) "</EXTVAL>" blank "</AGENT>"
    {
     let s = "<EXTVAL>"^arg^"</EXTVAL>" in 
     add "AGENT" s;
     in_main lexbuf
   }
| blank "<VCARD>"
    {
     countCard := !countCard + 1;
     in_main lexbuf
   }

and in_tag = parse
| blank (nonambigous as arg) "<" (str as tag) ">"
    {
     let s = in_tag lexbuf in
     arg ^ "<" ^ tag ^ ">" ^ s
   }
| blank (nonambigous as arg) "</" (str as tag) ">"
    {
     if (Pervasives.compare tag !nameTag) != 0 then
       begin
	 let s = in_tag lexbuf in
	 arg ^ "</" ^ tag ^ ">\n" ^ s
       end
     else arg
   }
| blank "<" (str as tag) "/>"
    {
     let s = in_tag lexbuf in
     "<"^tag^"/>\n" ^ s
   }
| eof
    {
     raise (Error("in tag\n"))
   }


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
    printf "<VCARD>\n";
    for i=1 to !count do
      let tag = Hashtbl.find hashNum i in
      let str = Hashtbl.find hashTag tag in
      printf "<%s> %s </%s>\n\n" tag str tag;
    done;
    printf "</VCARD>";
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
