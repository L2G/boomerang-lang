{

open Printf
exception Error of string

let verbose = ref true

let count = ref 0 (*!count is the number of keywords we added in these Hash tables *)
and countCard = ref 0 (*the nth Vcard we are seeing*)
and nb_space = ref 0

let nameTag = ref ""
and name = ref ""


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

let to_string = function
  | None -> ""
  | Some s -> sprintf "%s" s
} 


let motif = [^'\n'';'':']* 
let motif1 = [^'\n']* as motif1 
let motif2 = [^'\n']* as motif2
let motif3 = [^'\n']* as motif3
let blank = [' ''\t']
let ig = "=0D=0A=\n" 

let sentence = motif1? (ig blank* motif2)? (ig blank* motif3)? '\n'
let lastSentence= motif1? (ig blank* motif2)? (ig blank* motif3)? eof

    rule main = parse
| "BEGIN:" (motif as arg) '\n'
    {
     name := arg;
     countCard := !countCard +1;
     main lexbuf
   }
| blank* (motif as arg) ';'
    {
     let str = get_tag lexbuf in 
     let up_arg = String.uppercase arg in
     begin
       nb_space := 0;
       add up_arg str;
     end;
     main lexbuf
   }
| blank* "AGENT:"
    {
     in_agent lexbuf;
   }    
| blank* (motif as arg) ':'
    {
     let str = get_string lexbuf in 
     let up_arg = String.uppercase arg in
     begin
       nb_space := 0;
       add up_arg str;
     end;
     main lexbuf
   }
| blank* ('\n')* blank* "END:" (motif as arg) blank* '\n' 
| blank* "END:" (motif as arg) blank* eof
    {   
     if (Pervasives.compare arg !name) != 0 then
       raise (Error("unmatch BEGIN and END\n"))
     else if (!countCard) > 1 then
       begin
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
	 main lexbuf;
       end
     else ()
   }
(* | blank "END:" (motif as arg) blank eof  *)
(*     {    *)
(*      if (Pervasives.compare arg !name) != 0 then *)
(*        raise (Error("unmatch BEGIN and END\n")) *)
(*      else () *)
(*       } *)

and in_agent = parse 
| blank* ";EXTVAL:"
    {
     let str = get_string lexbuf in
     begin
       nb_space := 0;
       add "AGENT" ("<EXTVAL>"^str^"</EXTVAL>");
     end;
     main lexbuf;
   }
| ('\n')* blank*  "BEGIN:VCARD\n"
    {
     countCard := !countCard + 1;
     main lexbuf
   }

and get_tag = parse
| (motif as arg) ';'
    {
     nb_space := !nb_space + 2;
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       let str = get_tag lexbuf in
       let up_arg = String.uppercase arg in 
       (!s)^"<"^up_arg^">\n"^str^(!s)^"</"^up_arg^">\n"
     end
   }

| blank* "ENCODING=QUOTED-PRINTABLE:"
    {
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       let str = get_string1 lexbuf in
       str
     end
   }
| (motif as arg) ':'
    {
     nb_space := !nb_space + 2;
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       let str = get_string lexbuf in
       let up_arg = String.uppercase arg in
       (!s)^"<"^up_arg^">\n"^str^(!s)^"</"^up_arg^">\n"
     end
   }
    
and get_string = parse (*we clean up with get_string1 after*)
| sentence
| lastSentence
    {
     let str1 = to_string motif1
     and str2 = to_string motif2
     and str3 = to_string motif3 in
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       (!s)^" "^str1^" "^str2^" "^str3^"\n"
     end   
   }

and get_string1 = parse
| sentence
| lastSentence
    {
     let str1 = to_string motif1
     and str2 = to_string motif2
     and str3 = to_string motif3 in
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       (!s)^" "^str1^";"^str2^";"^str3^"\n"
     end   
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
    printf "<%s>\n" !name;
    for i=1 to !count do
      begin
	let s = Hashtbl.find hashNum i in
	let arg = Hashtbl.find hashTag s in
	printf "%s %s%s\n" ("<"^s^">\n") arg ("</"^s^">\n");
      end;
    done;
    printf "</%s>" !name;
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
