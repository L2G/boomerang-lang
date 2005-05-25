{

open Printf
exception Error of string

let verbose = ref true

(* let traite info =  *)
(*    if !verbose then *)
(*      printf "%s\n" (Decomposition.analyse info)    *)
(*    else printf "%s" info *)

let count = ref 1
and nb_space = ref 0

let hashNum : (int,string) Hashtbl.t = Hashtbl.create 13
and hashTag : (string,string) Hashtbl.t = Hashtbl.create 13


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
| blank* (motif as arg) ';'
    {
     let str = get_tag lexbuf in 
     begin
       nb_space := 0;
       try
	 let s = Hashtbl.find hashTag arg in
	 Hashtbl.replace hashTag arg (s^str);
       with Not_found -> 
	 begin
      	   Hashtbl.add hashNum !count arg;
           Hashtbl.add hashTag arg str;
           count := !count +1;
	 end;
     end;
     main lexbuf
   }
| blank* (motif as arg) ':'
    {
     let str = get_string lexbuf in 
     begin
       nb_space := 0;
       try
	 let s = Hashtbl.find hashTag arg in
	 Hashtbl.replace hashTag arg (s^str)
       with Not_found -> 
	 begin
	   Hashtbl.add hashNum !count arg;
	   Hashtbl.add hashTag arg str;
	   count := !count +1;
	 end;
     end;
     main lexbuf
   }
| eof 
    {() }

and get_tag = parse
| (motif as arg) ';'
    {
     nb_space := !nb_space + 2;
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       let str = get_tag lexbuf in
       (!s)^"<"^arg^">\n"^str^(!s)^"</"^arg^">\n"
     end
   }
| (motif as arg) ':'
    {
     nb_space := !nb_space + 2;
     let s = ref "" in
     begin
       for i=1 to !nb_space do s := !s^" " done;
       let str = get_string lexbuf in
       (!s)^"<"^arg^">\n"^str^(!s)^"</"^arg^">\n"
     end
   }
    
and get_string = parse
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
    for i=1 to !count-1 do
      begin
	let s = Hashtbl.find hashNum i in
	let arg = Hashtbl.find hashTag s in
	printf "%s %s%s\n" ("<"^s^">\n") arg ("</"^s^">\n");
      end;
    done;
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
