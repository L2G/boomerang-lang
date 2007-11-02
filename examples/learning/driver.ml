let show_structure strings =
  let chunks = List.map Lex.parse_chunk strings in
  let s = Struct.discover chunks in
  let s' = Struct.refine s in
    print_endline ("Cost before refinement: " ^ (string_of_float (Struct.cost s)));
    print_endline (Struct.to_string s);
    print_endline ("Cost after refinement:  " ^ (string_of_float (Struct.cost s')));
    print_endline (Struct.to_string s');
    print_newline ()

let input_lines ic =
  let rec iter ic cs =
    try iter ic ((input_line ic)::cs)
    with End_of_file -> List.rev cs
  in
    iter ic []

(* given a list of files, a chunk_fun produces a list of strings --
   chunks for the lexer *)
type chunk_fun = in_channel list -> Lex.chunk list

let chunk_by_line ics =
  let chunks = List.map input_lines ics
  in
    List.concat chunks

let chunk_by_file ics =
  List.map 
    (fun ic -> 
       String.concat "\n" (input_lines ic))
    ics

open Arg

let chunk_fun : chunk_fun ref = ref chunk_by_line
let input_files : string list ref = ref []

let arg_spec = 
  [("--by-line", Unit (fun () -> chunk_fun := chunk_by_line), 
    "Chunk the given input file(s) by line (default)");
   
   ("--by-file", Unit (fun () -> chunk_fun := chunk_by_file), 
    "Chunk the input by file -- more than one file must be specified!")]

let arg_spec = align arg_spec
let parse_argv_input_files filename = input_files := filename::!input_files

let run () = 
  Arg.parse arg_spec parse_argv_input_files "";
  let ics = List.map open_in !input_files in
  let strings = !chunk_fun ics in
    show_structure strings

let _ = run ()
  

