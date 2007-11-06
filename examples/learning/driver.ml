let show_structure strings =
  let chunks = List.map Lex.parse_chunk strings in
  let s = Struct.discover chunks in
  let s' = Struct.refine s in
    print_endline ("Cost before refinement: " ^ (string_of_float (Struct.cost s)));
    print_endline (Struct.to_string s);
    print_endline ("Cost after refinement:  " ^ (string_of_float (Struct.cost s')));
    print_endline (Struct.to_string s');
    print_newline ()

let lens_as_string strings mod_name lines_name =
  let chunks = List.map Lex.parse_chunk strings in
  let s = Struct.discover chunks in
  let s' = Struct.refine s in
    (* TODO specify lens name *)
  let l_module = Lenses.struct_to_module s' mod_name "l" lines_name in 
    Util.format_to_string (fun () -> Bsyntax.format_module l_module)

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
let output_file : string option ref = ref None

let arg_spec = 
  [("--by-line", Unit (fun () -> chunk_fun := chunk_by_line), 
    "Chunk the given input file(s) by line (default)");
   
   ("--by-file", Unit (fun () -> chunk_fun := chunk_by_file), 
    "Chunk the input by file -- more than one file must be specified!");

   ("-o", String (fun f -> output_file := Some f), "Where to output the lens source (defaults to STDOUT)")]

let arg_spec = align arg_spec
let parse_argv_input_files filename = 
  input_files := filename::!input_files

let usage_msg = "learn [OPTIONS] [files ...]"

let run () = 
  Arg.parse arg_spec parse_argv_input_files usage_msg;
  if !input_files = []
  then Arg.usage arg_spec usage_msg
  else
    let ics = List.map open_in !input_files in
    let strings = !chunk_fun ics in
    let get_lens mod_name =
      lens_as_string strings mod_name (if !chunk_fun == chunk_by_line
				       then Some "lines"
				       else None)
    in
      match !output_file with
	  None -> print_string (get_lens "learned")
	| Some f ->
	    let oc = open_out f in
	      output_string oc (get_lens (Filename.chop_extension f));
	      close_out oc

let _ = run ()
  
