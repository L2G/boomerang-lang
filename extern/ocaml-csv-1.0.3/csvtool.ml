(* Handy tool for managing CSV files.
 * $Id: csvtool.ml,v 1.1 2005/05/24 13:52:50 rich Exp $
 *)

open Printf
open Csv

let cmd_cols ~separator ~csv ~chan cols =
  let cols = List.map int_of_string cols in

  let output = List.map (
    fun row ->
      let n = List.length row in
      let row = List.map (
	fun col_wanted ->
	  if 0 <= col_wanted && col_wanted < n then
	    List.nth row col_wanted
	  else
	    ""
      ) cols in
      row
  ) csv in
  save_out ~separator chan output

let cmd_namedcols ~separator ~csv ~chan names =
  let header, data =
    match csv with
      | [] -> failwith "no rows in this CSV file"
      | h :: t -> h, t in
  let data = associate header data in
  let data = List.map (
    fun row -> List.map (fun name -> List.assoc name row) names
  ) data in
  save_out ~separator chan data

let cmd_width ~csv ~chan () =
  fprintf chan "%d\n" (columns csv)

let cmd_height ~csv ~chan () =
  fprintf chan "%d\n" (lines csv)

let cmd_readable ~csv ~chan () =
  save_out_readable chan csv

(* Process the arguments. *)
let usage =
  "csvtool - Copyright (C) 2005 Richard W.M. Jones, Merjis Ltd.

csvtool is a tool for performing manipulations on CSV files from shell scripts.

Summary:
  csvtool [-options] command [command-args] < input.csv

Commands:
  col [col1] [col2] ...
    Return one or more columns from the CSV file.  Columns are numbered
    starting from zero.

  namedcol [name1] [name2] ...
    Assuming the first row of the CSV file is a list of column headings,
    this returned the column(s) with the named headings.

  width
    Return the maximum width of the CSV file (number of columns in the
    widest row).

  height
    Return the number of rows in the CSV file.

  readable
    Print the input CSV in a readable format.

Input and output files:
  csvtool normally processes its input from stdin and writes its output
  to stdout.  Use the -i and -o options to override this behaviour.

Options:"

let () =
  let input_sep = ref ',' in
  let set_input_sep = function
    | "TAB" -> input_sep := '\t'
    | "COMMA" -> input_sep := ','
    | s -> input_sep := s.[0]
  in

  let output_sep = ref ',' in
  let set_output_sep = function
    | "TAB" -> output_sep := '\t'
    | "COMMA" -> output_sep := ','
    | s -> output_sep := s.[0]
  in

  let input_file = ref "" in
  let output_file = ref "" in

  let argspec = [
    "-t", Arg.String set_input_sep,
    "Input separator char.  Use -t TAB for tab separated input.";
    "-u", Arg.String set_output_sep,
    "Output separator char.  Use -t TAB for tab separated output.";
    "-i", Arg.Set_string input_file,
    "Read CSV input from file (instead of stdin)";
    "-o", Arg.Set_string output_file,
    "Write output to file (instead of stdout)"
  ] in

  let rest = ref [] in
  let set_rest str =
    rest := str :: !rest
  in

  Arg.parse argspec set_rest usage;

  let input_sep = !input_sep in
  let output_sep = !output_sep in
  let input_file = !input_file in
  let output_file = !output_file in
  let rest = List.rev !rest in

  let cmd, args =
    match rest with
      | [] -> prerr_endline (Sys.executable_name ^ " --help for usage"); exit 1
      | h :: t -> h, t in

  (* Read the input file. *)
  let input =
    if input_file <> "" then load ~separator:input_sep input_file
    else load_in ~separator:input_sep stdin in

  (* Set up the output file. *)
  let chan =
    if output_file <> "" then open_out output_file
    else stdout in

  (match cmd with
     | "col" | "cols" ->
	 cmd_cols ~separator:output_sep ~csv:input ~chan args
     | "namedcol" | "namedcols" ->
	 cmd_namedcols ~separator:output_sep ~csv:input ~chan args
     | "width" ->
	 cmd_width ~csv:input ~chan ()
     | "height" ->
	 cmd_height ~csv:input ~chan ()
     | "readable" ->
	 cmd_readable ~csv:input ~chan ()
     | _ -> prerr_endline (Sys.executable_name ^ " --help for usage")
  );

  if output_file <> "" then close_out chan
