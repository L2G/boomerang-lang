(* csv.ml - comma separated values parser
 *
 * $Id: csv.ml,v 1.5 2005/02/17 15:51:47 rich Exp $
 *)

(* The format of CSV files:
 * 
 * Each field starts with either a double quote char or some other
 * char. For the some other char case things are simple: just read up
 * to the next comma (,) which marks the end of the field.
 * 
 * In the case where a field begins with a double quote char the
 * parsing rules are different. Any double quotes are doubled ("") and
 * we finish reading when we reach an undoubled quote. eg: "The
 * following is a quote: "", and that's all" is the CSV equivalent of
 * the following literal field: The following is a quote: ", and that's
 * all
 *
 * "0 is the quoted form of ASCII NUL.
 * 
 * CSV fields can also contain literal carriage return characters, if
 * they are quoted, eg: "This field
 * is split over lines" represents a
 * single field containing a \n.
 * 
 * Excel will only use the quoting format if a field contains a double
 * quote or comma, although there's no reason why Excel couldn't always
 * use the quoted format.
 * 
 * The practical upshot of this is that you can't split a line in a CSV
 * file just by looking at the commas. You need to parse each field
 * separately.
 * 
 * How we represent CSV files:
 * 
 * We load in the whole CSV file at once, and store it internally as a
 * 'string list list' type (note that each line in the CSV file can,
 * and often will, have different lengths). We then provide simple
 * functions to read the CSV file line-by-line, copy it out, or copy a
 * subset of it into a matrix.
 *)

type t = string list list

exception Bad_CSV_file of string

let rec dropwhile f = function
  | [] -> []
  | x :: xs when f x -> dropwhile f xs
  | xs -> xs

let lines = List.length

let columns csv =
  List.fold_left max 0 (List.map List.length csv)

type state_t = StartField
	       | InUnquotedField
	       | InQuotedField
	       | InQuotedFieldAfterQuote

let load_rows ?(separator = ',') f chan =
  let row = ref [] in			(* Current row. *)
  let field = ref [] in			(* Current field. *)
  let state = ref StartField in		(* Current state. *)
  let end_of_field () =
    let field_list = List.rev !field in
    let field_len = List.length field_list in
    let field_str = String.create field_len in
    let rec loop i = function
	[] -> ()
      | x :: xs ->
	  field_str.[i] <- x;
	  loop (i+1) xs
    in
    loop 0 field_list;
    row := field_str :: !row;
    field := [];
    state := StartField
  in
  let empty_field () =
    row := "" :: !row;
    field := [];
    state := StartField
  in
  let end_of_row () =
    let row_list = List.rev !row in
    f row_list;
    row := [];
    state := StartField
  in
  let rec loop () =
    let c = input_char chan in
    if c != '\r' then (			(* Always ignore \r characters. *)
      match !state with
	  StartField ->			(* Expecting quote or other char. *)
	    if c = '\"' then (
	      state := InQuotedField;
	      field := []
	    ) else if c = separator then (* Empty field. *)
	      empty_field ()
	    else if c = '\n' then (	(* Empty field, end of row. *)
	      empty_field ();
	      end_of_row ()
	    ) else (
	      state := InUnquotedField;
	      field := [c]
	    )
	| InUnquotedField ->		(* Reading chars to end of field. *)
	    if c = separator then	(* End of field. *)
	      end_of_field ()
	    else if c = '\n' then (	(* End of field and end of row. *)
	      end_of_field ();
	      end_of_row ()
	    ) else
	      field := c :: !field
	| InQuotedField ->		(* Reading chars to end of field. *)
	    if c = '\"' then
	      state := InQuotedFieldAfterQuote
	    else
	      field := c :: !field
	| InQuotedFieldAfterQuote ->
	    if c = '\"' then (		(* Doubled quote. *)
	      field := c :: !field;
	      state := InQuotedField
	    ) else if c = '0' then (	(* Quote-0 is ASCII NUL. *)
	      field := '\000' :: !field;
	      state := InQuotedField
	    ) else if c = separator then (* End of field. *)
	      end_of_field ()
	    else if c = '\n' then (	(* End of field and end of row. *)
	      end_of_field ();
	      end_of_row ()
	    )
    ); (* end of match *)
    loop ()
  in
  try
    loop ()
  with
      End_of_file ->
	(* Any part left to write out? *)
	(match !state with
	     StartField ->
	       if !row <> [] then
		 ( empty_field (); end_of_row () )
	   | InUnquotedField | InQuotedFieldAfterQuote ->
	       end_of_field (); end_of_row ()
	   | InQuotedField ->
	       raise (Bad_CSV_file "Missing end quote after quoted field.")
	)

let load_in ?separator chan =
  let csv = ref [] in
  let f row =
    csv := row :: !csv
  in
  load_rows ?separator f chan;
  List.rev !csv

let load ?separator filename =
  let chan = open_in filename in
  let csv = load_in ?separator chan in
  close_in chan;
  csv 

let trim ?(top=true) ?(left=true) ?(right=true) ?(bottom=true) csv =
  let rec empty_row = function
    | [] -> true
    | x :: xs when x <> "" -> false
    | x :: xs -> empty_row xs
  in
  let csv = if top then dropwhile empty_row csv else csv in
  let csv =
    if right then
      List.map (fun row ->
		  let row = List.rev row in
		  let row = dropwhile ((=) "") row in
		  let row = List.rev row in
		  row) csv
    else csv in
  let csv =
    if bottom then (
      let csv = List.rev csv in
      let csv = dropwhile empty_row csv in
      let csv = List.rev csv in
      csv
    ) else csv in

  let empty_left_cell =
    function [] -> true | x :: xs when x = "" -> true | _ -> false in
  let empty_left_col =
    List.fold_left (fun a row -> a && empty_left_cell row) true in
  let remove_left_col =
    List.map (function [] -> [] | x :: xs -> xs) in
  let rec loop csv =
    if empty_left_col csv then (
      let csv = remove_left_col csv in
      loop csv
    ) else csv
  in

  let csv = if left then loop csv else csv in

  csv

let square csv =
  let columns = columns csv in
  List.map (
    fun row ->
      let n = List.length row in
      let row = List.rev row in
      let rec loop acc = function
	| 0 -> acc
	| i -> "" :: loop acc (i-1)
      in
      let row = loop row (columns - n) in
      List.rev row
  ) csv

let associate header data =
  let nr_cols = List.length header in
  let rec trunc = function
    | 0, _ -> []
    | n, [] -> "" :: trunc (n-1, [])
    | n, (x :: xs) -> x :: trunc (n-1, xs)
  in
  List.map (
    fun row ->
      let row = trunc (nr_cols, row) in
      List.combine header row
  ) data

let save_out ?(separator = ',') chan csv =
  (* Quote a single CSV field. *)
  let quote_field field =
    if String.contains field separator ||
      String.contains field '\"' ||
      String.contains field '\n'
    then (
      let buffer = Buffer.create 100 in
      Buffer.add_char buffer '\"';
      for i = 0 to (String.length field) - 1 do
	match field.[i] with
            '\"' -> Buffer.add_string buffer "\"\""
	  | c    -> Buffer.add_char buffer c
      done;
      Buffer.add_char buffer '\"';
      Buffer.contents buffer
    )
    else
      field
  in

  let separator = String.make 1 separator in
  List.iter (fun line ->
	       output_string chan (String.concat separator
				     (List.map quote_field line));
	       output_char chan '\n') csv

let print ?separator csv =
  save_out ?separator stdout csv; flush stdout

let save ?separator file csv =
  let chan = open_out file in
  save_out ?separator chan csv;
  close_out chan

let save_out_readable chan csv =
  (* Escape all the strings in the CSV file first. *)
  let csv = List.map (List.map String.escaped) csv in

  let csv = square csv in

  (* Find the width of each column. *)
  let widths =
    match csv with
      | [] -> []
      | r :: _ ->
	  let n = List.length r in
	  let lengths = List.map (List.map String.length) csv in
	  let max2rows r1 r2 =
	    let rp = List.combine r1 r2 in
	    List.map (fun ((a : int), (b : int)) -> max a b) rp
	  in
	  let rec repeat x = function
	    | 0 -> []
	    | i -> x :: repeat x (i-1)
	  in
	  List.fold_left max2rows (repeat 0 n) lengths in

  (* Print out each cell at the correct width. *)
  let rec repeat f = function
    | 0 -> ()
    | i -> f (); repeat f (i-1)
  in
  List.iter (
    fun row ->
      let row = List.combine widths row in
      List.iter (
	fun (width, cell) ->
	  output_string chan cell;
	  let n = String.length cell in
	  repeat (fun () -> output_char chan ' ') (width - n + 1)
      ) row;
      output_char chan '\n'
  ) csv

let print_readable = save_out_readable stdout