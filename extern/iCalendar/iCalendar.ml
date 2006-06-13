let read get_char =
  let lex_func s n =
    let l = String.length s in
    let cur_pos = ref 0 in
    let nb_chars () = !cur_pos in
    let should_stop () = (n - !cur_pos) < 3 in (* enough to put \r\n c*)
    let store_string_char c =
      if !cur_pos >= l then failwith "string given by lexbuf is too short !"
      else begin
        String.unsafe_set s (!cur_pos) c;
        incr cur_pos
      end
    in
    let rec read_more () =
      if should_stop () then nb_chars ()
      else
        let c = get_char () in
        match c with
        | '\r' ->
            let c' = get_char () in
            if c' <> '\n' then failwith "\\r without \\n"
            else begin
              let c'' = 
                try 
                  Some (get_char ()) 
                with
                | End_of_file -> None
              in
              match c'' with
              | Some ' ' | Some '\t' -> read_more ()
              | Some c'' ->
                  store_string_char c;
                  store_string_char c';
                  store_string_char c'';
                  read_more ()
              | None ->
                  store_string_char c;
                  store_string_char c';
                  nb_chars ()
            end
        | '\n' ->
            begin
              let c'' = 
                try 
                  Some (get_char ()) 
                with
                | End_of_file -> None
              in
              match c'' with
              | Some ' ' | Some '\t' -> read_more ()
              | Some c'' ->
                  store_string_char c;
                  store_string_char c'';
                  read_more ()
              | None ->
                  store_string_char c;
                  nb_chars ()
            end
        | _ ->
            store_string_char c;
            read_more ()
     in 
     try
       read_more ()
     with
     | End_of_file -> nb_chars ()
  in

  let lexbuf = Lexing.from_function lex_func in
  ICalendarparse.icalendar ICalendarlex.line lexbuf

let line_size = 75
  
let write outc fullnl ic =
  let cur_pos = ref 0 in
  let f c =
    if c = '\n' then 
      begin
        if fullnl then output_char outc '\r';
        output_char outc c;
        cur_pos :=0
      end
    else if !cur_pos < line_size then 
      begin
        output_char outc c;
        incr cur_pos
      end
    else
      begin
        if fullnl then
          output_string outc "\r\n "
        else
          output_string outc "\n ";
        output_char outc c;
        cur_pos := 2
      end
  in
  ICalendar_print.print_icalendar (String.iter f) ic

let chars_from_str inc =
  fun () -> Pervasives.input_char inc

let read_file inc =
  let len = in_channel_length inc in
  let buf = String.create len in
  really_input inc buf 0 len;
  buf

let iCalReader inc outc =
  let s = Tree.string_of_t (Ical.view_from_icalendar (read (chars_from_str inc))) in
  Pervasives.output_string outc s
    
let iCalWriter inc outc =
  let ic =
    let s = read_file inc in
    let _ = Metal.setup "meta string" in
    let lexbuf = Lexing.from_string s in        
    let res = 
      try 
	(Metay.tree Metal.token lexbuf) 
      with Parsing.Parse_error -> 
	raise (Error.Harmony_error
		 (fun () -> Format.printf "%s: syntax error." 
		     (Info.string_of_t (Lexer.info lexbuf))))
    in
    let _ = Metal.finish () in
      Ical.icalendar_from_view res in
    write outc false ic
      
