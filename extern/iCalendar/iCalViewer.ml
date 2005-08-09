
(******************************************************************************)
(*                              iCalendar viewer                              *)
(******************************************************************************)

let try_finalize f x e y =
  try (let r = f x in e y; r)
  with exn -> e y; raise exn

let chars_from_str inc =
  fun () -> Pervasives.input_char inc

let read_file inc =
  let len = in_channel_length inc in
  let buf = String.create len in
  really_input inc buf 0 len;
  buf

let iCalReader inc outc =
  let s = V.string_of_t (Ical.view_from_icalendar (ICalendar.read (chars_from_str inc))) in
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
  ICalendar.write outc false ic

let usage = "Usage : [iCalViewer ascal src dest] converts the ics calendar in src"
  ^ " to a meta view in dest\n\t [iCalViewer asmeta src dest] does the opposite\n"

let _ =
  match Array.length Sys.argv with
    4 -> begin
      match Sys.argv.(1) with
	"ascal" -> 
	  let doit () =
	    let inc = open_in Sys.argv.(2) in
	    let outc = open_out Sys.argv.(3) in
	    let close () = close_in inc; close_out outc in
	    try_finalize (iCalReader inc) outc close () in
	  Unix.handle_unix_error doit ()
      |	"asmeta" -> 
	  let doit () =
	    let inc = open_in Sys.argv.(2) in
	    let outc = open_out Sys.argv.(3) in
	    let close () = close_in inc; close_out outc in
	    try_finalize (iCalWriter inc) outc close () in
	  Unix.handle_unix_error doit ()
      |	_ -> print_string usage
    end
  | _ -> print_string usage
