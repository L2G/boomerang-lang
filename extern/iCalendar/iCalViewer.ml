
(******************************************************************************)
(*                              iCalendar viewer                              *)
(******************************************************************************)

let try_finalize f x e y =
  try (let r = f x in e y; r)
  with exn -> e y; raise exn

let ics2meta icsf metaf = 
  let go () = 
    let inc = open_in icsf in
    let outc = open_out metaf in 
    let close () = close_in inc; close_out outc in
      try_finalize (ICalendar.iCalReader inc) outc close () 
  in
    Unix.handle_unix_error go ()

let meta2ics metaf icsf = 
  let go () = 
    let inc = open_in metaf in
    let outc = open_out icsf in 
    let close () = close_in inc; close_out outc in
      try_finalize (ICalendar.iCalWriter inc) outc close () 
  in
    Unix.handle_unix_error go ()
  

let usage = "Usage : [iCalViewer ascal src dest] converts the ics calendar in src"
  ^ " to a meta view in dest\n\t [iCalViewer asmeta src dest] does the opposite\n"

let _ =
  match Array.length Sys.argv with
    4 -> begin
      match Sys.argv.(1) with
	  "ascal"  -> ics2meta Sys.argv.(2) Sys.argv.(3) 
        | "asmeta" -> meta2ics Sys.argv.(2) Sys.argv.(3) 
        | _        -> print_string usage
    end
  | _ -> print_string usage
