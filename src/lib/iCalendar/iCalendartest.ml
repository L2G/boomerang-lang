let read_file fname =
  let ch = open_in fname in
  ICalendar.read (fun () -> input_char)
in
let res = read_file Sys.argv.(1) in
if ICalendar_syntax.validate_icalendar res then
  ICalendar.write stdout true res
else
  failwith "ICalendar not well formed for pretty printing"
