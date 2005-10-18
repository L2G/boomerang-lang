let icalviewer = "../../extern/iCalendar/iCalViewer"

let read_ics f ftmp =
  if Sys.file_exists f then
    Toplevel.runcmd (Printf.sprintf "%s ascal %s %s" icalviewer f ftmp)
      
let write_ics f ftmp =
  Toplevel.runcmd (Printf.sprintf "%s asmeta %s %s" icalviewer f ftmp)

let chooseEncoding f =
  if Util.endswith f ".ics" then ("meta", (), Some read_ics, Some write_ics)
  else raise Not_found
      
let chooseAbstractSchema types = "ICalendar.ICalendar_A"

let chooseLens t schema = "ICalendar.l_stamps"
;

Toplevel.toplevel
  "harmonize-calendars"
  (fun() -> "")
  chooseEncoding
  chooseAbstractSchema
  chooseLens
