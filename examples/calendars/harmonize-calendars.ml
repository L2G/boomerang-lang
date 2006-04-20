let read_ics f ftmp = 
  let fc = open_in_bin f in
  let ftmpc = open_out_bin ftmp in 
    ICalendar.iCalReader fc ftmpc;
    close_in fc; 
    close_out ftmpc
      
let write_ics f ftmp = 
  let fc = open_in f in 
  let ftmpc = open_out_bin ftmp in 
    ICalendar.iCalWriter fc ftmpc;
    close_in fc;
    close_out ftmpc

let chooseEncoding f =
  if Util.endswith f ".ics" then ("meta", (), Some read_ics, Some write_ics)
  else raise Not_found
      
let chooseAbstractSchema types = "ICalendar.ICalendar_A"

let chooseLens t schema = "ICalendar.l_stamps"
;;

Toplevel.toplevel
  "harmonize-calendars"
  (fun() -> "")
  chooseEncoding
  chooseAbstractSchema
  chooseLens
