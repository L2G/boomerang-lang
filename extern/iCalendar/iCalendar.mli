(** the (unit -> char) function should raise End_of_file when done reading *)
val read : (unit -> char) -> ICalendar_syntax.icalendar
(** bool = true: use \r\n for end of line, bool = false, use \n  *)
val write : out_channel -> bool -> ICalendar_syntax.icalendar -> unit
val iCalReader : in_channel -> out_channel -> unit
val iCalWriter : in_channel -> out_channel -> unit
