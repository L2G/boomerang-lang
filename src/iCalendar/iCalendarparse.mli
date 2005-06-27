type token =
  | BVCALENDAR
  | EVCALENDAR
  | BVEVENT
  | EVEVENT
  | BVALARM
  | EVALARM
  | BVTIMEZONE
  | EVTIMEZONE
  | BSTANDARD
  | ESTANDARD
  | BDAYLIGHT
  | EDAYLIGHT
  | EOF
  | PRODID of ( ICalendar_syntax.xplist * string )
  | VERSION of ( ICalendar_syntax.xplist * string )
  | CALSCALE of ( ICalendar_syntax.xplist * ICalendar_syntax.calvalue )
  | METHOD of ( ICalendar_syntax.xplist * string )
  | XPROP of ( string * (ICalendar_lextypes.all_params list) * string )
  | CLASS of ( ICalendar_syntax.xplist * ICalendar_syntax.classval )
  | CREATED of ( ICalendar_syntax.xplist * ICalendar_syntax.date_time )
  | DESCRIPTION of ( (ICalendar_lextypes.all_params list) * string )
  | DTSTART of ( (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval )
  | GEO of ( ICalendar_syntax.xplist * ( float * float ))
  | LASTMODIFIED of ( ICalendar_syntax.xplist * ICalendar_syntax.date_time )
  | LOCATION of ( (ICalendar_lextypes.all_params list) * string )
  | ORGANIZER of ( (ICalendar_lextypes.all_params list) * string )
  | PRIORITY of ( ICalendar_syntax.xplist * int )
  | DTSTAMP of ( ICalendar_lextypes.all_params list * ICalendar_syntax.dtpval )
  | SEQUENCE of ( ICalendar_syntax.xplist * int )
  | STATUS of ( ICalendar_syntax.xplist * ICalendar_syntax.status )
  | SUMMARY of ( (ICalendar_lextypes.all_params list) * string )
  | TRANSP of ( ICalendar_syntax.xplist * ICalendar_syntax.transvalue )
  | UID of ( ICalendar_syntax.xplist * string )
  | URL of ( ICalendar_syntax.xplist * string )
  | RECURRENCEID of ( (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval )
  | DTEND of ( (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval )
  | DURATION of ( ICalendar_syntax.xplist * ICalendar_syntax.duration )
  | ATTACH of ( (ICalendar_lextypes.all_params list) * ICalendar_syntax.attachvalue )
  | ATTENDEE of ( (ICalendar_lextypes.all_params list) * string)
  | CATEGORIES of ( (ICalendar_lextypes.all_params list) * string list)
  | COMMENT of ( (ICalendar_lextypes.all_params list) * string)
  | CONTACT of ( (ICalendar_lextypes.all_params list) * string)
  | EXDATE of ( (ICalendar_lextypes.all_params list) * (ICalendar_syntax.dtpval) list )
  | EXRULE of ( ICalendar_syntax.xplist * (ICalendar_syntax.freq * ICalendar_lextypes.recur list) )
  | RSTATUS of ( (ICalendar_lextypes.all_params list) * int list * string * string option)
  | RELATED_TO of ( (ICalendar_lextypes.all_params list) * string)
  | RESOURCES of ( (ICalendar_lextypes.all_params list) * string list)
  | RDATE of ( (ICalendar_lextypes.all_params list) * (ICalendar_syntax.dtpval) list )
  | RRULE of ( ICalendar_syntax.xplist * (ICalendar_syntax.freq * ICalendar_lextypes.recur list) )
  | ACTION of ( ICalendar_syntax.xplist * ICalendar_syntax.action )
  | TRIGGER of ( ICalendar_lextypes.all_params list * ICalendar_syntax.dtpval )
  | REPEAT of ( ICalendar_syntax.xplist * int )
  | TZID of ( ICalendar_syntax.xplist * bool * string )
  | TZURL of ( ICalendar_syntax.xplist * string )
  | TZOFFSETTO of ( ICalendar_syntax.xplist * ICalendar_syntax.offset_time )
  | TZOFFSETFROM of ( ICalendar_syntax.xplist * ICalendar_syntax.offset_time )
  | TZNAME of ( ICalendar_lextypes.all_params list * string )

val icalendar :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ICalendar_syntax.icalendar
