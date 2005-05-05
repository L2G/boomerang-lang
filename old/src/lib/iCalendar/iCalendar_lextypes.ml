type all_params =
  | Altrepparam of string
  | CNparam of string
  | Cutypeparam of ICalendar_syntax.cutypeval
  | Delfromparam of string list
  | Deltoparam of string list
  | Dirparam of string
  | Encparam of ICalendar_syntax.encoding
  | Fmtparam of string
  | Langparam of string
  | Memberparam of string list
  | Partstatparam of ICalendar_syntax.partstatval
  | Rangeparam of ICalendar_syntax.range
  | Reltypeparam of ICalendar_syntax.reltypeval
  | Roleparam of ICalendar_syntax.roleval
  | Rsvpparam of bool
  | Sentbyparam of string
  | Trigrelparam of ICalendar_syntax.trigrelparam
  | Tzidparam of (bool * string)
  | Valuetypeparam of ICalendar_syntax.valuetype
  | Xparam of ICalendar_syntax.xparam

type duration =
  | Week of int
  | DayT of (int * duration)
  | Day of int
  | Hour of (int * duration)
  | Minute of (int * duration)
  | Second of int
  | DurEnd

type recur =
  | RecDate of ICalendar_syntax.dtpval
  | RecCount of int
  | RecInterval of int
  | RecBySecond of int list
  | RecByMinute of int list
  | RecByHour of int list
  | RecByDay of (int option * ICalendar_syntax.weekday) list
  | RecByMonthDay of int list
  | RecByYearDay of int list
  | RecByWeekNo of int list
  | RecByMonth of int list
  | RecBySetPos of int list
  | RecWkStart of ICalendar_syntax.weekday
  | RecX of (string * string)
  
let mk_date (y,m,d) = { ICalendar_syntax.year = y; ICalendar_syntax.month = m; ICalendar_syntax.day = d;}
let mk_time ((h,m,s),z) = { ICalendar_syntax.hour = h; ICalendar_syntax.minute = m; 
                            ICalendar_syntax.second = s; ICalendar_syntax.zulu = z; }
let mk_datetime d t =  { ICalendar_syntax.date = mk_date d; ICalendar_syntax.time = mk_time t; }

let convert_time = function
  | Hour (h, Minute (m, Second s)) -> { ICalendar_syntax.dur_hour = h; 
                                        ICalendar_syntax.dur_minute = m; 
                                        ICalendar_syntax.dur_second = s; }
  | Hour (h, Minute (m, DurEnd))   -> { ICalendar_syntax.dur_hour = h; 
                                        ICalendar_syntax.dur_minute = m; 
                                        ICalendar_syntax.dur_second = 0; }
  | Hour (h, DurEnd)               -> { ICalendar_syntax.dur_hour = h; 
                                        ICalendar_syntax.dur_minute = 0; 
                                        ICalendar_syntax.dur_second = 0; }
  | Minute (m, Second s)           -> { ICalendar_syntax.dur_hour = 0; 
                                        ICalendar_syntax.dur_minute = m; 
                                        ICalendar_syntax.dur_second = s; }
  | Minute (m, DurEnd)             -> { ICalendar_syntax.dur_hour = 0; 
                                        ICalendar_syntax.dur_minute = m; 
                                        ICalendar_syntax.dur_second = 0; }
  | Second s                       -> { ICalendar_syntax.dur_hour = 0; 
                                        ICalendar_syntax.dur_minute = 0; 
                                        ICalendar_syntax.dur_second = s; }
  | _                              -> failwith "Duration Time badly formatted"

let convert_duration_time t =
  { ICalendar_syntax.dur_neg = false; 
    ICalendar_syntax.dur_length = ICalendar_syntax.DurTime (convert_time t); }

let convert_duration_week_date = function
  | Week w     -> { ICalendar_syntax.dur_neg = false; 
                    ICalendar_syntax.dur_length = ICalendar_syntax.DurWeek w; }
  | DayT (d,t) -> { ICalendar_syntax.dur_neg = false; 
                    ICalendar_syntax.dur_length = ICalendar_syntax.DurDate (d, Some (convert_time t)); }
  | Day d      -> { ICalendar_syntax.dur_neg = false; 
                    ICalendar_syntax.dur_length = ICalendar_syntax.DurDate (d, None); }
  | _          -> failwith "Duration Week or Date badly formatted"

let neg_dur d =
  d.ICalendar_syntax.dur_neg <- true;
  d

let calvalue_from_string = function
  | "GREGORIAN" -> ICalendar_syntax.CalGregorian
  | s           -> ICalendar_syntax.CalOther s

let  cutype_from_string = function
| "INDIVIDUAL"          -> ICalendar_syntax.CUIndividual 
| "GROUP"               -> ICalendar_syntax.CUGroup 
| "RESOURCE"            -> ICalendar_syntax.CUResource 
| "ROOM"                -> ICalendar_syntax.CURoom 
| "UNKNOWN"             -> ICalendar_syntax.CUUnknown 
| s                     -> ICalendar_syntax.CUOther s 

let encoding_from_string = function
| "8BIT"                -> ICalendar_syntax.Enc8bit 
| "BASE64"              -> ICalendar_syntax.EncBase64 
| s                     -> ICalendar_syntax.EncOther s

let partstatparam_from_string = function
| "NEEDS-ACTION"        -> ICalendar_syntax.PSNeedsAction 
| "ACCEPTED"            -> ICalendar_syntax.PSAccepted 
| "DECLINED"            -> ICalendar_syntax.PSDeclined
| "TENTATIVE"           -> ICalendar_syntax.PSTentative 
| "DELEGATED"           -> ICalendar_syntax.PSDelegated 
| "COMPLETED"           -> ICalendar_syntax.PSCompleted 
| "IN-PROCESS"          -> ICalendar_syntax.PSInProcess 
| s                     -> ICalendar_syntax.PSOther s

let rangeparam_from_string = function
| "THISANDPRIOR"  -> ICalendar_syntax.RangePrior
| "THISANDFUTURE" -> ICalendar_syntax.RangeFuture
| _ -> assert false

let reltypeparam_from_string = function
| "PARENT"              -> ICalendar_syntax.RelParent 
| "CHILD"               -> ICalendar_syntax.RelChild  
| "SIBLING"             -> ICalendar_syntax.RelSibling      
| s                     -> ICalendar_syntax.RelOther s
              
let roleparam_from_string = function
| "CHAIR"               -> ICalendar_syntax.RoleChair 
| "REQ-PARTICIPANT"     -> ICalendar_syntax.RoleReqParticipant 
| "OPT-PARTICIPANT"     -> ICalendar_syntax.RoleOptParticipant 
| "NON-PARTICIPANT"     -> ICalendar_syntax.RoleNonParticipant 
| s                     -> ICalendar_syntax.RoleOther s

let trigrelparam_from_string = function
| "START"       -> ICalendar_syntax.RelStart
| "END"         -> ICalendar_syntax.RelEnd 
| _             -> assert false

let valuetypeparam_from_string = function
| "DATE-TIME"           -> ICalendar_syntax.DateTime 
| "DATE"                -> ICalendar_syntax.Date 
| "BINARY"              -> ICalendar_syntax.Binary 
| "PERIOD"              -> ICalendar_syntax.Period 
| "DURATION"            -> ICalendar_syntax.Duration 
| "TEXT"                -> ICalendar_syntax.Text 
| _                     -> failwith "value type not yet implemented"

let actionval_from_string = function
| "AUDIO"     -> ICalendar_syntax.ActAudio 
| "DISPLAY"   -> ICalendar_syntax.ActDisplay 
| "EMAIL"     -> ICalendar_syntax.ActEmail 
| "PROCEDURE" -> ICalendar_syntax.ActProcedure 
| s           -> ICalendar_syntax.ActOther s

let classvalue_from_string = function
| "PUBLIC"                  -> ICalendar_syntax.ClassPublic 
| "PRIVATE"                 -> ICalendar_syntax.ClassPrivate 
| "CONFIDENTIAL"            -> ICalendar_syntax.ClassConfidential 
| s                         -> ICalendar_syntax.ClassOther s

let weekday_from_string = function
| "SU"                   -> ICalendar_syntax.Sunday 
| "MO"                   -> ICalendar_syntax.Monday 
| "TU"                   -> ICalendar_syntax.Tuesday 
| "WE"                   -> ICalendar_syntax.Wednesday 
| "TH"                   -> ICalendar_syntax.Thursday 
| "FR"                   -> ICalendar_syntax.Friday 
| "SA"                   -> ICalendar_syntax.Saturday 
| s                      -> failwith (s^" is not a correct week day")

let freq_from_string = function
| "SECONDLY"             -> ICalendar_syntax.Secondly 
| "MINUTELY"             -> ICalendar_syntax.Minutely 
| "HOURLY"               -> ICalendar_syntax.Hourly 
| "DAILY"                -> ICalendar_syntax.Daily 
| "WEEKLY"               -> ICalendar_syntax.Weekly 
| "MONTHLY"              -> ICalendar_syntax.Monthly 
| "YEARLY"               -> ICalendar_syntax.Yearly 
| s                      -> failwith (s^" is not a correct frequency")
                           
let status_from_string = function
| "TENTATIVE"            -> ICalendar_syntax.STTentative 
| "CONFIRMED"            -> ICalendar_syntax.STConfirmed 
| "CANCELLED"            -> ICalendar_syntax.STCancelled 
| "NEEDS-ACTIONS"        -> ICalendar_syntax.STNeedsAction 
| "COMPLETED"            -> ICalendar_syntax.STCompleted 
| "IN-PROCESS"           -> ICalendar_syntax.STInProcess 
| "DRAFT"                -> ICalendar_syntax.STDraft 
| "FINAL"                -> ICalendar_syntax.STFinal 
| s                      -> failwith (s^" is not a correct status")

let transvalue_from_string = function
| "TRANSPARENT"          -> ICalendar_syntax.Transparent 
| "OPAQUE"               -> ICalendar_syntax.Opaque 
| s                      -> failwith (s^" is not a correct transparency value")

