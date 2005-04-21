type calvalue =
  | CalGregorian
  | CalOther of string

type attachvalue =
  | AttBinary of string
  | AttUri of string
  
type xparam = string * (string list)
  
type xplist = xparam list

type dur_time =
  { dur_hour : int;
    dur_minute : int;
    dur_second : int; }
    
type dur_length =
  | DurWeek of int
  | DurTime of dur_time
  | DurDate of (int * (dur_time option))
    
type duration =
  { mutable dur_neg : bool;
    mutable dur_length : dur_length;
  }
    
type date =
  { year : int;
    month : int;
    day : int;
  }

type time =
  { hour : int;
    minute : int;
    second : int;
    zulu : bool;
  }
  
type date_time =
  { date : date;
    time : time;
  }

type offset_time =
  { positive : bool;
    off_h : int;
    off_m : int;
    off_s : int; }
  
type period =
  | PeriodExplicit of (date_time * date_time)
  | PeriodStart    of (date_time * duration)
  
type valuetype = DateTime | Date | Binary | Period | Duration | Text

type dtpval =
  | DateTimeVal of date_time
  | DateVal of date
  | PeriodVal of period
  | DurationVal of duration
  
(* most of the Other params must be iana-tokens or xnames, but it is not checked at the moment *)

type encoding = Enc8bit | EncBase64 | EncOther of string

type range = RangePrior | RangeFuture

type cutypeval =
  | CUIndividual | CUGroup | CUResource | CURoom | CUUnknown | CUOther of string

type roleval =
  | RoleChair | RoleReqParticipant | RoleOptParticipant | RoleNonParticipant | RoleOther of string

type partstatval =
  | PSNeedsAction | PSAccepted | PSDeclined | PSTentative | PSDelegated | PSCompleted
  | PSInProcess | PSOther of string

type reltypeval =
| RelParent | RelChild | RelSibling | RelOther of string
    
type trigrelparam = RelStart | RelEnd

type allparam =
  { mutable altrepparam    : string option;
    mutable cnparam        : string option;
    mutable cutypeparam    : cutypeval option;
    mutable delfromparam   : string list; (* at least one element in the list means the option is present *)
    mutable deltoparam     : string list; (* at least one element in the list means the option is present *)
    mutable dirparam       : string option;
    mutable encodingparam  : encoding option;
    mutable fmttypeparam   : string option;
    mutable fbtypeparam    : unit option;
    mutable langparam      : string option;
    mutable memberparam    : string list; (* at least one element in the list means the option is present *)
    mutable partstatparam  : partstatval option;
    mutable rangeparam     : range option;
    mutable reltypeparam   : reltypeval option;
    mutable roleparam      : roleval option;
    mutable rsvpparam      : bool option;
    mutable sentbyparam    : string option;
    mutable trigrelparam   : trigrelparam option;
    mutable tzidparam      : (bool * string) option;
    mutable valuetypeparam : valuetype option;
    mutable xplist         : xplist; 
  }

let noparam () =
  { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
    deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
    fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
    rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
    sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
    xplist         = []; 
  }
    
type description =
  { descparam : allparam;
    desctext : string;
  }

type location =
  { locparam : allparam;
    loctext : string;
  }

type dt =
  { dtparam : allparam;
    dtval : dtpval;
  }

type dtpl =
  { dtplparam : allparam;
    dtplval : dtpval list; (* at least one *)
  }

type icalobjprops =
  { mutable prodid   : xplist * string;
    mutable version  : xplist * string;
    mutable calscale : (xplist * calvalue) option;
    mutable imethod  : (xplist * string) option;
    mutable xprop   : (string * allparam * string) list
  }

type classval =
  | ClassPublic
  | ClassPrivate
  | ClassConfidential
  | ClassOther of string (* may be an xname or an iana-token *)
  
type classt = xplist * classval
  
type dtxp = xplist * date_time
  
type organizer =
  { orgparam : allparam;
    org_cal_address : string; }

type status =
  | STTentative
  | STConfirmed
  | STCancelled
  | STNeedsAction
  | STCompleted
  | STInProcess
  | STDraft
  | STFinal

type transvalue =
  | Transparent
  | Opaque

type freq =
  | Secondly | Minutely | Hourly | Daily | Weekly | Monthly | Yearly

and weekday =
  | Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday 
  
type recur_end =
  | RecUntil of dtpval
  | RecCount of int
  | RecNone
  
type recur =
  {         recur_freq       : freq;
    mutable recur_end        : recur_end;
    mutable recur_interval   : int option;
    mutable recur_bysec      : int list;
    mutable recur_bymin      : int list;
    mutable recur_byhour     : int list;
    mutable recur_byday      : (int option * weekday) list;
    mutable recur_bymonthday : int list;
    mutable recur_byyearday  : int list;
    mutable recur_byweekno   : int list;
    mutable recur_bymonth    : int list;
    mutable recur_bysetpos   : int list;
    mutable recur_wkstart    : weekday option;
    mutable recur_bytext     : (string * string) option;
  }
  
let new_recur f =
  { recur_freq       = f;
    recur_end        = RecNone;
    recur_interval   = None;
    recur_bysec      = [];
    recur_bymin      = [];
    recur_byhour     = [];
    recur_byday      = [];
    recur_bymonthday = [];
    recur_byyearday  = [];
    recur_byweekno   = [];
    recur_bymonth    = [];
    recur_bysetpos   = [];
    recur_wkstart    = None;
    recur_bytext     = None;
  }
  
type summary =
  { sumparam : allparam;
    sumtext : string; }
 
type recurid =
  { recurparam : allparam;
    recurval : dtpval; }

type attach =
  { attachparam : allparam;
    attachval   : attachvalue; } 
  
type attendee =
  { attendeeparam : allparam;
    attendeeval   : string; } 

type categories =
  { categoriesparam : allparam;
    categoriesval   : string list; }  (* at least one *)

type comment =
  { commentparam : allparam;
    commenttext : string;
  }

type contact =
  { contactparam : allparam;
    contacttext : string;
  }

type rstatus =
  { rstatparam : allparam;
    statcode : int list; (*cannot be empty*)
    stattext : string;
    extdata : string option;
  }
  
type related_to =
  { relatedtoparam : allparam;
    relatedtotext : string;
  }

type resources =
  { resourcesparam : allparam;
    resourcesval   : string list; }  (* at least one *)

type action =
| ActAudio 
| ActDisplay
| ActEmail
| ActProcedure
| ActOther of string

type trigger =
  { triggerparam : allparam;
    triggerval : dtpval }

type tzname =
  { tznameparam : allparam;
    tznameval : string }
    
type comp_prop =
  { 
    mutable comp_action       : (xplist * action) option;
    mutable comp_attach       : attach list;  
    mutable comp_attendee     : attendee list;
    mutable comp_categories   : categories list;
    mutable comp_class        : classt option;
    mutable comp_comment      : comment list;
    mutable comp_completed    : unit option;
    mutable comp_contact      : contact list;
    mutable comp_created      : dtxp option;
    mutable comp_daylightc    : comp_prop list;
    mutable comp_description  : description option;
    mutable comp_dtend        : dt option;
    mutable comp_dtstamp      : dt option;
    mutable comp_dtstart      : dt option;
    mutable comp_due          : unit option;
    mutable comp_duration     : (xplist * duration) option;
    mutable comp_exdate       : dtpl list;
    mutable comp_exrule       : (xplist * recur) list;
    mutable comp_freebusy     : unit list;
    mutable comp_geo          : (xplist * (float * float)) option;
    mutable comp_lastmod      : dtxp option;
    mutable comp_location     : location option;
    mutable comp_organizer    : organizer option;
    mutable comp_percent      : unit option;
    mutable comp_priority     : (xplist * int) option;
    mutable comp_rdate        : dtpl list;
    mutable comp_recurid      : recurid option;
    mutable comp_related_to   : related_to list;
    mutable comp_repeat       : (xplist * int) option;
    mutable comp_resources    : resources list;
    mutable comp_rrule        : (xplist * recur) list;
    mutable comp_rstatus      : rstatus list;
    mutable comp_seq          : (xplist * int) option;
    mutable comp_standardc    : comp_prop list;
    mutable comp_status       : (xplist * status) option;
    mutable comp_summary      : summary option;
    mutable comp_transp       : (xplist * transvalue) option;
    mutable comp_trigger      : trigger option;
    mutable comp_tzid         : (xplist * bool * string) option;
    mutable comp_tzname       : tzname list;
    mutable comp_tzoffsetto   : (xplist * offset_time) option;
    mutable comp_tzoffsetfrom : (xplist * offset_time) option;
    mutable comp_tzurl        : (xplist * string) option;
    mutable comp_uid          : (xplist * string) option;
    mutable comp_url          : (xplist * string) option;
    mutable comp_xprop       :  (string * allparam * string) list;
  }

let no_comp_prop () = 
  { 
    comp_action       = None; comp_attach       = []  ; comp_attendee     = []  ; comp_categories   = []  ; 
    comp_class        = None; comp_comment      = []  ; comp_completed    = None; comp_contact      = []  ; 
    comp_created      = None; comp_daylightc    = []  ; comp_description  = None; comp_dtend        = None; 
    comp_dtstamp      = None; comp_dtstart      = None; comp_due          = None; comp_duration     = None; 
    comp_exdate       = []  ; comp_exrule       = []  ; comp_freebusy     = []  ; comp_geo          = None; 
    comp_lastmod      = None; comp_location     = None; comp_organizer    = None; comp_percent      = None; 
    comp_priority     = None; comp_rdate        = []  ; comp_recurid      = None; comp_related_to   = []  ; 
    comp_repeat       = None; comp_resources    = []  ; comp_rrule        = []  ; comp_rstatus      = []  ; 
    comp_seq          = None; comp_standardc    = []  ; comp_status       = None; comp_summary      = None; 
    comp_transp       = None; comp_trigger      = None; comp_tzid         = None; comp_tzname       = []  ; 
    comp_tzoffsetto   = None; comp_tzoffsetfrom = None; comp_tzurl        = None; comp_uid          = None; 
    comp_url          = None; comp_xprop       = []  ;
  }
  
type eventc = { event_comp : comp_prop;
                event_alarms : comp_prop list; }
  
type component =
  | Eventc of eventc
  | Timezonec of comp_prop
  
type icalobject =
  { mutable calprops : icalobjprops ;
    mutable components : component list;
  }

type icalendar = icalobject list

let is_before_date_time d1 d2 =
  if d1.date.year < d2.date.year then true
  else if d1.date.year > d2.date.year then false
  else if d1.date.month < d2.date.month then true
  else if d1.date.month > d2.date.month then false
  else if d1.date.day < d2.date.day then true
  else if d1.date.day > d2.date.day then false
  else if d1.time.zulu <> d2.time.zulu then failwith "comparing a zulu time with a non zulu time"
  else if d1.time.hour < d2.time.hour then true
  else if d1.time.hour > d2.time.hour then false
  else if d1.time.minute < d2.time.minute then true
  else if d1.time.minute > d2.time.minute then false
  else if d1.time.second < d2.time.second then true
  else if d1.time.second > d2.time.second then false
  else true

let validate_period = function
  | PeriodExplicit (dts, dte) -> is_before_date_time dts dte
  | PeriodStart (_, d) -> not d.dur_neg

let validate_recur = function 
  { recur_freq       = _   ; recur_end        = rend; recur_interval   = rint; recur_bysec      = rsec; 
    recur_bymin      = rmin; recur_byhour     = rhour; recur_byday      = rday; recur_bymonthday = rmday;
    recur_byyearday  = ryday; recur_byweekno   = rwno; recur_bymonth    = rmon; recur_bysetpos   = rsetp;
    recur_wkstart    = _; recur_bytext     = _;
  } ->
    (match rend with
     | RecUntil (DateVal _ | DateTimeVal _) -> true
     | RecCount i -> i >= 0
     | RecNone -> true
     | _ -> false)
   &&
    (match rint with
     | Some i -> i >= 0
     | None -> true)
   &&
     List.for_all (fun i -> i >= 0 && i <= 59) rsec
   &&
     List.for_all (fun i -> i >= 0 && i <= 59) rmin
   &&
     List.for_all (fun i -> i >= 0 && i <= 23) rhour
   &&
     List.for_all (fun (io, _) -> 
                          (match io with Some i -> i <> 0 && i >= -53 && i <= 53 | None -> true))
                          rday
   &&
     List.for_all (fun i -> i <> 0 && i >= -31 && i <= 31) rmday
   &&
     List.for_all (fun i -> i <> 0 && i >= -366 && i <= 366) ryday
   &&
     List.for_all (fun i -> i <> 0 && i >= -53 && i <= 53) rwno
   &&
     List.for_all (fun i -> i >= 1 && i <= 12) rmon
   &&
     List.for_all (fun i -> i <> 0 && i >= -366 && i <= 366) rsetp
   &&
     (match rsec, rmin, rhour, rday, rmday, ryday, rwno, rmon, rsetp   with
              [],   [],    [],   [],    [],    [],   [],   [], _ :: _ -> false
            | _ -> true
     )

let validate_xprop = function
  (_, 
          { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
            deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
            fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
            rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
            sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = (None | Some Text);
           },
       _)
      -> true
  | _ -> false

let validate_xprop_opt = function
  | None -> true
  | Some x -> validate_xprop x
  
let validate_xprop_list = List.for_all validate_xprop
  
let validate_class _ = true

let validate_created _ = true

let validate_description = function
  | None -> true
  | Some { descparam = 
             { altrepparam    = _   ; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }
          }
         -> true
  | _ -> false

let validate_dt = function
  | None -> true
  | Some { dtparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = _   ; valuetypeparam = d   ;
             };
           dtval = v; } ->
    (match d,v with
    | (None, (DateVal _ | DateTimeVal _)) | (Some DateTime, DateTimeVal _) | (Some Date, DateVal _) -> true
    | _ -> false)
  | _ -> false

let validate_geo _ = true

let validate_lastmod _ = true

let validate_location = function
  | None -> true
  | Some { locparam = 
             { altrepparam    = _   ; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             };
          } 
         -> true
  | _ -> false

let validate_organizer = function
  | None -> true
  | Some { orgparam = 
             { altrepparam    = None; cnparam        = _   ; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = _   ; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = _   ; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }
         }
       -> true
  | _ -> false

let validate_priority = function
  | None -> true
  | Some (_, n) -> if (n >= 0) && (n < 10) then true else false
  
let validate_sequence _ = true

let validate_event_status = function
  | None -> true
  | Some (_, (STTentative | STConfirmed | STCancelled)) -> true
  | _ -> false
  
let validate_summary = function
  | None -> true
  | Some { sumparam = 
             { altrepparam    = _   ; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             };
          } 
         -> true
  | _ -> false

let validate_transp _ = true
             
let validate_uid _ = true

let validate_url _ = true

let validate_recurid = function
  | None -> true
  | Some { recurparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
               rangeparam     = _   ; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = _   ; valuetypeparam = d   ;
             };
           recurval = v; } ->
    (match d,v with
    | (None, (DateVal _ | DateTimeVal _)) | (Some DateTime, DateTimeVal _) | (Some Date, DateVal _) -> true
    | _ -> false)
  | _ -> false

let validate_dur_time { dur_hour = h; dur_minute = m; dur_second = s; } = 
  h >= 0 && m >= 0 && s >= 0 && (h > 0 || m > 0 || s > 0)
    
(* duration must be positive *)
let validate_dur_length = function
  | DurWeek w -> w > 0
  | DurTime t -> validate_dur_time t
  | DurDate (d, t) -> d > 0 || (match t with None -> false | Some t -> validate_dur_time t)
    
let validate_duration = function
  | None -> true
  | Some (_, { dur_length = d } ) -> validate_dur_length d
    
let validate_attach = List.for_all (
  fun elt -> match elt with
  | { attachparam = 
        { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
          deltoparam     = []  ; dirparam       = None; encodingparam  = enc ; fmttypeparam   = _   ;
          fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
          rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
          sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = v   ;
        };
     attachval = att; } ->
    (match enc,v,att with
    | (None, None, AttUri _) | (Some EncBase64, Some Binary, AttBinary _) -> true
    | _ -> false)
  | _ -> false
  )

let validate_event_partstatparam = function
  | None | Some (PSNeedsAction | PSAccepted | PSDeclined | PSTentative | PSDelegated | PSOther _) -> true
  | _ -> false
  
let validate_attendee = List.for_all (
  fun elt -> match elt with
  | { attendeeparam = 
             { altrepparam    = None; cnparam        = _   ; cutypeparam    = _   ; delfromparam   = _   ;
               deltoparam     = _   ; dirparam       = _   ; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = _   ; partstatparam  = p   ;
               rangeparam     = None; reltypeparam   = None; roleparam      = _   ; rsvpparam      = _   ;
               sentbyparam    = _   ; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }} ->
    validate_event_partstatparam p
  | _ -> false
  )

let validate_categories = List.for_all (
  fun elt -> match elt with
  | { categoriesparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             };
      categoriesval = elt :: rest } -> true
  | _ -> false
  )

let validate_comment = List.for_all (
  fun elt -> match elt with
  | { commentparam = 
             { altrepparam    = _   ; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }} -> true
  | _ -> false
  )

let validate_contact = List.for_all (
  fun elt -> match elt with
  | { contactparam = 
             { altrepparam    = _   ; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }} -> true
  | _ -> false
  )

let validate_dtpl allow_period = List.for_all (
  fun elt -> match elt with
  | { dtplparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = _   ; valuetypeparam = d   ;
             };
      dtplval = ((elt :: rest) as dtv) } ->
    List.for_all (fun v ->
      (match d,v with
      | (None, (DateVal _ | DateTimeVal _)) | (Some DateTime, DateTimeVal _) | (Some Date, DateVal _) -> true
      | (None, PeriodVal p) | (Some Period, PeriodVal p) -> if allow_period then validate_period p else false
      | _ -> false))
      dtv
  | _ -> false
  )

let validate_rule = List.for_all ( fun (x,r) -> validate_recur r )

let validate_rstatus = List.for_all (
  fun elt -> match elt with
  | { rstatparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             };
      statcode = _ :: _ } -> true
  | _ -> false
  )

let validate_related_to = List.for_all (
  fun elt -> match elt with
  | { relatedtoparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = _   ; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }} -> true
  | _ -> false
  )

let validate_resources = List.for_all (
  fun elt -> match elt with
  | { resourcesparam = 
             { altrepparam    = _   ; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             };
      resourcesval = _ :: _ } -> true
  | _ -> false
  )

let validate_eventprop = function 
  | { 
      comp_action       = None; comp_attach       = atta; comp_attendee     = atte; comp_categories   = cate;
      comp_class        = cl  ; comp_comment      = comm; comp_completed    = None; comp_contact      = cont;
      comp_created      = cr  ; comp_daylightc    = []  ; comp_description  = desc; comp_dtend        = dte ;
      comp_dtstamp      = dtst; comp_dtstart      = dts ; comp_due          = None; comp_duration     = dur ;
      comp_exdate       = exda; comp_exrule       = exru; comp_freebusy     = []  ; comp_geo          = geo ;
      comp_lastmod      = last; comp_location     = loc ; comp_organizer    = org ; comp_percent      = None;
      comp_priority     = prio; comp_rdate        = rdat; comp_recurid      = recu; comp_related_to   = rel ;
      comp_repeat       = None; comp_resources    = res ; comp_rrule        = rrul; comp_rstatus      = rsta;
      comp_seq          = seq ; comp_standardc    = []  ; comp_status       = stat; comp_summary      = summ;
      comp_transp       = tran; comp_trigger      = None; comp_tzid         = None; comp_tzname       = []  ;
      comp_tzoffsetto   = None; comp_tzoffsetfrom = None; comp_tzurl        = None; comp_uid          = uid ;
      comp_url          = url ; comp_xprop       = xpro;
    } ->
  validate_class cl && 
  validate_created cr &&
  validate_description desc &&
  validate_dt dts &&
  validate_geo geo &&
  validate_lastmod last &&
  validate_location loc &&
  validate_organizer org &&
  validate_priority prio &&
  validate_dt dtst &&
  validate_sequence seq &&
  validate_event_status stat &&
  validate_summary summ &&
  validate_transp tran &&
  validate_uid uid &&
  validate_url url &&
  validate_recurid recu &&
  validate_dt dte &&
  validate_duration dur &&
  validate_attach atta &&
  validate_attendee atte &&
  validate_categories cate &&
  validate_comment comm &&
  validate_contact cont &&
  validate_dtpl false exda && 
  validate_rule exru && 
  validate_rstatus rsta && 
  validate_related_to rel && 
  validate_resources res && 
  validate_dtpl true rdat && 
  validate_rule rrul && 
  validate_xprop_list xpro &&
  ( match dte, dur with Some _, Some _ -> false | _, _ -> true)
 | _ -> false
  
let validate_trigger = function
  | { triggerparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = None; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = t   ; tzidparam      = None; valuetypeparam = d   ;
             };
      triggerval = elt } ->
        (match t,d,elt with
        | _, (None | Some Duration), DurationVal _ -> true
        | None, Some DateTime, DateTimeVal _ -> true
        | _ -> false)
  | _ -> false

let validate_repeat _ = true
 
let validate_alarm = function
  | { 
      comp_action       = acti; comp_attach       = atta; comp_attendee     = atte; comp_categories   = []  ; 
      comp_class        = None; comp_comment      = []  ; comp_completed    = None; comp_contact      = []  ; 
      comp_created      = None; comp_daylightc    = []  ; comp_description  = desc; comp_dtend        = None; 
      comp_dtstamp      = None; comp_dtstart      = None; comp_due          = None; comp_duration     = dura; 
      comp_exdate       = []  ; comp_exrule       = []  ; comp_freebusy     = []  ; comp_geo          = None; 
      comp_lastmod      = None; comp_location     = None; comp_organizer    = None; comp_percent      = None; 
      comp_priority     = None; comp_rdate        = []  ; comp_recurid      = None; comp_related_to   = []  ; 
      comp_repeat       = repe; comp_resources    = []  ; comp_rrule        = []  ; comp_rstatus      = []  ; 
      comp_seq          = None; comp_standardc    = []  ; comp_status       = None; comp_summary      = summ; 
      comp_transp       = None; comp_trigger      = trig; comp_tzid         = None; comp_tzname       = []  ; 
      comp_tzoffsetto   = None; comp_tzoffsetfrom = None; comp_tzurl        = None; comp_uid          = None; 
      comp_url          = None; comp_xprop        = xpro;
    } ->
       validate_xprop_list xpro &&
      (* common rule to all alarms: duration and repeat are optional but must occur together
         if they do occur *)
       ( match dura, repe with
         | Some dura, Some repe -> (validate_duration (Some dura)) && (validate_repeat repe)
         | None, None -> true
         | _ -> false) &&
       ( match acti with Some _ -> true | None -> false) &&
       ( match trig with Some t -> validate_trigger t | None -> false) &&
       ( match             acti,              atta, atte,   desc, summ with
         | Some (_,   ActAudio), (([_] | []) as a),   [],   None,   None -> validate_attach a 
         | Some (_, ActDisplay),                [],   [], Some d,   None -> validate_description (Some d)
         | Some (_,   ActEmail),                 a, e::r, Some d, Some s -> validate_attach a             &&
                                                                            validate_description (Some d) &&
                                                                            validate_summary (Some s)     &&
                                                                            validate_attendee (e::r)
         | Some (_, ActProcedure),            [a],    [],      d,   None -> validate_attach [a] &&
                                                                            validate_description d
         | _ -> false
       )
  | _ -> false
 
let validate_alarm_list = List.for_all validate_alarm
 
let validate_tzid _ = true

let validate_tzurl _ = true

let validate_offsettime { positive = p; off_h = h; off_m = m; off_s = s} =
  (* might want to check more ... should check the rfc for that *)
  h >= 0 && m >= 0 && s >= 0 && s < 60 && not ( (not p) && h = 0 && m = 0 && s = 0)

let validate_tzoffset = function
  | None -> true
  | Some (xpl, off) -> validate_offsettime off

let validate_tzname = List.for_all (
  fun elt -> match elt with
  | { tznameparam = 
             { altrepparam    = None; cnparam        = None; cutypeparam    = None; delfromparam   = []  ;
               deltoparam     = []  ; dirparam       = None; encodingparam  = None; fmttypeparam   = None;
               fbtypeparam    = None; langparam      = _   ; memberparam    = []  ; partstatparam  = None;
               rangeparam     = None; reltypeparam   = None; roleparam      = None; rsvpparam      = None;
               sentbyparam    = None; trigrelparam   = None; tzidparam      = None; valuetypeparam = None;
             }} -> true
  | _ -> false
  )

let validate_tzprop = function
  | { 
      comp_action       = None; comp_attach       = []  ; comp_attendee     = []  ; comp_categories   = []  ; 
      comp_class        = None; comp_comment      = comm; comp_completed    = None; comp_contact      = []  ; 
      comp_created      = None; comp_daylightc    = []  ; comp_description  = None; comp_dtend        = None; 
      comp_dtstamp      = None; comp_dtstart      =  dts; comp_due          = None; comp_duration     = None; 
      comp_exdate       = []  ; comp_exrule       = []  ; comp_freebusy     = []  ; comp_geo          = None; 
      comp_lastmod      = None; comp_location     = None; comp_organizer    = None; comp_percent      = None; 
      comp_priority     = None; comp_rdate        = rdat; comp_recurid      = None; comp_related_to   = []  ; 
      comp_repeat       = None; comp_resources    = []  ; comp_rrule        = rrul; comp_rstatus      = []  ; 
      comp_seq          = None; comp_standardc    = []  ; comp_status       = None; comp_summary      = None; 
      comp_transp       = None; comp_trigger      = None; comp_tzid         = None; comp_tzname       = tzna; 
      comp_tzoffsetto   = tzot; comp_tzoffsetfrom = tzof; comp_tzurl        = None; comp_uid          = None; 
      comp_url          = None; comp_xprop        = xpro;
    } ->
      validate_xprop_list xpro && 
      validate_comment comm &&
      validate_dtpl true rdat &&
      validate_rule rrul &&
      validate_tzname tzna &&
      (match dts, tzot, tzof with
       | Some _, Some _, Some _ -> validate_dt dts && validate_tzoffset tzot && validate_tzoffset tzof
       | _ -> false)
  | _ -> false

let validate_timezone = function
  | { 
      comp_action       = None; comp_attach       = []  ; comp_attendee     = []  ; comp_categories   = []  ; 
      comp_class        = None; comp_comment      = []  ; comp_completed    = None; comp_contact      = []  ; 
      comp_created      = None; comp_daylightc    = dc  ; comp_description  = None; comp_dtend        = None; 
      comp_dtstamp      = None; comp_dtstart      = None; comp_due          = None; comp_duration     = None; 
      comp_exdate       = []  ; comp_exrule       = []  ; comp_freebusy     = []  ; comp_geo          = None; 
      comp_lastmod      = lmod; comp_location     = None; comp_organizer    = None; comp_percent      = None; 
      comp_priority     = None; comp_rdate        = []  ; comp_recurid      = None; comp_related_to   = []  ; 
      comp_repeat       = None; comp_resources    = []  ; comp_rrule        = []  ; comp_rstatus      = []  ; 
      comp_seq          = None; comp_standardc    = sc  ; comp_status       = None; comp_summary      = None; 
      comp_transp       = None; comp_trigger      = None; comp_tzid         = tzid; comp_tzname       = []  ; 
      comp_tzoffsetto   = None; comp_tzoffsetfrom = None; comp_tzurl        = turl; comp_uid          = None; 
      comp_url          = None; comp_xprop        = xpro;
    } ->
      validate_xprop_list xpro && 
      (match tzid with | Some t -> validate_tzid t
                       | None   -> false) &&
      validate_lastmod lmod &&
      validate_tzurl turl &&
      (match sc, dc with | [], [] -> false
                         | l1, l2 -> List.for_all validate_tzprop l1 &&
                                     List.for_all validate_tzprop l2)
  | _ -> false


(* TODO: validate relation between alarm and event
         (for instance if an alarm is set on START, then DTSTART must be set in event
          see P70 of rfc2445 for what needs to be checked)
*)
let validate_component = function
  | Eventc e ->
      validate_eventprop e.event_comp && validate_alarm_list e.event_alarms
  | Timezonec t ->
      validate_timezone t
  
let validate_calprops _ = true
      
let validate_icalobject { calprops = cp; components = comps; } =
  (validate_calprops cp) && (List.for_all validate_component comps) 

let validate_icalendar = List.for_all validate_icalobject

