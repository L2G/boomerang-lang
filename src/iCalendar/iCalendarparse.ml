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

open Parsing;;
# 2 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
open ICalendar_syntax
open ICalendar_print

type props =
  | Prodid of (xplist * string)
  | Version of (xplist * string)
  | Calscale of (xplist * calvalue)
  | Method of (xplist * string)
  | Xprop of (string * allparam * string)

type all_component_props =
  | Class of (xplist * classval)
  | Created of (xplist * date_time)
  | Description of description
  | DTstart of dt
  | Geo of (xplist * (float * float))
  | LastModified of (xplist * date_time)
  | Location of location
  | Organizer of organizer
  | Priority of (xplist * int)
  | Dtstamp of dt
  | Sequence of (xplist * int)
  | Status of (xplist * status)
  | Summary of summary
  | Transp of (xplist * transvalue)
  | Uid of (xplist * string)
  | Url of (xplist * string)
  | Recurid of recurid
  | DTend of dt
  | Duration of (xplist * duration)
  | Attach of attach
  | Attendee of attendee
  | Categories of categories
  | Comment of comment
  | Contact of contact
  | Exdate of dtpl
  | Exrule of (xplist * recur)
  | Rstatus of rstatus
  | Related_to of related_to
  | Resources of resources
  | Rdate of dtpl
  | Rrule of (xplist * recur)
  | CXprop of (string * allparam * string)
  | Action of (xplist * action)
  | Trigger of trigger
  | Repeat of (xplist * int)
  | Tzid of (xplist * bool * string)
  | Tzurl of (xplist * string)
  | Standardc of comp_prop
  | Daylightc of comp_prop
  | Tzoffsetto of (xplist * offset_time)
  | Tzoffsetfrom of (xplist * offset_time)
  | Tzname of tzname

let mk_allparam l =
  let p = noparam () in
  let set_altrepparam s =
    match p.altrepparam with
    | Some _ -> failwith "setting ALTREP twice"
    | None -> p.altrepparam <- Some s
  in
  let set_cnparam c =
    match p.cnparam with
    | Some _ -> failwith "setting CN twice"
    | None -> p.cnparam <- Some c
  in
  let set_cutypeparam c =
    match p.cutypeparam with
    | Some _ -> failwith "setting CUTYPE twice"
    | None -> p.cutypeparam <- Some c
  in
  let set_delfromparam d =
    match p.delfromparam with
    | [] -> p.delfromparam <- d
    | _ -> failwith "setting DELEGATE-FROM twice"
  in
  let set_deltoparam d =
    match p.deltoparam with
    | [] -> p.deltoparam <- d
    | _ -> failwith "setting DELEGATE-TO twice"
  in
  let set_dirparam d =
    match p.dirparam with
    | Some _ -> failwith "setting DIR twice"
    | None -> p.dirparam <- Some d
  in
  let set_encparam e =
    match p.encodingparam with
    | Some _ -> failwith "setting ENCODING twice"
    | None -> p.encodingparam <- Some e
  in
  let set_fmtparam f =
    match p.fmttypeparam with
    | Some _ -> failwith "setting FMTTYPE twice"
    | None -> p.fmttypeparam <- Some f
  in
  let set_langparam l =
    match p.langparam with
    | Some _ -> failwith "setting LANGUAGE twice"
    | None -> p.langparam <- Some l
  in
  let set_memberparam m =
    match p.memberparam with
    | [] -> p.memberparam <- m
    | _ -> failwith "setting MEMBER twice"
  in
  let set_partstatparam pa =
    match p.partstatparam with
    | Some _ -> failwith "setting PARTSTAT twice"
    | None -> p.partstatparam <- Some pa
  in
  let set_rangeparam r =
    match p.rangeparam with
    | Some _ -> failwith "setting RANGE twice"
    | None -> p.rangeparam <- Some r
  in
  let set_reltypeparam r =
    match p.reltypeparam with
    | Some _ -> failwith "setting RELTYPE twice"
    | None -> p.reltypeparam <- Some r
  in
  let set_roleparam r =
    match p.roleparam with
    | Some _ -> failwith "setting ROLE twice"
    | None -> p.roleparam <- Some r
  in
  let set_rsvpparam r =
    match p.rsvpparam with
    | Some _ -> failwith "setting RSVP twice"
    | None -> p.rsvpparam <- Some r
  in
  let set_sentbyparam l =
    match p.sentbyparam with
    | Some _ -> failwith "setting SENT-BY twice"
    | None -> p.sentbyparam <- Some l
  in
  let set_trigrelparam t =
    match p.trigrelparam with
    | Some _ -> failwith "setting RELATED twice"
    | None -> p.trigrelparam <- Some t
  in
  let set_tzidparam t =
    match p.tzidparam with
    | Some _ -> failwith "setting TZID twice"
    | None -> p.tzidparam <- Some t
  in
  let set_valuetypeparam v =
    match p.valuetypeparam with
    | Some _ -> failwith "setting VALUE twice"
    | None -> p.valuetypeparam <- Some v
  in
  let set_xparam x =
    p.xplist <- x :: p.xplist
  in
  let rec loop = function
    | ICalendar_lextypes.Altrepparam s :: r -> set_altrepparam s; loop r
    | ICalendar_lextypes.CNparam c :: r -> set_cnparam c; loop r
    | ICalendar_lextypes.Cutypeparam c :: r -> set_cutypeparam c; loop r
    | ICalendar_lextypes.Delfromparam d :: r -> set_delfromparam d; loop r
    | ICalendar_lextypes.Deltoparam d :: r -> set_deltoparam d; loop r
    | ICalendar_lextypes.Dirparam d :: r -> set_dirparam d; loop r
    | ICalendar_lextypes.Encparam e :: r -> set_encparam e; loop r
    | ICalendar_lextypes.Fmtparam f :: r -> set_fmtparam f; loop r
    | ICalendar_lextypes.Langparam l :: r -> set_langparam l; loop r
    | ICalendar_lextypes.Memberparam m :: r -> set_memberparam m; loop r
    | ICalendar_lextypes.Partstatparam p :: r -> set_partstatparam p; loop r
    | ICalendar_lextypes.Rangeparam ra :: r -> set_rangeparam ra; loop r
    | ICalendar_lextypes.Reltypeparam re :: r -> set_reltypeparam re; loop r
    | ICalendar_lextypes.Roleparam ro :: r -> set_roleparam ro; loop r
    | ICalendar_lextypes.Rsvpparam rs :: r -> set_rsvpparam rs; loop r
    | ICalendar_lextypes.Sentbyparam l :: r -> set_sentbyparam l; loop r
    | ICalendar_lextypes.Trigrelparam t :: r -> set_trigrelparam t; loop r
    | ICalendar_lextypes.Tzidparam t :: r -> set_tzidparam t; loop r
    | ICalendar_lextypes.Valuetypeparam v :: r -> set_valuetypeparam v; loop r
    | ICalendar_lextypes.Xparam x :: r -> set_xparam x; loop r
    | [] -> ()
  in loop l;
  p

let mk_recur (f,l) =
  let r = new_recur f in
  let set_date e =
    match r.recur_end with
    | RecNone -> r.recur_end <- RecUntil e
    | _ -> failwith "setting UNTIL or COUNT twice in recur (only one may occur)"
  in
  let set_count i =
    match r.recur_end with
    | RecNone -> r.recur_end <- RecCount i
    | _ -> failwith "setting UNTIL or COUNT twice in recur (only one may occur)"
  in
  let set_interval i =
    match r.recur_interval with
    | None -> r.recur_interval <- Some i
    | _ -> failwith "setting INTERVAL twice in recur" 
  in
  let set_bysec l =
    match r.recur_bysec with
    | [] -> r.recur_bysec <- l
    | _ -> failwith "setting BYSECOND twice in recur" 
  in
  let set_bymin l =
    match r.recur_bymin with
    | [] -> r.recur_bymin <- l
    | _ -> failwith "setting BYMINUTE twice in recur" 
  in
  let set_byhour l =
    match r.recur_byhour with
    | [] -> r.recur_byhour <- l
    | _ -> failwith "setting BYHOUR twice in recur" 
  in
  let set_byday l =
    match r.recur_byday with
    | [] -> r.recur_byday <- l
    | _ -> failwith "setting BYDAY twice in recur" 
  in
  let set_bymonthday l =
    match r.recur_bymonthday with
    | [] -> r.recur_bymonthday <- l
    | _ -> failwith "setting BYMONTHDAY twice in recur" 
  in
  let set_byyearday l =
    match r.recur_byyearday with
    | [] -> r.recur_byyearday <- l
    | _ -> failwith "setting BYYEARDAY twice in recur" 
  in
  let set_byweekno l =
    match r.recur_byweekno with
    | [] -> r.recur_byweekno <- l
    | _ -> failwith "setting BYWEEKNO twice in recur" 
  in
  let set_bymonth l =
    match r.recur_bymonth with
    | [] -> r.recur_bymonth <- l
    | _ -> failwith "setting BYMONTH twice in recur" 
  in
  let set_bysetpos l =
    match r.recur_bysetpos with
    | [] -> r.recur_bysetpos <- l
    | _ -> failwith "setting BYSETPOS twice in recur" 
  in
  let set_wkstart w =
    match r.recur_wkstart with
    | None -> r.recur_wkstart <- Some w
    | _ -> failwith "setting WKST twice in recur" 
  in
  let set_bytext t =
    match r.recur_bytext with
    | None -> r.recur_bytext <- Some t
    | _ -> failwith "setting xname twice in recur (is this the intent of the rfc ?)" 
  in
  let rec loop = function
  | ICalendar_lextypes.RecDate v :: r ->  set_date v; loop r
  | ICalendar_lextypes.RecCount v :: r ->  set_count v; loop r
  | ICalendar_lextypes.RecInterval v :: r ->  set_interval v; loop r
  | ICalendar_lextypes.RecBySecond v :: r ->  set_bysec v; loop r
  | ICalendar_lextypes.RecByMinute v :: r ->  set_bymin v; loop r
  | ICalendar_lextypes.RecByHour v :: r ->  set_byhour v; loop r
  | ICalendar_lextypes.RecByDay v :: r ->  set_byday v; loop r
  | ICalendar_lextypes.RecByMonthDay v :: r ->  set_bymonthday v; loop r
  | ICalendar_lextypes.RecByYearDay v :: r ->  set_byyearday v; loop r
  | ICalendar_lextypes.RecByWeekNo v :: r ->  set_byweekno v; loop r
  | ICalendar_lextypes.RecByMonth v :: r ->  set_bymonth v; loop r
  | ICalendar_lextypes.RecBySetPos v :: r ->  set_bysetpos v; loop r
  | ICalendar_lextypes.RecWkStart v :: r ->  set_wkstart v; loop r
  | ICalendar_lextypes.RecX v :: r ->  set_bytext v; loop r
  | [] -> ()
  in
  loop l;
  r
  
let mk_xprop (s, l, s') =
  let l' = mk_allparam l in
  let res = (s, l', s') in
  if validate_xprop res then res
  else begin
    print_xprop print_string res;
    failwith "XPROP creation failed"
  end
  
let mk_calobj l comps = 
  let calobj = 
    { calprops = 
      { prodid   = [], "";
        version  = [], "";
        calscale = None;
        imethod  = None;
        xprop   = [];
      };
      components = comps;
    }
  in
  let prodid_set = ref false in
  let version_set = ref false in
  let set_prodid p = 
    if !prodid_set then failwith "setting PRODID twice"
    else begin
      calobj.calprops.prodid <- p; 
      prodid_set := true
    end
  in 
  let set_version v = 
    if !version_set then failwith "setting VERSION twice"
    else begin
      calobj.calprops.version <- v; 
      version_set := true
    end
  in
  let set_calscale c =
    match calobj.calprops.calscale with
    | Some _ -> failwith "setting CALSCALE twice"
    | None -> calobj.calprops.calscale <- Some c
  in
  let set_method m =
    match calobj.calprops.imethod with
    | Some _ -> failwith "setting METHOD twice"
    | None -> calobj.calprops.imethod <- Some m
  in
  let set_xprop p =
    calobj.calprops.xprop <- p :: calobj.calprops.xprop
  in
  let rec loop = function
    | Prodid p   :: r -> set_prodid p; loop r 
    | Version v  :: r -> set_version v; loop r 
    | Calscale c :: r -> set_calscale c; loop r 
    | Method m   :: r -> set_method m; loop r 
    | Xprop p    :: r -> set_xprop p; loop r 
    | [] -> ()
  in
  loop l;
  calobj.calprops.xprop <- List.rev calobj.calprops.xprop;
  if !prodid_set && !version_set then calobj
  else failwith "PRODID or VERSION is not set"

let mk_compprop l = 
  let ep = no_comp_prop () in
  let set_class c =
    match ep.comp_class with
    | Some _ -> failwith "setting CLASS twice"
    | None -> ep.comp_class <- Some c
  in
  let set_created (xpl, d) =
    match ep.comp_created with
    | Some _ -> failwith "setting CREATED twice"
    | None -> ep.comp_created <- Some (xpl, d)
  in
  let set_description d =
    match ep.comp_description with
    | Some _ -> failwith "setting DESCRIPTION twice"
    | None -> ep.comp_description <- Some d
  in
  let set_dtstart dt =
    match ep.comp_dtstart with
    | Some _ -> failwith "setting DTSTART twice"
    | None -> ep.comp_dtstart <- Some dt
  in
  let set_geo g =
    match ep.comp_geo with
    | Some _ -> failwith "setting GEO twice"
    | None -> ep.comp_geo <- Some g
  in
  let set_lastmod (xpl, d) =
    match ep.comp_lastmod with
    | Some _ -> failwith "setting LAST-MODIFIED twice"
    | None -> ep.comp_lastmod <- Some (xpl, d)
  in
  let set_location l =
    match ep.comp_location with
    | Some _ -> failwith "setting LOCATION twice"
    | None -> ep.comp_location <- Some l
  in
  let set_organizer o =
    match ep.comp_organizer with
    | Some _ -> failwith "setting ORGANIZER twice"
    | None -> ep.comp_organizer <- Some o
  in
  let set_priority p =
    match ep.comp_priority with
    | Some _ -> failwith "setting PRIORITY twice"
    | None -> ep.comp_priority <- Some p
  in
  let set_dtstamp dts =
    match ep.comp_dtstamp with
    | Some _ -> failwith "setting DTSTAMP twice"
    | None -> ep.comp_dtstamp <- Some dts
  in
  let set_sequence s =
    match ep.comp_seq with
    | Some _ -> failwith "setting SEQUENCE twice"
    | None -> ep.comp_seq <- Some s
  in
  let set_status s =
    match ep.comp_status with
    | Some _ -> failwith "setting STATUS twice"
    | None -> ep.comp_status <- Some s
  in
  let set_summary s =
    match ep.comp_summary with
    | Some _ -> failwith ("setting SUMMARY twice, second time is: "^s.sumtext)
    | None -> ep.comp_summary <- Some s
  in
  let set_transp t =
    match ep.comp_transp with
    | Some _ -> failwith "setting TRANSP twice"
    | None -> ep.comp_transp <- Some t
  in
  let set_uid u =
    match ep.comp_uid with
    | Some _ -> failwith "setting UID twice"
    | None -> ep.comp_uid <- Some u
  in
  let set_url u =
    match ep.comp_url with
    | Some _ -> failwith "setting URL twice"
    | None -> ep.comp_url <- Some u
  in
  let set_recurid r =
    match ep.comp_recurid with
    | Some _ -> failwith "setting RECURRENCE-ID twice"
    | None -> ep.comp_recurid <- Some r
  in
  let set_dtend dt =
    match ep.comp_dtend with
    | Some _ -> failwith "setting DTEND twice"
    | None -> ep.comp_dtend <- Some dt
  in
  let set_duration d =
    match ep.comp_duration with
    | Some _ -> failwith "setting DURATION twice"
    | None -> ep.comp_duration <- Some d
  in
  let set_attach a =
    ep.comp_attach <- a :: ep.comp_attach
  in
  let set_attendee a =
    ep.comp_attendee <- a :: ep.comp_attendee
  in
  let set_categories c =
    ep.comp_categories <- c :: ep.comp_categories
  in
  let set_comment c =
    ep.comp_comment <- c :: ep.comp_comment
  in
  let set_contact c =
    ep.comp_contact <- c :: ep.comp_contact
  in
  let set_exdate e =
    ep.comp_exdate <- e :: ep.comp_exdate
  in
  let set_exrule e =
    ep.comp_exrule <- e :: ep.comp_exrule
  in
  let set_rstatus r =
    ep.comp_rstatus <- r :: ep.comp_rstatus
  in
  let set_related_to r =
    ep.comp_related_to <- r :: ep.comp_related_to
  in
  let set_resources r =
    ep.comp_resources <- r :: ep.comp_resources
  in
  let set_rdate e =
    ep.comp_rdate <- e :: ep.comp_rdate
  in
  let set_rrule e =
    ep.comp_rrule <- e :: ep.comp_rrule
  in
  let set_xprop e =
    ep.comp_xprop <- e :: ep.comp_xprop
  in
  let set_action d =
    match ep.comp_action with
    | Some _ -> failwith "setting ACTION twice"
    | None -> ep.comp_action <- Some d
  in
  let set_trigger t =
    match ep.comp_trigger with
    | Some _ -> failwith "setting TRIGGER twice"
    | None -> ep.comp_trigger <- Some t
  in
  let set_repeat r =
    match ep.comp_repeat with
    | Some _ -> failwith "setting REPEAT twice"
    | None -> ep.comp_repeat <- Some r
  in
  let set_tzid t =
    match ep.comp_tzid with
    | Some _ -> failwith "setting TZID twice"
    | None -> ep.comp_tzid <- Some t
  in
  let set_tzurl t =
    match ep.comp_tzurl with
    | Some _ -> failwith "setting TZURL twice"
    | None -> ep.comp_tzurl <- Some t
  in
  let set_daylightc e =
    ep.comp_daylightc <- e :: ep.comp_daylightc
  in
  let set_standardc e =
    ep.comp_standardc <- e :: ep.comp_standardc
  in
  let set_tzoffsetto t =
    match ep.comp_tzoffsetto with
    | Some _ -> failwith "setting TZOFFSETTO twice"
    | None -> ep.comp_tzoffsetto <- Some t
  in
  let set_tzoffsetfrom t =
    match ep.comp_tzoffsetfrom with
    | Some _ -> failwith "setting TZOFFSETFROM twice"
    | None -> ep.comp_tzoffsetfrom <- Some t
  in
  let set_tzname t =
    ep.comp_tzname <- t :: ep.comp_tzname
  in
  let rec loop = function
    | Class c :: r -> set_class c; loop r
    | Created c :: r -> set_created c; loop r
    | Description d :: r -> set_description d; loop r
    | DTstart dt :: r -> set_dtstart dt; loop r
    | Geo g :: r -> set_geo g; loop r
    | LastModified l :: r -> set_lastmod l; loop r
    | Location l :: r -> set_location l; loop r
    | Organizer o :: r -> set_organizer o; loop r
    | Priority p :: r -> set_priority p; loop r
    | Dtstamp d :: r -> set_dtstamp d; loop r
    | Sequence s :: r -> set_sequence s; loop r
    | Status s :: r -> set_status s; loop r
    | Summary s :: r -> set_summary s; loop r
    | Transp t :: r -> set_transp t; loop r
    | Uid u :: r -> set_uid u; loop r
    | Url u :: r -> set_url u; loop r
    | Recurid re :: r -> set_recurid re; loop r
    | DTend dt :: r -> set_dtend dt; loop r
    | Duration d :: r -> set_duration d; loop r
    | Attach a :: r -> set_attach a; loop r
    | Attendee a :: r -> set_attendee a; loop r
    | Categories c :: r -> set_categories c; loop r
    | Comment c :: r -> set_comment c; loop r
    | Contact c :: r -> set_contact c; loop r
    | Exdate e :: r -> set_exdate e; loop r
    | Exrule e :: r -> set_exrule e; loop r
    | Rstatus e :: r -> set_rstatus e; loop r
    | Related_to e :: r -> set_related_to e; loop r
    | Resources e :: r -> set_resources e; loop r
    | Rdate e :: r -> set_rdate e; loop r
    | Rrule e :: r -> set_rrule e; loop r
    | CXprop e :: r -> set_xprop e; loop r
    | Action e :: r -> set_action e; loop r
    | Trigger t :: r -> set_trigger t; loop r
    | Repeat t :: r -> set_repeat t; loop r
    | Tzid t :: r -> set_tzid t; loop r
    | Tzurl t :: r -> set_tzurl t; loop r
    | Daylightc t :: r -> set_daylightc t; loop r
    | Standardc t :: r -> set_standardc t; loop r
    | Tzoffsetto t :: r -> set_tzoffsetto t; loop r
    | Tzoffsetfrom t :: r -> set_tzoffsetfrom t; loop r
    | Tzname t :: r -> set_tzname t; loop r
    | [] -> ()
  in loop l;
  ep.comp_attach <- List.rev ep.comp_attach;
  ep.comp_attendee <- List.rev ep.comp_attendee;
  ep.comp_categories <- List.rev ep.comp_categories;
  ep.comp_comment <- List.rev ep.comp_comment;
  ep.comp_contact <- List.rev ep.comp_contact;
  ep.comp_exdate <- List.rev ep.comp_exdate;
  ep.comp_exrule <- List.rev ep.comp_exrule;
  ep.comp_rstatus <- List.rev ep.comp_rstatus;
  ep.comp_related_to <- List.rev ep.comp_related_to;
  ep.comp_resources <- List.rev ep.comp_resources;
  ep.comp_rdate <- List.rev ep.comp_rdate;
  ep.comp_rrule <- List.rev ep.comp_rrule;
  ep.comp_daylightc <- List.rev ep.comp_daylightc;
  ep.comp_standardc <- List.rev ep.comp_standardc;
  ep.comp_tzname <- List.rev ep.comp_tzname;
  ep.comp_xprop <- List.rev ep.comp_xprop;
  ep

let mk_eventprop e =
  let res = mk_compprop e in
  if validate_eventprop res then res (* we duplicate some work here. Bad. *)
  else failwith "VEVENT creation failed"
  
let mk_alarmc a =
  let res = mk_compprop a in
  if validate_alarm res then res (* we duplicate some work here. Bad. *)
  else failwith "VALARM creation failed"
  
let mk_timezonec t =
  let res = mk_compprop t in
  if validate_timezone res then res (* we duplicate some work here. Bad. *)
  else failwith "VTIMEZONE creation failed"

let mk_tzprop t =
  let res = mk_compprop t in
  if validate_tzprop res then res (* we duplicate some work here. Bad. *)
  else failwith "DAYLIGHT or STANDARD creation failed"

let mk_description (l,t) =
  let res = { descparam = mk_allparam l; desctext = t; } in
  if (validate_description (Some res)) then res
  else begin
    print_description print_string (Some res);
    failwith "DESCRIPTION creation failed"
  end

let mk_location (l,t) =
  let res = { locparam = mk_allparam l; loctext = t; } in
  if (validate_location (Some res)) then res
  else begin
    print_location print_string (Some res);
    failwith "LOCATION creation failed"
  end

let mk_dt s (l,v) =
  let params = mk_allparam l in
  let res = { dtparam = params; dtval = v } in
  if validate_dt (Some res) then res
  else begin
    print_dt print_string s (Some res);
    failwith (s^" creation failed")
  end

let mk_organizer (l,v) =
  let res = { orgparam = mk_allparam l; org_cal_address = v; } in
  if (validate_organizer (Some res)) then res
  else begin
    print_organizer print_string (Some res);
    failwith "ORGANIZER creation failed"
  end

let mk_priority p =
  if validate_priority (Some p) then p
  else begin
    print_priority print_string (Some p);
    failwith "PRIORITY creation failed"
  end

let mk_event_status s =
  if validate_event_status (Some s) then s
  else begin
    print_status print_string (Some s);
    failwith "STATUS for event creation failed"
  end

let mk_summary (p,t) =
  let res = { sumparam = mk_allparam p; sumtext = t; } in
  if (validate_summary (Some res)) then res
  else begin
    print_summary print_string (Some res);
    failwith "SUMMARY creation failed"
  end

let mk_recurid (l,v) =
  let params = mk_allparam l in
  let res = { recurparam = params; recurval = v } in
  if validate_recurid (Some res) then res
  else begin
    print_recurid print_string (Some res);
    failwith "RECURRENCE-ID creation failed"
  end

let mk_duration d =
  if (validate_duration (Some d)) then d
  else begin
    print_duration print_string (Some d);
    failwith "DURATION creation failed"
  end

let mk_attach (p,v) =
  let res = { attachparam = mk_allparam p; attachval = v; } in
  if (validate_attach [res]) then res
  else begin
    print_attach print_string [res];
    failwith "ATTACH creation failed"
  end

let mk_attendee (p,v) =
  let res = { attendeeparam = mk_allparam p; attendeeval = v; } in
  if (validate_attendee [res]) then res
  else begin
    print_attendee print_string [res];
    failwith "ATTENDEE creation failed"
  end

let mk_categories (p,v) =
  let res = { categoriesparam = mk_allparam p; categoriesval = v; } in
  if (validate_categories [res]) then res
  else begin
    print_categories print_string [res];
    failwith "CATEGORIES creation failed"
  end

let mk_comment (p,t) =
  let res = { commentparam = mk_allparam p; commenttext = t; } in
  if (validate_comment [res]) then res
  else begin
    print_comment print_string [res];
    failwith "COMMENT creation failed"
  end

let mk_contact (p,t) =
  let res = { contactparam = mk_allparam p; contacttext = t; } in
  if (validate_contact [res]) then res
  else begin
    print_contact print_string [res];
    failwith "CONTACT creation failed"
  end

let mk_dtl s allow_period (p,l) =
  let params = mk_allparam p in
  let res = { dtplparam = params; dtplval = l }
  in
  if validate_dtpl allow_period [res] then res
  else begin
    print_dtpl print_string s [res];
    failwith (s^" creation failed")
  end

let mk_rule s (x, l) =
  let recu = mk_recur l in
  if validate_recur recu then (x, recu)
  else begin
    print_rule print_string s [x,recu];
    failwith (s^" creation failed")
  end

let mk_rstatus (p,sc,st,ed) =
  let res = { rstatparam = mk_allparam p; statcode = sc; stattext = st; extdata = ed } in
  if (validate_rstatus [res]) then res
  else begin
    print_rstatus print_string [res];
    failwith "REQUEST-STATUS creation failed"
  end

let mk_related_to (p,t) =
  let res = { relatedtoparam = mk_allparam p; relatedtotext = t; } in
  if (validate_related_to [res]) then res
  else begin
    print_related_to print_string [res];
    failwith "RELATED-TO creation failed"
  end

let mk_resources (p,l) =
  let res = { resourcesparam = mk_allparam p; resourcesval = l; } in
  if (validate_resources [res]) then res
  else begin
    print_resources print_string [res];
    failwith "RESOURCES creation failed"
  end

let mk_trigger (p,v) =
  let res = { triggerparam = mk_allparam p; triggerval = v; } in
  if (validate_trigger res) then res
  else begin
    print_trigger print_string (Some res);
    failwith "TRIGGER creation failed"
  end

let mk_offset s (l,v) =
  if validate_tzoffset (Some (l, v)) then (l, v)
  else begin
    print_offset print_string s (Some (l,v));
    failwith (s^" creation failed")
  end

let mk_tzname (p,t) =
  let res = { tznameparam = mk_allparam p; tznameval = t; } in
  if (validate_tzname [res]) then res
  else begin
    print_tzname print_string [res];
    failwith "TZNAME creation failed"
  end
# 835 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
let yytransl_const = [|
  257 (* BVCALENDAR *);
  258 (* EVCALENDAR *);
  259 (* BVEVENT *);
  260 (* EVEVENT *);
  261 (* BVALARM *);
  262 (* EVALARM *);
  263 (* BVTIMEZONE *);
  264 (* EVTIMEZONE *);
  265 (* BSTANDARD *);
  266 (* ESTANDARD *);
  267 (* BDAYLIGHT *);
  268 (* EDAYLIGHT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  269 (* PRODID *);
  270 (* VERSION *);
  271 (* CALSCALE *);
  272 (* METHOD *);
  273 (* XPROP *);
  274 (* CLASS *);
  275 (* CREATED *);
  276 (* DESCRIPTION *);
  277 (* DTSTART *);
  278 (* GEO *);
  279 (* LASTMODIFIED *);
  280 (* LOCATION *);
  281 (* ORGANIZER *);
  282 (* PRIORITY *);
  283 (* DTSTAMP *);
  284 (* SEQUENCE *);
  285 (* STATUS *);
  286 (* SUMMARY *);
  287 (* TRANSP *);
  288 (* UID *);
  289 (* URL *);
  290 (* RECURRENCEID *);
  291 (* DTEND *);
  292 (* DURATION *);
  293 (* ATTACH *);
  294 (* ATTENDEE *);
  295 (* CATEGORIES *);
  296 (* COMMENT *);
  297 (* CONTACT *);
  298 (* EXDATE *);
  299 (* EXRULE *);
  300 (* RSTATUS *);
  301 (* RELATED_TO *);
  302 (* RESOURCES *);
  303 (* RDATE *);
  304 (* RRULE *);
  305 (* ACTION *);
  306 (* TRIGGER *);
  307 (* REPEAT *);
  308 (* TZID *);
  309 (* TZURL *);
  310 (* TZOFFSETTO *);
  311 (* TZOFFSETFROM *);
  312 (* TZNAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\006\000\006\000\
\006\000\006\000\006\000\007\000\005\000\005\000\008\000\008\000\
\009\000\011\000\011\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\012\000\012\000\010\000\
\014\000\015\000\000\000"

let yylen = "\002\000\
\001\000\002\000\000\000\005\000\002\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\000\000\003\000\003\000\
\002\000\002\000\000\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\004\000\000\000\001\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\067\000\001\000\000\000\007\000\008\000\
\009\000\010\000\012\000\000\000\000\000\011\000\002\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\061\000\000\000\000\000\
\000\000\059\000\060\000\000\000\064\000\000\000\013\000\000\000\
\000\000\015\000\000\000\017\000\018\000\016\000\004\000\065\000\
\066\000\000\000\000\000\062\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\018\000\013\000\062\000\019\000\
\063\000\068\000\064\000\076\000\065\000\066\000\067\000"

let yysindex = "\016\000\
\017\255\000\000\008\255\000\000\000\000\017\255\000\000\000\000\
\000\000\000\000\000\000\006\255\008\255\000\000\000\000\042\255\
\042\255\024\255\006\255\000\000\042\255\042\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\255\022\255\
\042\255\000\000\000\000\020\255\000\000\029\000\000\000\021\255\
\018\255\000\000\042\255\000\000\000\000\000\000\000\000\000\000\
\000\000\026\255\022\255\000\000"

let yyrindex = "\000\000\
\033\000\000\000\013\255\000\000\000\000\033\000\000\000\000\000\
\000\000\000\000\000\000\032\255\013\255\000\000\000\000\254\254\
\027\255\000\000\032\255\000\000\028\255\025\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\255\
\002\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\255\000\000"

let yygindex = "\000\000\
\000\000\034\000\000\000\028\000\023\000\000\000\254\255\000\000\
\000\000\000\000\239\255\216\255\000\000\000\000\000\000"

let yytablesize = 98
let yytable = "\069\000\
\014\000\019\000\019\000\072\000\073\000\019\000\019\000\019\000\
\016\000\019\000\014\000\019\000\017\000\019\000\006\000\006\000\
\001\000\003\000\074\000\006\000\007\000\008\000\009\000\010\000\
\011\000\070\000\075\000\078\000\079\000\081\000\080\000\083\000\
\003\000\014\000\019\000\019\000\019\000\019\000\063\000\015\000\
\020\000\071\000\084\000\000\000\000\000\000\000\000\000\077\000\
\000\000\000\000\021\000\000\000\022\000\000\000\000\000\000\000\
\000\000\082\000\011\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000"

let yycheck = "\017\000\
\003\000\004\001\005\001\021\000\022\000\004\001\005\001\006\001\
\003\001\008\001\013\000\010\001\007\001\012\001\002\001\003\001\
\001\000\001\001\004\001\007\001\013\001\014\001\015\001\016\001\
\017\001\002\001\005\001\008\001\000\000\012\001\010\001\006\001\
\000\000\002\001\008\001\006\001\012\001\010\001\004\001\006\000\
\013\000\019\000\083\000\255\255\255\255\255\255\255\255\065\000\
\255\255\255\255\009\001\255\255\011\001\255\255\255\255\255\255\
\255\255\075\000\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\048\001\049\001\050\001\051\001\052\001\053\001\054\001\
\055\001\056\001"

let yynames_const = "\
  BVCALENDAR\000\
  EVCALENDAR\000\
  BVEVENT\000\
  EVEVENT\000\
  BVALARM\000\
  EVALARM\000\
  BVTIMEZONE\000\
  EVTIMEZONE\000\
  BSTANDARD\000\
  ESTANDARD\000\
  BDAYLIGHT\000\
  EDAYLIGHT\000\
  EOF\000\
  "

let yynames_block = "\
  PRODID\000\
  VERSION\000\
  CALSCALE\000\
  METHOD\000\
  XPROP\000\
  CLASS\000\
  CREATED\000\
  DESCRIPTION\000\
  DTSTART\000\
  GEO\000\
  LASTMODIFIED\000\
  LOCATION\000\
  ORGANIZER\000\
  PRIORITY\000\
  DTSTAMP\000\
  SEQUENCE\000\
  STATUS\000\
  SUMMARY\000\
  TRANSP\000\
  UID\000\
  URL\000\
  RECURRENCEID\000\
  DTEND\000\
  DURATION\000\
  ATTACH\000\
  ATTENDEE\000\
  CATEGORIES\000\
  COMMENT\000\
  CONTACT\000\
  EXDATE\000\
  EXRULE\000\
  RSTATUS\000\
  RELATED_TO\000\
  RESOURCES\000\
  RDATE\000\
  RRULE\000\
  ACTION\000\
  TRIGGER\000\
  REPEAT\000\
  TZID\000\
  TZURL\000\
  TZOFFSETTO\000\
  TZOFFSETFROM\000\
  TZNAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'icalendarlist) in
    Obj.repr(
# 830 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( _1 )
# 1069 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : ICalendar_syntax.icalendar))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'icalobject) in
    let _2 = (peek_val parser_env 0 : 'icalendarlist) in
    Obj.repr(
# 834 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( _1 :: _2 )
# 1077 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'icalendarlist))
; (fun parser_env ->
    Obj.repr(
# 836 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( [] )
# 1083 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'icalendarlist))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'calprops) in
    let _3 = (peek_val parser_env 2 : 'components) in
    Obj.repr(
# 840 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( mk_calobj _2  _3 )
# 1091 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'icalobject))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'calprop) in
    let _2 = (peek_val parser_env 0 : 'calprops) in
    Obj.repr(
# 844 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( _1 :: _2 )
# 1099 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprops))
; (fun parser_env ->
    Obj.repr(
# 846 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( [] )
# 1105 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprops))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * string ) in
    Obj.repr(
# 850 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Prodid _1 )
# 1112 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * string ) in
    Obj.repr(
# 852 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Version _1 )
# 1119 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.calvalue ) in
    Obj.repr(
# 854 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Calscale _1 )
# 1126 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * string ) in
    Obj.repr(
# 856 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Method _1 )
# 1133 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'xprop) in
    Obj.repr(
# 858 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Xprop _1 )
# 1140 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'calprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  string * (ICalendar_lextypes.all_params list) * string ) in
    Obj.repr(
# 862 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( mk_xprop _1 )
# 1147 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'xprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'component) in
    let _2 = (peek_val parser_env 0 : 'components) in
    Obj.repr(
# 866 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( _1 :: _2 )
# 1155 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'components))
; (fun parser_env ->
    Obj.repr(
# 868 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( [] )
# 1161 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'components))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'eventc) in
    Obj.repr(
# 872 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Eventc _2 )
# 1168 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'component))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'timezonec) in
    Obj.repr(
# 874 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Timezonec _2 )
# 1175 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'component))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'allprops) in
    let _2 = (peek_val parser_env 0 : 'alarmcs) in
    Obj.repr(
# 878 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( {event_comp = mk_eventprop _1; event_alarms = _2} )
# 1183 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'eventc))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'aprop) in
    let _2 = (peek_val parser_env 0 : 'allprops) in
    Obj.repr(
# 882 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( _1 :: _2 )
# 1191 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'allprops))
; (fun parser_env ->
    Obj.repr(
# 884 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( [] )
# 1197 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'allprops))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.classval ) in
    Obj.repr(
# 888 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Class _1 )
# 1204 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.date_time ) in
    Obj.repr(
# 890 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Created _1 )
# 1211 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string ) in
    Obj.repr(
# 892 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Description (mk_description _1) )
# 1218 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval ) in
    Obj.repr(
# 894 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( DTstart (mk_dt "DTSTART" _1) )
# 1225 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ( float * float )) in
    Obj.repr(
# 896 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Geo _1 )
# 1232 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.date_time ) in
    Obj.repr(
# 898 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( LastModified _1 )
# 1239 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string ) in
    Obj.repr(
# 900 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Location (mk_location _1) )
# 1246 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string ) in
    Obj.repr(
# 902 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Organizer (mk_organizer _1) )
# 1253 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * int ) in
    Obj.repr(
# 904 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Priority (mk_priority _1) )
# 1260 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_lextypes.all_params list * ICalendar_syntax.dtpval ) in
    Obj.repr(
# 906 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Dtstamp (mk_dt "DTSTAMP" _1) )
# 1267 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * int ) in
    Obj.repr(
# 908 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Sequence _1 )
# 1274 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.status ) in
    Obj.repr(
# 910 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Status (mk_event_status _1) )
# 1281 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string ) in
    Obj.repr(
# 912 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Summary (mk_summary _1) )
# 1288 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.transvalue ) in
    Obj.repr(
# 914 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Transp _1 )
# 1295 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * string ) in
    Obj.repr(
# 916 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Uid _1 )
# 1302 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * string ) in
    Obj.repr(
# 918 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Url _1 )
# 1309 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval ) in
    Obj.repr(
# 920 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Recurid (mk_recurid _1) )
# 1316 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval ) in
    Obj.repr(
# 922 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( DTend (mk_dt "DTEND" _1) )
# 1323 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.duration ) in
    Obj.repr(
# 924 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Duration _1 )
# 1330 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * ICalendar_syntax.attachvalue ) in
    Obj.repr(
# 926 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Attach (mk_attach _1) )
# 1337 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string) in
    Obj.repr(
# 928 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Attendee (mk_attendee _1) )
# 1344 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string list) in
    Obj.repr(
# 930 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Categories (mk_categories _1) )
# 1351 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string) in
    Obj.repr(
# 932 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Comment (mk_comment _1) )
# 1358 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string) in
    Obj.repr(
# 934 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Contact (mk_contact _1) )
# 1365 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * (ICalendar_syntax.dtpval) list ) in
    Obj.repr(
# 936 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Exdate (mk_dtl "EXDATE" false _1) )
# 1372 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * (ICalendar_syntax.freq * ICalendar_lextypes.recur list) ) in
    Obj.repr(
# 938 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Exrule (mk_rule "EXRULE" _1) )
# 1379 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * int list * string * string option) in
    Obj.repr(
# 940 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Rstatus (mk_rstatus _1) )
# 1386 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string) in
    Obj.repr(
# 942 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Related_to (mk_related_to _1) )
# 1393 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * string list) in
    Obj.repr(
# 944 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Resources (mk_resources _1) )
# 1400 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  (ICalendar_lextypes.all_params list) * (ICalendar_syntax.dtpval) list ) in
    Obj.repr(
# 946 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Rdate (mk_dtl "RDATE" true _1) )
# 1407 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * (ICalendar_syntax.freq * ICalendar_lextypes.recur list) ) in
    Obj.repr(
# 948 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Rrule (mk_rule "RRULE" _1) )
# 1414 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.action ) in
    Obj.repr(
# 950 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Action _1 )
# 1421 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_lextypes.all_params list * ICalendar_syntax.dtpval ) in
    Obj.repr(
# 952 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Trigger (mk_trigger _1) )
# 1428 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * int ) in
    Obj.repr(
# 954 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Repeat _1 )
# 1435 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * bool * string ) in
    Obj.repr(
# 956 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Tzid _1 )
# 1442 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * string ) in
    Obj.repr(
# 958 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Tzurl _1 )
# 1449 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.offset_time ) in
    Obj.repr(
# 960 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Tzoffsetto (mk_offset "TZOFFSETTO" _1) )
# 1456 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_syntax.xplist * ICalendar_syntax.offset_time ) in
    Obj.repr(
# 962 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Tzoffsetfrom (mk_offset "TZOFFSETFROM" _1) )
# 1463 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 :  ICalendar_lextypes.all_params list * string ) in
    Obj.repr(
# 964 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Tzname (mk_tzname _1) )
# 1470 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'standardc) in
    Obj.repr(
# 966 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Standardc _1 )
# 1477 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'daylightc) in
    Obj.repr(
# 968 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( Daylightc _1 )
# 1484 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'xprop) in
    Obj.repr(
# 970 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( CXprop _1 )
# 1491 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'aprop))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'allprops) in
    let _4 = (peek_val parser_env 0 : 'alarmcs) in
    Obj.repr(
# 974 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( (mk_alarmc _2) :: _4 )
# 1499 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'alarmcs))
; (fun parser_env ->
    Obj.repr(
# 976 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( [] )
# 1505 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'alarmcs))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'allprops) in
    Obj.repr(
# 980 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( mk_timezonec _1 )
# 1512 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'timezonec))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'allprops) in
    Obj.repr(
# 984 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( mk_tzprop _2 )
# 1519 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'standardc))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'allprops) in
    Obj.repr(
# 988 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.mly"
                   ( mk_tzprop _2 )
# 1526 "/home/steak/devel/harmony4/src/iCalendar/iCalendarparse.ml"
               : 'daylightc))
(* Entry icalendar *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let icalendar (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : ICalendar_syntax.icalendar)
