%{
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
  | Completed of (xplist * date_time)
  | Created of (xplist * date_time)
  | Description of description
  | DTstart of dt
  | Geo of (xplist * (float * float))
  | LastModified of (xplist * date_time)
  | Location of location
  | Organizer of organizer
  | Percent of (xplist * int)
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
  | Due of dt
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
  let set_completed (xpl, d) =
    match ep.comp_completed with
    | Some _ -> failwith "setting COMPLETED twice"
    | None -> ep.comp_completed <- Some (xpl, d)
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
  let set_percent p =
    match ep.comp_percent with
    | Some _ -> failwith "setting PERCENT twice"
    | None -> ep.comp_percent <- Some p
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
  let set_due dt =
    match ep.comp_due with
    | Some _ -> failwith "setting DUE twice"
    | None -> ep.comp_due <- Some dt
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
    | Completed c :: r -> set_completed c; loop r
    | Created c :: r -> set_created c; loop r
    | Description d :: r -> set_description d; loop r
    | DTstart dt :: r -> set_dtstart dt; loop r
    | Geo g :: r -> set_geo g; loop r
    | LastModified l :: r -> set_lastmod l; loop r
    | Location l :: r -> set_location l; loop r
    | Organizer o :: r -> set_organizer o; loop r
    | Percent p :: r -> set_percent p; loop r
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
    | Due dt :: r -> set_due dt; loop r
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
  
let mk_todoprop e =
  let res = mk_compprop e in
  if validate_todoprop res then res (* we duplicate some work here. Bad. *)
  else failwith "VTODO creation failed"

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
%}

%token BVCALENDAR EVCALENDAR BVEVENT EVEVENT BVALARM EVALARM BVTIMEZONE EVTIMEZONE
%token BSTANDARD ESTANDARD BDAYLIGHT EDAYLIGHT BVTODO EVTODO
%token EOF
%token < ICalendar_syntax.xplist * string > PRODID
%token < ICalendar_syntax.xplist * string > VERSION
%token < ICalendar_syntax.xplist * ICalendar_syntax.calvalue > CALSCALE
%token < ICalendar_syntax.xplist * string > METHOD
%token < string * (ICalendar_lextypes.all_params list) * string > XPROP
%token < ICalendar_syntax.xplist * ICalendar_syntax.classval > CLASS
%token < ICalendar_syntax.xplist * ICalendar_syntax.date_time > COMPLETED
%token < ICalendar_syntax.xplist * ICalendar_syntax.date_time > CREATED
%token < (ICalendar_lextypes.all_params list) * string > DESCRIPTION
%token < (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval > DTSTART
%token < ICalendar_syntax.xplist * ( float * float )> GEO
%token < ICalendar_syntax.xplist * ICalendar_syntax.date_time > LASTMODIFIED
%token < (ICalendar_lextypes.all_params list) * string > LOCATION
%token < (ICalendar_lextypes.all_params list) * string > ORGANIZER
%token < ICalendar_syntax.xplist * int > PERCENT
%token < ICalendar_syntax.xplist * int > PRIORITY
%token < ICalendar_lextypes.all_params list * ICalendar_syntax.dtpval > DTSTAMP
%token < ICalendar_syntax.xplist * int > SEQUENCE
%token < ICalendar_syntax.xplist * ICalendar_syntax.status > STATUS
%token < (ICalendar_lextypes.all_params list) * string > SUMMARY
%token < ICalendar_syntax.xplist * ICalendar_syntax.transvalue > TRANSP
%token < ICalendar_syntax.xplist * string > UID
%token < ICalendar_syntax.xplist * string > URL
%token < (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval > RECURRENCEID
%token < (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval > DTEND
%token < (ICalendar_lextypes.all_params list) * ICalendar_syntax.dtpval > DUE
%token < ICalendar_syntax.xplist * ICalendar_syntax.duration > DURATION
%token < (ICalendar_lextypes.all_params list) * ICalendar_syntax.attachvalue > ATTACH
%token < (ICalendar_lextypes.all_params list) * string> ATTENDEE
%token < (ICalendar_lextypes.all_params list) * string list> CATEGORIES
%token < (ICalendar_lextypes.all_params list) * string> COMMENT
%token < (ICalendar_lextypes.all_params list) * string> CONTACT
%token < (ICalendar_lextypes.all_params list) * (ICalendar_syntax.dtpval) list > EXDATE
%token < ICalendar_syntax.xplist * (ICalendar_syntax.freq * ICalendar_lextypes.recur list) > EXRULE
%token < (ICalendar_lextypes.all_params list) * int list * string * string option> RSTATUS
%token < (ICalendar_lextypes.all_params list) * string> RELATED_TO
%token < (ICalendar_lextypes.all_params list) * string list> RESOURCES
%token < (ICalendar_lextypes.all_params list) * (ICalendar_syntax.dtpval) list > RDATE
%token < ICalendar_syntax.xplist * (ICalendar_syntax.freq * ICalendar_lextypes.recur list) > RRULE
%token < ICalendar_syntax.xplist * ICalendar_syntax.action > ACTION
%token < ICalendar_lextypes.all_params list * ICalendar_syntax.dtpval > TRIGGER
%token < ICalendar_syntax.xplist * int > REPEAT
%token < ICalendar_syntax.xplist * bool * string > TZID
%token < ICalendar_syntax.xplist * string > TZURL
%token < ICalendar_syntax.xplist * ICalendar_syntax.offset_time > TZOFFSETTO
%token < ICalendar_syntax.xplist * ICalendar_syntax.offset_time > TZOFFSETFROM
%token < ICalendar_lextypes.all_params list * string > TZNAME

%type <ICalendar_syntax.icalendar> icalendar

%start icalendar

%%
icalendar      : icalendarlist
                   { $1 }
               ;

icalendarlist  : icalobject icalendarlist
                   { $1 :: $2 }
               | /* nothing */
                   { [] }
               ;

icalobject     : BVCALENDAR calprops components EVCALENDAR EOF
                   { mk_calobj $2  $3 }
               ;

calprops       : calprop calprops
                   { $1 :: $2 }
               | /* nothing */
                   { [] }
               ;

calprop        : PRODID
                   { Prodid $1 }
               | VERSION
                   { Version $1 }
               | CALSCALE
                   { Calscale $1 }
               | METHOD
                   { Method $1 }
               | xprop
                   { Xprop $1 }
               ;

xprop          : XPROP
                   { mk_xprop $1 }
               ;

components     : component components
                   { $1 :: $2 }
               | /* nothing */
                   { [] }
               ;
               
component      : BVEVENT eventc EVEVENT
                   { Eventc $2 }
               | BVTIMEZONE timezonec EVTIMEZONE
                   { Timezonec $2 }
	       | BVTODO todoc EVTODO
		   { Todoc $2 }
               ;

eventc         : allprops alarmcs
                   { {event_comp = mk_eventprop $1; event_alarms = $2} }
	       ;

todoc          : allprops alarmcs
		   { {todo_comp = mk_todoprop $1; todo_alarms = $2} }
	       ;

allprops       : aprop allprops
                   { $1 :: $2 }
               | /* nothing */
                   { [] }
               ;
 
aprop          : CLASS
                   { Class $1 }
	       | COMPLETED
		   { Completed $1 }
               | CREATED
                   { Created $1 }
               | DESCRIPTION
                   { Description (mk_description $1) }
               | DTSTART
                   { DTstart (mk_dt "DTSTART" $1) }
               | GEO
                   { Geo $1 }
               | LASTMODIFIED
                   { LastModified $1 }
               | LOCATION
                   { Location (mk_location $1) }
               | ORGANIZER
                   { Organizer (mk_organizer $1) }
	       | PERCENT
		   { Percent $1 }
               | PRIORITY
                   { Priority (mk_priority $1) }
               | DTSTAMP
                   { Dtstamp (mk_dt "DTSTAMP" $1) }
               | SEQUENCE
                   { Sequence $1 }
               | STATUS
                   { Status (mk_event_status $1) }
               | SUMMARY
                   { Summary (mk_summary $1) }
               | TRANSP
                   { Transp $1 }
               | UID
                   { Uid $1 }
               | URL
                   { Url $1 }
               | RECURRENCEID
                   { Recurid (mk_recurid $1) }
               | DTEND
                   { DTend (mk_dt "DTEND" $1) }
	       | DUE
		   { Due (mk_dt "DUE" $1) }
               | DURATION
                   { Duration $1 }
               | ATTACH
                   { Attach (mk_attach $1) }
               | ATTENDEE
                   { Attendee (mk_attendee $1) }
               | CATEGORIES
                   { Categories (mk_categories $1) }
               | COMMENT
                   { Comment (mk_comment $1) }
               | CONTACT
                   { Contact (mk_contact $1) }
               | EXDATE
                   { Exdate (mk_dtl "EXDATE" false $1) }
               | EXRULE
                   { Exrule (mk_rule "EXRULE" $1) }
               | RSTATUS
                   { Rstatus (mk_rstatus $1) }
               | RELATED_TO
                   { Related_to (mk_related_to $1) }
               | RESOURCES
                   { Resources (mk_resources $1) }
               | RDATE
                   { Rdate (mk_dtl "RDATE" true $1) }
               | RRULE
                   { Rrule (mk_rule "RRULE" $1) }
               | ACTION
                   { Action $1 }
               | TRIGGER
                   { Trigger (mk_trigger $1) }
               | REPEAT
                   { Repeat $1 }
               | TZID
                   { Tzid $1 }
               | TZURL
                   { Tzurl $1 }
               | TZOFFSETTO
                   { Tzoffsetto (mk_offset "TZOFFSETTO" $1) }
               | TZOFFSETFROM
                   { Tzoffsetfrom (mk_offset "TZOFFSETFROM" $1) }
               | TZNAME
                   { Tzname (mk_tzname $1) }
               | standardc
                   { Standardc $1 }
               | daylightc
                   { Daylightc $1 }
               | xprop
                   { CXprop $1 }
               ;

alarmcs        : BVALARM allprops EVALARM alarmcs
                   { (mk_alarmc $2) :: $4 }
               | /* nothing */
                   { [] }
               ;

timezonec      : allprops 
                   { mk_timezonec $1 }
               ;

standardc      : BSTANDARD allprops ESTANDARD
                   { mk_tzprop $2 }
               ;

daylightc      : BDAYLIGHT allprops EDAYLIGHT
                   { mk_tzprop $2 }
               ;
