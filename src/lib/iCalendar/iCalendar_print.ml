open ICalendar_syntax

let newline = "\n"

let print_xp prs (name, values) =
  prs name;
  prs "=";
  match values with
  | hd :: r -> prs hd; List.iter (fun s -> prs ","; prs s) r
  | _ -> assert false
  
let rec print_xplist prs = function
  | [] -> ()
  | xp :: rest ->
      prs ";";
      print_xp prs xp;
      print_xplist prs rest
  
let print_prodid prs (xpl,pidval) =
  prs "PRODID";
  print_xplist prs xpl;
  prs ":";
  prs pidval;
  prs newline
  
let print_version prs (xpl,v) =
  prs "VERSION";
  print_xplist prs xpl;
  prs ":";
  prs v;
  prs newline

let print_calvalue prs = function
  | CalGregorian -> prs "GREGORIAN"
  | CalOther o -> prs o
  
let print_calscale prs (xpl, cv) =
  prs "CALSCALE";
  print_xplist prs xpl;
  prs ":";
  print_calvalue prs cv;
  prs newline

let print_method prs (xpl, met) =
  prs "METHOD";
  print_xplist prs xpl;
  prs ":";
  prs met;
  prs newline

let print_classval prs = function
  | ClassPublic       -> prs "PUBLIC"
  | ClassPrivate      -> prs "PRIVATE"
  | ClassConfidential -> prs "CONFIDENTIAL"
  | ClassOther s      -> prs s
  
let print_class prs = function
  | None -> ()
  | Some (xpl, cv) ->
      prs "CLASS:";
      print_xplist prs xpl;
      print_classval prs cv;
      prs newline

let print_date prs {year = y; month = m; day = d} =
  prs (Printf.sprintf "%04d%02d%02d" y m d)

let print_time prs { hour = h; minute = mi; second = s; zulu = z} =
  prs (Printf.sprintf "%02d%02d%02d%s" h mi s (if z then "Z" else ""))

let print_date_time prs { date = d; time = t} =
  print_date prs d;
  prs "T";
  print_time prs t
                     
let print_dtxp prs = function
  | None -> ()
  | Some (xpl, date_time) ->
      prs "CREATED";
      print_xplist prs xpl;
      prs ":";
      print_date_time prs date_time;
      prs newline

let print_valuetype prs = function
  | DateTime -> prs "DATE-TIME"
  | Date -> prs "DATE"
  | Binary -> prs "BINARY"
  | Text -> prs "TEXT"
  | Period -> prs "PERIOD"
  | Duration -> prs "DURATION"
      
let print_encoding prs = function
  | Enc8bit -> prs "8BIT"
  | EncBase64 -> prs "BASE64"
  | EncOther s -> prs s
  
let print_cutype prs = function
  | CUIndividual -> prs "INDIVIDUAL"
  | CUGroup -> prs "GROUP"
  | CUResource -> prs "RESOURCE"
  | CURoom -> prs "ROOM"
  | CUUnknown -> prs "UNKNOWN"
  | CUOther s -> prs s
  
let rec print_quoted_list prs = function
  | [] -> ()
  | elt :: rest -> prs ",\""; prs elt; prs "\""; print_quoted_list prs rest
  
let print_role prs = function
  | RoleChair -> prs "CHAIR"
  | RoleReqParticipant -> prs "REQ-PARTICIPANT"
  | RoleOptParticipant -> prs "OPT-PARTICIPANT"
  | RoleNonParticipant -> prs "NON-PARTICIPANT"
  | RoleOther s -> prs s
  
let print_partstat prs = function
  | PSNeedsAction -> prs "NEEDS-ACTION"
  | PSAccepted -> prs "ACCEPTED"
  | PSDeclined -> prs "DECLINED"
  | PSTentative -> prs "TENTATIVE"
  | PSDelegated -> prs "DELEGATED"
  | PSCompleted -> prs "COMPLETED"
  | PSInProcess -> prs "IN-PROCESS"
  | PSOther s -> prs s
  
let print_reltypeparam prs = function
  | RelParent    -> prs "PARENT"              
  | RelChild     -> prs "CHILD"               
  | RelSibling   -> prs "SIBLING"             
  | RelOther s   -> prs s
              
let print_range prs = function
  | RangePrior -> prs "THISANDPRIOR"
  | RangeFuture -> prs "THISANDFUTURE"
  
let print_trigrel prs = function
  | RelStart -> prs "START"
  | RelEnd -> prs "END"
  
let print_allparam prs allp =
  match allp with
  | { altrepparam    = a   ; cnparam        = c   ; cutypeparam    = cu  ; delfromparam   = delf;
      deltoparam     = delt; dirparam       = dir ; encodingparam  = enc ; fmttypeparam   = fmt ;
      fbtypeparam    = None; langparam      = l   ; memberparam    = memb; partstatparam  = part;
      rangeparam     = r   ; reltypeparam   = relt; roleparam      = role; rsvpparam      = rsvp;
      sentbyparam    = s   ; trigrelparam   = trel; tzidparam      = t   ; valuetypeparam = v   ;
      xplist         = x   ; 
    } ->
    (match a with
    | None -> ()
    | Some a -> prs (Printf.sprintf ";ALTREP=\"%s\"" a));
    (match c with
    | None -> ()
    | Some s -> prs (Printf.sprintf ";CN=%s" s));
    (match cu with
    | None -> ()
    | Some p -> prs ";CUTYPE="; print_cutype prs p);
    (match delf with
    | [] -> ()
    | elt :: rest ->
        prs ";DELEGATED-FROM=\"";
        prs elt;
        prs "\"";
        print_quoted_list prs rest
    );
    (match delt with
    | [] -> ()
    | elt :: rest ->
        prs ";DELEGATED-TO=\"";
        prs elt;
        prs "\"";
        print_quoted_list prs rest
    );
    (match dir with
    | None -> ()
    | Some s -> prs (Printf.sprintf ";DIR=\"%s\"" s));
    (match fmt with
    | None -> ()
    | Some f -> prs (Printf.sprintf ";FMTTYPE=%s" f));
    (match memb with
    | [] -> ()
    | elt :: rest ->
        prs ";MEMBER=\"";
        prs elt;
        prs "\"";
        print_quoted_list prs rest
    );
    (match part with
    | None -> ()
    | Some p -> prs ";PARTSTAT="; print_partstat prs p);
    (match r with
    | None -> ()
    | Some s -> prs ";RANGE="; print_range prs s);
    (match relt with
    | None -> ()
    | Some r -> prs ";RELTYPE="; print_reltypeparam prs r);
    (match role with
    | None -> ()
    | Some r -> prs ";ROLE="; print_role prs r);
    (match rsvp with
    | None -> ()
    | Some true -> prs ";RSVP=TRUE"
    | Some false -> prs ";RSVP=FALSE");
    (match s with
    | None -> ()
    | Some s -> prs (Printf.sprintf ";SENT-BY=\"%s\"" s));
    (match t with
    | None -> ()
    | Some (b,s) -> prs (Printf.sprintf ";TZID=%s%s" (if b then "/" else "") s));
    (match trel with
    | None -> ()
    | Some r -> prs ";RELATED="; print_trigrel prs r);
    print_xplist prs x;
    (* these are printed at the end to satisfy some rfc requirement for xprop
       (language at the end) and attach (enc followed by valuetype at the end *)
    (match enc with
    | None -> ()
    | Some e -> prs ";ENCODING="; print_encoding prs e);
    (match v with
    | None -> ()
    | Some vt -> prs ";VALUE="; print_valuetype prs vt);
    (match l with
    | None -> ()
    | Some l -> prs (Printf.sprintf ";LANGUAGE=%s" l));
  | _ -> assert false
 
let print_xprop prs (name, par, txt) =
  prs name;
  print_allparam prs par;
  prs ":";
  prs txt;
  prs newline
  
let print_xprop_list prs = List.iter (print_xprop prs)
  
let print_calprops prs cp =
  print_prodid prs cp.prodid;
  print_version prs cp.version;
  (match (cp.calscale) with
  | None -> ()
  | Some cal -> print_calscale prs cal);
  (match (cp.imethod) with
  | None -> ()
  | Some met -> print_method prs met);
  print_xprop_list prs cp.xprop

let print_description prs = function
  | None -> ()
  | Some { descparam = dp; desctext = s} ->
      prs "DESCRIPTION";
      print_allparam prs dp;
      prs ":";
      prs s;
      prs newline

let print_dur_time prs { dur_hour = h; dur_minute = m; dur_second = s; } =
  prs "T";
  prs (string_of_int h);
  prs "H";
  prs (string_of_int m);
  prs "M";
  prs (string_of_int s);
  prs "S"

let print_dur_length prs = function
  | DurWeek w -> prs (string_of_int w); prs "W"
  | DurTime t -> print_dur_time prs t
  | DurDate (d, t) -> prs (string_of_int d); prs "D"; (match t with None -> () | Some t -> print_dur_time prs t)

let print_durval prs { dur_neg = dn; dur_length = dl } =
  if dn then prs "-P" else prs "P";
  print_dur_length prs dl

let print_period prs = function
  | PeriodExplicit (dts, dte) ->
      print_date_time prs dts;
      prs "/";
      print_date_time prs dte
  | PeriodStart (dts, dur) ->
      print_date_time prs dts;
      prs "/";
      print_durval prs dur
      
let print_dtval prs = function
  | DateTimeVal d -> print_date_time prs d
  | DateVal d -> print_date prs d
  | PeriodVal p -> print_period prs p
  | DurationVal d -> print_durval prs d
      
let print_dt prs s = function
  | None -> ()
  | Some { dtparam = p; dtval = v;} ->
      prs s;
      print_allparam prs p;
      prs ":";
      print_dtval prs v;
      prs newline

let print_geo prs = function
  | None -> ()
  | Some (xpl, (f1, f2)) ->
      prs "GEO";
      print_xplist prs xpl;
      prs ":";
      prs (string_of_float f1);
      prs ";";
      prs (string_of_float f2);
      prs newline
        
let print_lastmod prs = function
  | None -> ()
  | Some (xpl, date_time) ->
      prs "LAST-MODIFIED";
      print_xplist prs xpl;
      prs ":";
      print_date_time prs date_time;
      prs newline

let print_location prs = function
  | None -> ()
  | Some { locparam = dp; loctext = s} ->
      prs "LOCATION";
      print_allparam prs dp;
      prs ":";
      prs s;
      prs newline

let print_organizer prs = function
  | None -> ()
  | Some { orgparam = p; org_cal_address = a} ->
      prs "ORGANIZER";
      print_allparam prs p;
      prs ":";
      prs a;
      prs newline

let print_priority prs = function
  | None -> ()
  | Some (xpl, p) ->
      prs "PRIORITY";
      print_xplist prs xpl;
      prs ":";
      prs (string_of_int p);
      prs newline
      
let print_dtstamp prs = function
  | None -> ()
  | Some (xpl, date_time) ->
      prs "DTSTAMP";
      print_xplist prs xpl;
      prs ":";
      print_date_time prs date_time;
      prs newline

let print_sequence prs = function
  | None -> ()
  | Some (xpl, s) ->
      prs "SEQUENCE";
      print_xplist prs xpl;
      prs ":";
      prs (string_of_int s);
      prs newline

let print_status_val prs = function
  | STTentative -> prs "TENTATIVE"
  | STConfirmed -> prs "CONFIRMED"
  | STCancelled -> prs "CANCELLED"
  | STNeedsAction -> prs "NEEDS-ACTION"
  | STCompleted -> prs "COMPLETED"
  | STInProcess -> prs "IN-PROCESS"
  | STDraft -> prs "DRAFT"
  | STFinal -> prs "FINAL"

let print_status prs = function
  | None -> ()
  | Some (xpl, st) ->
      prs "STATUS";
      print_xplist prs xpl;
      prs ":";
      print_status_val prs st;
      prs newline

let print_summary prs = function
  | None -> ()
  | Some { sumparam = p; sumtext = s} ->
      prs "SUMMARY";
      print_allparam prs p;
      prs ":";
      prs s;
      prs newline

let print_transvalue prs = function
  | Transparent -> prs "TRANSPARENT"
  | Opaque -> prs "OPAQUE"

let print_transp prs = function
  | None -> ()
  | Some (xpl, t) ->
      prs "TRANSP";
      print_xplist prs xpl;
      prs ":";
      print_transvalue prs t;
      prs newline
      
let print_uid prs = function
  | None -> ()
  | Some (xpl,u) ->
      prs "UID";
      print_xplist prs xpl;
      prs ":";
      prs u;
      prs newline

let print_url prs = function
  | None -> ()
  | Some (xpl,u) ->
      prs "URL";
      print_xplist prs xpl;
      prs ":";
      prs u;
      prs newline

let print_recurid prs = function
  | None -> ()
  | Some { recurparam = p; recurval = v;} ->
      prs "RECURRENCE-ID";
      print_allparam prs p;
      prs ":";
      print_dtval prs v;
      prs newline

let print_duration prs = function
  | None -> ()
  | Some (xpl,d) ->
      prs "DURATION";
      print_xplist prs xpl;
      prs ":";
      print_durval prs d;
      prs newline
      
let print_attval prs = function
  | AttBinary b -> prs b
  | AttUri u -> prs u
      
let print_attach prs = List.iter ( fun { attachparam = p; attachval = v } ->
      prs "ATTACH";
      print_allparam prs p;
      prs ":";
      print_attval prs v;
      prs newline
    )

let print_attendee prs = List.iter ( fun { attendeeparam = p; attendeeval = v } ->
      prs "ATTENDEE";
      print_allparam prs p;
      prs ":";
      prs v;
      prs newline
    )

let print_categories prs = List.iter ( fun { categoriesparam = p; categoriesval = v } ->
      prs "CATEGORIES";
      print_allparam prs p;
      prs ":";
      (match v with
      | elt :: rest -> prs elt; List.iter (fun e -> prs ","; prs e) rest
      | _ -> assert false);
      prs newline
    )

let print_comment prs = List.iter ( fun { commentparam = p; commenttext = t } ->
      prs "COMMENT";
      print_allparam prs p;
      prs ":";
      prs t;
      prs newline
    )

let print_contact prs = List.iter ( fun { contactparam = p; contacttext = t } ->
      prs "CONTACT";
      print_allparam prs p;
      prs ":";
      prs t;
      prs newline
    )

let print_dtpl prs s = List.iter (fun { dtplparam = p; dtplval = l;} ->
      prs s;
      print_allparam prs p;
      prs ":";
      (match l with
      | elt :: rest -> print_dtval prs elt; List.iter (fun e -> prs ","; print_dtval prs e) rest
      | _ -> assert false);
      prs newline
    )

let print_freq prs = function
  | Secondly -> prs "SECONDLY"
  | Minutely -> prs "MINUTELY"
  | Hourly -> prs "HOURLY"
  | Daily -> prs "DAILY"
  | Weekly -> prs "WEEKLY"
  | Monthly -> prs "MONTHLY"
  | Yearly -> prs "YEARLY"

let print_weekday prs = function
  | Sunday    -> prs "SU"
  | Monday    -> prs "MO"
  | Tuesday   -> prs "TU"
  | Wednesday -> prs "WE"
  | Thursday  -> prs "TH"
  | Friday    -> prs "FR"
  | Saturday  -> prs "SA"

let print_rend prs = function
  | RecUntil d -> prs ";UNTIL="; print_dtval prs d
  | RecCount i -> prs ";COUNT="; prs (string_of_int i)
  | RecNone -> ()
  
let print_wdaynum prs = function
  | None, w -> print_weekday prs w
  | Some i, w -> prs (string_of_int i); print_weekday prs w
  
let print_recur prs = function 
  { recur_freq       = rfreq   ; recur_end        = rend; recur_interval   = rint; recur_bysec      = rsec; 
    recur_bymin      = rmin; recur_byhour     = rhour; recur_byday      = rday; recur_bymonthday = rmday;
    recur_byyearday  = ryday; recur_byweekno   = rwno; recur_bymonth    = rmon; recur_bysetpos   = rsetp;
    recur_wkstart    = rwkst; recur_bytext     = rtxt;
  } ->
    prs "FREQ=";
    print_freq prs rfreq;
    print_rend prs rend;
    (match rint with
    | Some i -> prs ";INTERVAL="; prs (string_of_int i)
    | None -> ());
    (match rsec with
    | [] -> ()
    | elt :: rest -> prs ";BYSECOND="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rmin with
    | [] -> ()
    | elt :: rest -> prs ";BYMINUTE="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rhour with
    | [] -> ()
    | elt :: rest -> prs ";BYHOUR="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rday with
    | [] -> ()
    | elt :: rest -> prs ";BYDAY="; print_wdaynum prs elt; List.iter (fun e -> prs ","; print_wdaynum prs e) rest) ;
    (match rmday with
    | [] -> ()
    | elt :: rest -> prs ";BYMONTHDAY="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match ryday with
    | [] -> ()
    | elt :: rest -> prs ";BYYEARDAY="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rwno with
    | [] -> ()
    | elt :: rest -> prs ";BYWEEKNO="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rmon with
    | [] -> ()
    | elt :: rest -> prs ";BYMONTH="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rsetp with
    | [] -> ()
    | elt :: rest -> prs ";BYSETPOS="; prs (string_of_int elt); List.iter (fun e -> prs ","; prs (string_of_int e)) rest);
    (match rwkst with
    | Some w -> prs ";WKST="; print_weekday prs w
    | None -> ());
    (match rtxt with
    | Some (name, txt) -> prs ";"; prs name; prs "="; prs txt
    | None -> ())

let print_rule prs s = List.iter (fun (x,r) ->
  prs s;
  print_xplist prs x;
  prs ":";
  print_recur prs r;
  prs newline)
    
let print_rstatus prs = List.iter ( fun { rstatparam = p; statcode = c; stattext = t; extdata = e } ->
      prs "REQUEST-STATUS";
      print_allparam prs p;
      prs ":";
      (match c with
      | elt :: rest -> prs (string_of_int elt); List.iter (fun e -> prs "."; prs (string_of_int e)) rest
      | _ -> assert false);
      prs ";";
      prs t;
      (match e with
      | Some t -> prs ";"; prs t
      | None -> ());
      prs newline
    )

let print_related_to prs = List.iter ( fun { relatedtoparam = p; relatedtotext = t } ->
      prs "RELATED-TO";
      print_allparam prs p;
      prs ":";
      prs t;
      prs newline
    )

let print_resources prs = List.iter ( fun { resourcesparam = p; resourcesval = v } ->
      prs "RESOURCES";
      print_allparam prs p;
      prs ":";
      (match v with
      | elt :: rest -> prs elt; List.iter (fun e -> prs ","; prs e) rest
      | _ -> assert false);
      prs newline
    )

let print_actionval prs = function
| ActAudio     -> prs "AUDIO"                    
| ActDisplay   -> prs "DISPLAY"                  
| ActEmail     -> prs "EMAIL"                    
| ActProcedure -> prs "PROCEDURE"                
| ActOther s   -> prs s
  
let print_action prs = function
  | Some (xpl, act) ->
      prs "ACTION";
      print_xplist prs xpl;
      prs ":";
      print_actionval prs act;
      prs newline
  | None -> ()

let print_trigger prs = function
  | Some {triggerparam = t; triggerval = v } ->
      prs "TRIGGER";
      print_allparam prs t;
      prs ":";
      print_dtval prs v;
      prs newline
  | None -> ()
  
let print_repeat prs = function
  | Some (xpl, i) ->
      prs "REPEAT";
      print_xplist prs xpl;
      prs ":";
      prs (string_of_int i);
      prs newline
  | None -> ()
  
let print_tzid prs = function
  | Some (xpl, global, t) ->
      prs "TZID";
      print_xplist prs xpl;
      prs ":";
      if global then prs "/";
      prs t;
      prs newline
  | None -> ()

let print_tzurl prs = function
  | Some (xpl, t) ->
      prs "TZURL";
      print_xplist prs xpl;
      prs ":";
      prs t;
      prs newline
  | None -> ()


let print_offsettime prs { positive = p; off_h = h; off_m = m; off_s = s} =
  prs (Printf.sprintf "%s%02d%02d%02d" (if p then "+" else "-") h m s)

let print_offset prs s = function
  | None -> ()
  | Some (xpl, o) ->
      prs s;
      print_xplist prs xpl;
      prs ":";
      print_offsettime prs o;
      prs newline
  
let print_tzname prs = List.iter (fun { tznameparam = p; tznameval = v } ->
    prs "TZNAME";
    print_allparam prs p;
    prs ":";
    prs v;
    prs newline
  )
      
let rec print_standardc prs e =
  prs "BEGIN:STANDARD";
  prs newline;
  print_allprop prs e;
  prs "END:STANDARD";
  prs newline

and print_daylightc prs e =
  prs "BEGIN:DAYLIGHT";
  prs newline;
  print_allprop prs e;
  prs "END:DAYLIGHT";
  prs newline

and print_allprop prs e =
  print_uid prs e.comp_uid;
  print_summary prs e.comp_summary;
  print_tzid prs e.comp_tzid;
  print_class prs e.comp_class;
  print_dtxp prs e.comp_created;
  print_description prs e.comp_description;
  print_dt prs "DTSTART" e.comp_dtstart;
  print_offset prs "TZOFFSETTO" e.comp_tzoffsetto;
  print_offset prs "TZOFFSETFROM" e.comp_tzoffsetfrom;
  print_geo prs e.comp_geo;
  print_lastmod prs e.comp_lastmod;
  print_tzurl prs e.comp_tzurl;
  List.iter (print_standardc prs) e.comp_standardc;
  List.iter (print_daylightc prs) e.comp_daylightc;
  print_location prs e.comp_location;
  print_organizer prs e.comp_organizer;
  print_priority prs e.comp_priority;
  print_dt prs "DTSTAMP" e.comp_dtstamp;
  print_sequence prs e.comp_seq;
  print_status prs e.comp_status;
  print_transp prs e.comp_transp;
  print_url prs e.comp_url;
  print_recurid prs e.comp_recurid;
  print_dt prs "DTEND" e.comp_dtend;
  print_trigger prs e.comp_trigger;
  print_repeat prs e.comp_repeat;
  print_duration prs e.comp_duration;
  print_action prs e.comp_action;
  print_attach prs e.comp_attach;
  print_attendee prs e.comp_attendee;
  print_categories prs e.comp_categories;
  print_comment prs e.comp_comment;
  print_contact prs e.comp_contact;
  print_dtpl prs "EXDATE" e.comp_exdate;
  print_rule prs "EXRULE" e.comp_exrule;
  print_rstatus prs e.comp_rstatus;
  print_related_to prs e.comp_related_to;
  print_resources prs e.comp_resources;
  print_dtpl prs "RDATE" e.comp_rdate;
  print_rule prs "RRULE" e.comp_rrule;
  print_tzname prs e.comp_tzname;
  print_xprop_list prs e.comp_xprop;
  ()
  
let print_alarm prs a =
  prs "BEGIN:VALARM";
  prs newline;
  print_allprop prs a;
  prs "END:VALARM";
  prs newline
  
let print_alarm_list prs = List.iter (print_alarm prs)
  
let print_component prs = function
  | Eventc e ->
      prs "BEGIN:VEVENT\n";
      print_allprop prs e.event_comp;
      print_alarm_list prs e.event_alarms;
      prs "END:VEVENT\n"
  | Timezonec t ->
      prs "BEGIN:VTIMEZONE\n";
      print_allprop prs t;
      prs "END:VTIMEZONE\n"
  
let print_icalobject prs { calprops = cp; components = comps; } =
  prs "BEGIN:VCALENDAR\n";
  print_calprops prs cp;
  List.iter (print_component prs) comps; 
  prs "END:VCALENDAR\n"

let print_icalendar prs = List.iter (print_icalobject prs)
