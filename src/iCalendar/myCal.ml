(* TODO:
 * change repeat exception representation from list to set
*)

open ICalendar_syntax

let compute_day_of_week y_string m_string d_string =
  let y = int_of_string y_string in
  let m = int_of_string m_string in
  let d = int_of_string d_string in
  let tm = {
    Unix.tm_sec   = 0;
    Unix.tm_min   = 0;
    Unix.tm_hour  = 0;
    Unix.tm_mday  = d;
    Unix.tm_mon   = m - 1;
    Unix.tm_year  = y - 1900;
    Unix.tm_wday  = 0;
    Unix.tm_yday  = 0;
    Unix.tm_isdst = false; } in 
  let _, tm' = Unix.mktime tm in
  match tm'.Unix.tm_wday with
  | 0 -> Sunday
  | 1 -> Monday
  | 2 -> Tuesday
  | 3 -> Wednesday
  | 4 -> Thursday
  | 5 -> Friday
  | 6 -> Saturday
  | _ -> assert false

let compute_end_date_and_time ?(years   = 0) 
                              ?(months  = 0)
                              ?(weeks   = 0)
                              ?(days    = 0)
                              ?(hours   = 0)
                              ?(minutes = 0)
                              ?(seconds = 0)
                              dt =
  let tm = {
    Unix.tm_sec   = dt.time.second + seconds;
    Unix.tm_min   = dt.time.minute + minutes;
    Unix.tm_hour  = dt.time.hour + hours;
    Unix.tm_mday  = dt.date.day + days + (weeks * 7);
    Unix.tm_mon   = dt.date.month - 1 + months;
    Unix.tm_year  = dt.date.year - 1900 + years;
    Unix.tm_wday  = 0;
    Unix.tm_yday  = 0;
    Unix.tm_isdst = false; } in 
  let _, tm' = Unix.mktime tm in
  { date = {
      year   = tm'.Unix.tm_year + 1900;
      month  = tm'.Unix.tm_mon  + 1;
      day    = tm'.Unix.tm_mday; };
    time = {
      hour   = tm'.Unix.tm_hour;
      minute = tm'.Unix.tm_min;
      second = tm'.Unix.tm_sec;
      zulu   = dt.time.zulu; }
  }

let print_illformed_exc s vl =
  (`String s) :: (`Break) :: (List.map (fun v -> `Tree v) vl)

let view_from_bool = function
  | true -> V.new_value "#true"
  | false -> V.new_value "#false"

let bool_from_view v =
  match V.get_value v with
  | "#true"  -> true
  | "#false" -> false
  | _ -> assert false
  
let viewpair_from_xplist = function
  | [] -> []
  | x -> ["xplist", 
              V.structure_from_list
                (Safelist.map ( fun (s, sl) ->
                                V.from_list [ "name", V.new_value s;
                                              "val" , V.structure_from_list 
                                                        (Safelist.map V.new_value sl)
                                            ]
                              )
                              x
                )
         ]

let xplist_from_view_opt = function
  | None -> []
  | Some v -> 
      Safelist.map
        (fun e ->
          Error.exit_on_error
	    (fun () ->
              ( V.get_value (V.get_required e "name"),
               Safelist.map V.get_value (V.list_from_structure (V.get_required e "val"))
		 ))
            )
        (V.list_from_structure v)

let viewpair_from_allparams a = 
  let l = Safelist.flatten [
    (match a.altrepparam with
     | None -> []
     | Some s -> ["altrepparam", V.new_value s]
    );
    (match a.cnparam with
     | None -> []
     | Some s -> ["cnparam", V.new_value s]
    );
    (match a.cutypeparam with
    | None -> []
    | Some ct ->
        ["cutypeparam", V.new_value (ICalendar.tostring ICalendar_print.print_cutype ct)]
    );
    (match a.delfromparam with
    | [] -> []
    | l ->
        ["delfromparam", V.structure_from_list (Safelist.map V.new_value l)]
    );
    (match a.deltoparam with
    | [] -> []
    | l ->
        ["deltoparam", V.structure_from_list (Safelist.map V.new_value l)]
    );
    (match a.dirparam with
     | None -> []
     | Some s -> ["dirparam", V.new_value s]
    );
    (match a.encodingparam with
    | None -> []
    | Some en ->
        ["encodingparam", V.new_value (ICalendar.tostring ICalendar_print.print_encoding en)]
    );
    (match a.fmttypeparam with
     | None -> []
     | Some s -> ["fmttypeparam", V.new_value s]
    );
    (match a.fbtypeparam with
     | None -> []
     | Some s -> failwith "fbtypeparam not implemented yet in viewer"
    );
    (match a.langparam with
     | None -> []
     | Some s -> ["langparam", V.new_value s]
    );
    (match a.memberparam with
    | [] -> []
    | l ->
        ["memberparam", V.structure_from_list (Safelist.map V.new_value l)]
    );
    (match a.partstatparam with
    | None -> []
    | Some pa ->
        ["partstatparam", V.new_value (ICalendar.tostring ICalendar_print.print_partstat pa)]
    );
    (match a.rangeparam with
    | None -> []
    | Some ra ->
        ["rangeparam", V.new_value (ICalendar.tostring ICalendar_print.print_range ra)]
    );
    (match a.reltypeparam with
    | None -> []
    | Some ra ->
        ["reltypeparam", V.new_value (ICalendar.tostring ICalendar_print.print_reltypeparam ra)]
    );
    (match a.roleparam with
    | None -> []
    | Some ra ->
        ["roleparam", V.new_value (ICalendar.tostring ICalendar_print.print_role ra)]
    );
    (match a.rsvpparam with
     | None -> []
     | Some b -> ["rsvpparam", view_from_bool b]
    );
    (match a.sentbyparam with
     | None -> []
     | Some s -> ["sentbyparam", V.new_value s]
    );
    (match a.trigrelparam with
    | None -> []
    | Some ra ->
        ["trigrelparam", V.new_value (ICalendar.tostring ICalendar_print.print_trigrel ra)]
    );
    (match a.tzidparam with
     | None -> []
     | Some (b,t) -> ["tzidparam", V.from_list ["global", view_from_bool b; "val", V.new_value t]]
    );
    (match a.valuetypeparam with
    | None -> []
    | Some va ->
        ["valuetypeparam", V.new_value (ICalendar.tostring ICalendar_print.print_valuetype va)]
    );
    viewpair_from_xplist a.xplist
  ]
  in
  match l with
  | [] -> []
  | _  -> ["params", V.from_list l]

let allparams_from_view_opt = function
  | None -> ICalendar_syntax.noparam ()
  | Some v -> 
      { altrepparam    = 
          (match V.get v "altrepparam" with
          | None -> None
          | Some s -> Some (V.get_value s)
          );
        cnparam        =
          (match V.get v "cnparam" with
          | None -> None
          | Some s -> Some (V.get_value s)
          );
        cutypeparam    =
          (match V.get v "cutypeparam" with
          | None -> None
          | Some s -> Some (ICalendar_lextypes.cutype_from_string (V.get_value s))
          );
        delfromparam   =
          (match V.get v "delfromparam" with
          | None -> []
          | Some l -> Safelist.map V.get_value (V.list_from_structure l)
          );
        deltoparam     =
          (match V.get v "deltoparam" with
          | None -> []
          | Some l -> Safelist.map V.get_value (V.list_from_structure l)
          );
        dirparam       =
          (match V.get v "dirparam" with
          | None -> None
          | Some s -> Some (V.get_value s)
          );
        encodingparam  =
          (match V.get v "encodingparam" with
          | None -> None
          | Some s -> Some (ICalendar_lextypes.encoding_from_string (V.get_value s))
          );
        fmttypeparam   =
          (match V.get v "fmttypeparam" with
          | None -> None
          | Some s -> Some (V.get_value s)
          );
        fbtypeparam    =
          (match V.get v "fmttypeparam" with
          | None -> None
          | Some s -> failwith "fbtypeparam not implemented yet"
          );
        langparam      =
          (match V.get v "langparam" with
          | None -> None
          | Some s -> Some (V.get_value s)
          );
        memberparam    =
          (match V.get v "memberparam" with
          | None -> []
          | Some l -> Safelist.map V.get_value (V.list_from_structure l)
          );
        partstatparam  =
          (match V.get v "partstatparam" with
          | None -> None
          | Some s -> Some (ICalendar_lextypes.partstatparam_from_string (V.get_value s))
          );
        rangeparam     =
          (match V.get v "rangeparam" with
          | None -> None
          | Some s -> Some (ICalendar_lextypes.rangeparam_from_string (V.get_value s))
          );
        reltypeparam   =
          (match V.get v "reltypeparam" with
          | None -> None
          | Some s -> Some (ICalendar_lextypes.reltypeparam_from_string (V.get_value s))
          );
        roleparam      =
          (match V.get v "roleparam" with
          | None -> None
          | Some s -> Some (ICalendar_lextypes.roleparam_from_string (V.get_value s))
          );
        rsvpparam      =
          (match V.get v "rsvpparam" with
          | None -> None
          | Some v -> Some (bool_from_view v)
          );
        sentbyparam    =
          (match V.get v "sentbyparam" with
          | None -> None
          | Some s -> Some (V.get_value s)
          );
        trigrelparam   =
        (match V.get v "trigrelparam" with
        | None -> None
        | Some s -> Some (ICalendar_lextypes.trigrelparam_from_string (V.get_value s))
              );
        tzidparam      =
        (
        match V.get v "tzidparam" with
        | None -> None
        | Some v -> 
	    Error.exit_on_error 
	      ( fun () ->
                Some (bool_from_view (V.get_required v "global"),
		      V.get_value (V.get_required v "val"))
		  )
	      );
        valuetypeparam =
        (match V.get v "valuetypeparam" with
        | None -> None
        | Some s -> Some (ICalendar_lextypes.valuetypeparam_from_string (V.get_value s))
              );
        xplist = xplist_from_view_opt (V.get v "xplist")
      }

let view_from_attach a =
  V.from_list
    (("val", 
        match a.attachval with
        | AttBinary b -> V.from_list ["AttBinary", V.new_value b]
        | AttUri    u -> V.from_list ["AttUri", V.new_value u])
     :: 
     (viewpair_from_allparams a.attachparam))
      
let attach_from_view v =
  Error.exit_on_error 
    (fun () ->
      { attachparam = allparams_from_view_opt (V.get v "params");
	attachval   =
        let v' = V.get_required v "val" in
        match V.get v' "AttBinary", V.get v' "AttUri" with
        | Some c, None -> AttBinary (V.get_value c)
        | None, Some c -> AttUri (V.get_value c) 
        | _ -> V.error_msg [`String "View should have exactly one of AttBinary of AttUri";
                             `Tree v]
      })
	
let view_from_attendee a =
  V.from_list
    (("val", V.new_value a.attendeeval)
     :: 
     (viewpair_from_allparams a.attendeeparam))
      
let attendee_from_view v =
  Error.exit_on_error ( fun () ->
    { attendeeparam = allparams_from_view_opt (V.get v "params");
      attendeeval   = V.get_value (V.get_required v "val")
    })

let view_from_categories c =
  V.from_list
    (("val", V.structure_from_list (Safelist.map V.new_value c.categoriesval))
     :: 
     (viewpair_from_allparams c.categoriesparam))
      
let categories_from_view v =
  Error.exit_on_error ( fun () ->
    { categoriesparam = allparams_from_view_opt (V.get v "params");
      categoriesval   = Safelist.map V.get_value (V.list_from_structure (V.get_required v "val"))
    } )

let view_from_comment a =
  V.from_list
    (("val", V.new_value a.commenttext)
     :: 
     (viewpair_from_allparams a.commentparam))
      
let comment_from_view v =
  Error.exit_on_error ( fun () ->
    { commentparam = allparams_from_view_opt (V.get v "params");
      commenttext   = V.get_value (V.get_required v "val")
    })
    
let view_from_contact a =
  V.from_list
    (("val", V.new_value a.contacttext)
     :: 
     (viewpair_from_allparams a.contactparam))
      
let contact_from_view v =
  Error.exit_on_error ( fun () ->
    { contactparam = allparams_from_view_opt (V.get v "params");
      contacttext   = V.get_value (V.get_required v "val")
    })

let view_from_date { year = y; month = m; day = d} =
  V.from_list [
    "year", V.new_value (Printf.sprintf "%04d" y);
    "month", V.new_value (Printf.sprintf "%02d" m);
    "day", V.new_value (Printf.sprintf "%02d" d)
  ]

let date_from_view v =
  Error.exit_on_error ( fun () ->
    { year = int_of_string (V.get_value (V.get_required v "year"));
      month = int_of_string (V.get_value (V.get_required v "month"));
      day = int_of_string (V.get_value (V.get_required v "day"));
    })
    
    
let view_from_time { hour = h; minute = m; second = s; zulu = z } =
  V.from_list [
    "hour", V.new_value (Printf.sprintf "%02d" h);
    "minute", V.new_value (Printf.sprintf "%02d" m);
    "second", V.new_value (Printf.sprintf "%02d" s);
    "zulu", view_from_bool z
    ]

let time_from_view v =
  Error.exit_on_error ( fun () ->
    { hour = int_of_string (V.get_value (V.get_required v "hour"));
      minute = int_of_string (V.get_value (V.get_required v "minute"));
      second = int_of_string (V.get_value (V.get_required v "second"));
      zulu = bool_from_view (V.get_required v "zulu");
    })
  
let view_from_date_time { date = d; time = t} =
  V.from_list [ "date", view_from_date d; "time", view_from_time t]
  
let date_time_from_view v =
  Error.exit_on_error ( fun () ->
    { date = date_from_view (V.get_required v "date");
      time = time_from_view (V.get_required v "time");
    })
    
let view_from_dtxp (xpl,dt) =
  V.from_list
    ( ("val", (view_from_date_time dt)) ::
     viewpair_from_xplist xpl)
    
let dtxp_from_view v =
  Error.exit_on_error ( fun () ->
    ( xplist_from_view_opt (V.get v "xplist"), date_time_from_view (V.get_required v "val"))
      )
  
let view_from_description d =
  V.from_list
    (("val", V.new_value d.desctext)
     :: 
     (viewpair_from_allparams d.descparam))
      
let description_from_view v =
  Error.exit_on_error ( fun () ->
    { descparam = allparams_from_view_opt (V.get v "params");
      desctext  = V.get_value (V.get_required v "val")
    })
    
let view_from_dur_time { dur_hour = h; dur_minute = m; dur_second = s } =
  V.from_list ["hour", V.new_value (string_of_int h);
               "minute", V.new_value (string_of_int m);
               "second", V.new_value (string_of_int s)]

let dur_time_from_view v =
  Error.exit_on_error ( fun () ->
    { dur_hour = int_of_string (V.get_value (V.get_required v "hour"));
      dur_minute = int_of_string (V.get_value (V.get_required v "minute"));
      dur_second = int_of_string (V.get_value (V.get_required v "second"))
    })

let view_from_dur_date = function
  | i, None -> V.from_list ["Day", V.new_value (string_of_int i)]
  | i, Some t -> V.from_list ["Day", V.new_value (string_of_int i); "Time", view_from_dur_time t]

let dur_date_from_view v =
  Error.exit_on_error ( fun () ->
    (int_of_string (V.get_value (V.get_required v "Day")),
     match V.get v "Time" with
     | None -> None
     | Some t -> Some (dur_time_from_view t))
      )
    
let  view_from_dur_length = function
  | DurWeek d -> V.from_list ["DurWeek", V.new_value (string_of_int d)]
  | DurTime t -> V.from_list ["DurTime", view_from_dur_time t]
  | DurDate d -> V.from_list ["DurDate", view_from_dur_date d]

let dur_length_from_view v =
  match V.get v "DurWeek", V.get v "DurTime", V.get v "DurDate" with
  | Some d, None, None -> DurWeek (int_of_string (V.get_value d))
  | None, Some t, None -> DurTime (dur_time_from_view t)
  | None, None, Some d -> DurDate (dur_date_from_view d)
  | _ -> V.error_msg [`String "This view should have exactly one child in 'DurWeek', 'DurTime', or 'DurDate':";
                       `Tree v]
  
let view_from_duration {dur_neg = b; dur_length = dl} =
  V.from_list ["negative", view_from_bool b;
               "length", view_from_dur_length dl] 

let duration_from_view v =
  Error.exit_on_error ( fun () ->
    { dur_neg = bool_from_view (V.get_required v "negative");
      dur_length = dur_length_from_view (V.get_required v "length") }
      )
  
let view_from_period = function
  | PeriodExplicit (ds, de) -> V.from_list ["PeriodExplicit",
                                               V.from_list ["start", view_from_date_time ds;
                                                            "end",   view_from_date_time de]]
  | PeriodStart (ds, dur) -> V.from_list ["PeriodStart",
                                               V.from_list ["start", view_from_date_time ds;
                                                            "duration", view_from_duration dur]]

let period_from_view v =
  Error.exit_on_error ( fun () ->
    match V.get v "PeriodExplicit", V.get v "PeriodStart" with
    | Some v', None -> PeriodExplicit (date_time_from_view (V.get_required v' "start"), 
                                       date_time_from_view (V.get_required v' "end"))
    | None, Some v' -> PeriodStart (date_time_from_view (V.get_required v' "start"), 
                                    duration_from_view (V.get_required v' "duration"))
    | _ -> V.error_msg [`String "This view should have exactly one of ";
                         `String "PeriodExplicit or PeriodStart:";
                         `Tree v]
	  )
  
let view_from_dtpval = function
  | DateTimeVal dt -> V.from_list ["DateTimeVal", view_from_date_time dt]
  | DateVal d -> V.from_list ["DateVal", view_from_date d]
  | PeriodVal p -> V.from_list ["PeriodVal", view_from_period p]
  | DurationVal d -> V.from_list ["DurationVal", view_from_duration d]

let dtpval_from_view v =
  match V.get v "DateTimeVal", V.get v "DateVal", V.get v "PeriodVal", V.get v "DurationVal" with
  | Some d, None, None, None -> DateTimeVal (date_time_from_view d)
  | None, Some d, None, None -> DateVal (date_from_view d)
  | None, None, Some p, None -> PeriodVal (period_from_view p)
  | None, None, None, Some d -> DurationVal (duration_from_view d)
  | _ -> V.error_msg [`String "This view should have exactly one of ";
                      `String "DateTimeVal, DateVal, PeriodVal, or DurationVal:";
                      `Tree v]
  
let view_from_dt d =
  V.from_list
    (("val", view_from_dtpval d.dtval)
     :: 
     (viewpair_from_allparams d.dtparam))
      
let dt_from_view v =
  Error.exit_on_error ( fun () ->
    { dtparam = allparams_from_view_opt (V.get v "params");
      dtval  = dtpval_from_view (V.get_required v "val")
    })

let view_from_dtpl d = 
  V.from_list
    (("val", V.structure_from_list (Safelist.map view_from_dtpval d.dtplval))
     :: 
     (viewpair_from_allparams d.dtplparam))

let dtpl_from_view v =
  Error.exit_on_error ( fun () ->
    { dtplparam = allparams_from_view_opt (V.get v "params");
      dtplval  = Safelist.map dtpval_from_view (V.list_from_structure (V.get_required v "val"))
    })

let viewpair_from_recur_end = function
  | RecUntil d -> ["end", (V.from_list ["until", view_from_dtpval d])]
  | RecCount i -> ["end", (V.from_list ["count", V.new_value (string_of_int i)])]
  | RecNone -> []

let recur_end_from_view v =
  match V.get v "end" with
  | Some v' ->
      (match V.get v' "until", V.get v' "count" with
      | Some v'', None -> RecUntil (dtpval_from_view v'')
      | None, Some v'' -> RecCount (int_of_string (V.get_value v''))
      | _ -> V.error_msg [`String "This recur_end is not well-formed:"; `Tree v]
      )
  | None -> RecNone
  
let view_from_bydayelt (i, w) =
  V.from_list (
    ("weekday", V.new_value (ICalendar.tostring ICalendar_print.print_weekday w)) ::
    (match i with
    | None -> []
    | Some i -> ["which", V.new_value (string_of_int i)]))

let bydayelt_from_view v =
  Error.exit_on_error ( fun () ->
    (
      (match V.get v "which" with
      | None -> None
      | Some i -> Some (int_of_string (V.get_value i))),
      ICalendar_lextypes.weekday_from_string (V.get_value (V.get_required v "weekday"))
    ))

let view_from_recur r =
  V.from_list (Safelist.flatten [
    ["freq", V.new_value (ICalendar.tostring ICalendar_print.print_freq r.recur_freq)];
    (viewpair_from_recur_end r.recur_end);
    (match r.recur_interval with
    | None -> []
    | Some i -> ["interval", V.new_value (string_of_int i)]);
    (match r.recur_bysec with
    | [] -> []
    | l -> ["bysecond", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_bymin with
    | [] -> []
    | l -> ["byminute", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_byhour with
    | [] -> []
    | l -> ["byhour", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_byday with
    | [] -> []
    | l -> ["byday", V.structure_from_list (Safelist.map view_from_bydayelt l)]);
    (match r.recur_bymonthday with
    | [] -> []
    | l -> ["bymonthday", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_byyearday with
    | [] -> []
    | l -> ["byyearday", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_byweekno with
    | [] -> []
    | l -> ["byweekno", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_bymonth with
    | [] -> []
    | l -> ["bymonth", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_bysetpos with
    | [] -> []
    | l -> ["bysetpos", V.structure_from_list (Safelist.map (fun e -> V.new_value (string_of_int e)) l)]);
    (match r.recur_wkstart with
    | None -> []
    | Some w -> ["wkstart", V.new_value (ICalendar.tostring ICalendar_print.print_weekday w)]);
    (match r.recur_bytext with
    | None -> []
    | Some (n,t) -> ["bytext", V.from_list ["name", V.new_value n; "val", V.new_value t]])
  ])

let recur_from_view v =
  Error.exit_on_error ( fun () ->
    { recur_freq       = ICalendar_lextypes.freq_from_string (V.get_value (V.get_required v "freq"));
      recur_end        = recur_end_from_view v;
      recur_interval   = 
        (match V.get v "interval" with
        | None -> None
        | Some i -> Some (int_of_string (V.get_value i)));
      recur_bysec      =
        (match V.get v "bysecond" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_bymin      =
        (match V.get v "byminute" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_byhour     =
        (match V.get v "byhour" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_byday      =
        (match V.get v "byday" with
        | None -> []
        | Some l -> Safelist.map bydayelt_from_view (V.list_from_structure l));
      recur_bymonthday =
        (match V.get v "bymonthday" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_byyearday  =
        (match V.get v "byyearday" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_byweekno   =
        (match V.get v "byweekno" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_bymonth    =
        (match V.get v "bymonth" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_bysetpos   =
        (match V.get v "bysetpos" with
        | None -> []
        | Some l -> Safelist.map (fun e -> int_of_string (V.get_value e)) (V.list_from_structure l));
      recur_wkstart    =
        (match V.get v "wkstart" with
        | None -> None
        | Some w -> Some (ICalendar_lextypes.weekday_from_string (V.get_value w)));
      recur_bytext     =
        (match V.get v "bytext" with
        | None -> None
        | Some w -> Some ( V.get_value (V.get_required v "name"),
                           V.get_value (V.get_required v "val") ));
    })
let view_from_location l =
  V.from_list
    (("val", V.new_value l.loctext)
     :: 
     (viewpair_from_allparams l.locparam))
      
let location_from_view v =
  Error.exit_on_error ( fun () ->
    { locparam = allparams_from_view_opt (V.get v "params");
      loctext  = V.get_value (V.get_required v "val")
    })

let view_from_organizer o =
  V.from_list
    (("val", V.new_value o.org_cal_address)
     :: 
     (viewpair_from_allparams o.orgparam))
      
let organizer_from_view v =
  Error.exit_on_error ( fun () ->
    { orgparam = allparams_from_view_opt (V.get v "params");
      org_cal_address  = V.get_value (V.get_required v "val")
    })

let view_from_recurid r =
  V.from_list
    (("val", view_from_dtpval r.recurval)
     :: 
     (viewpair_from_allparams r.recurparam))
  
let recurid_from_view v =
  Error.exit_on_error ( fun () ->
    { recurparam = allparams_from_view_opt (V.get v "params");
      recurval  = dtpval_from_view (V.get_required v "val")
    })
    
let view_from_related_to r =
  V.from_list
    (("val", V.new_value r.relatedtotext)
     :: 
     (viewpair_from_allparams r.relatedtoparam))
      
let related_to_from_view v =
  Error.exit_on_error ( fun () ->
    { relatedtoparam = allparams_from_view_opt (V.get v "params");
      relatedtotext  = V.get_value (V.get_required v "val")
    })

let view_from_resources r =
  V.from_list
    (("val", V.structure_from_list (Safelist.map V.new_value r.resourcesval))
     :: 
     (viewpair_from_allparams r.resourcesparam))
      
let resources_from_view v =
  Error.exit_on_error ( fun () ->
    { resourcesparam = allparams_from_view_opt (V.get v "params");
      resourcesval  = Safelist.map V.get_value (V.list_from_structure (V.get_required v "val"))
    })

let view_from_rstatus r =
  V.from_list
  (Safelist.flatten [
    ["code", V.structure_from_list (Safelist.map (fun c -> V.new_value (string_of_int c)) r.statcode)];
    ["text", V.new_value r.stattext];
    (match r.extdata with
    | None -> []
    | Some t -> ["extdata", V.new_value t]);
    (viewpair_from_allparams r.rstatparam)])
      
let rstatus_from_view v =
  Error.exit_on_error ( fun () ->
    { rstatparam = allparams_from_view_opt (V.get v "params");
      statcode  = Safelist.map (fun e -> int_of_string (V.get_value e)) 
                        (V.list_from_structure (V.get_required v "code"));
      stattext = V.get_value (V.get_required v "text");
      extdata = 
        match V.get v "extdata" with
        | None -> None
        | Some t -> Some (V.get_value t) 
    })

let view_from_summary s =
  V.from_list
    (("val", V.new_value s.sumtext)
     :: 
     (viewpair_from_allparams s.sumparam))
      
let summary_from_view v =
  Error.exit_on_error ( fun () ->
    { sumparam = allparams_from_view_opt (V.get v "params");
      sumtext  = V.get_value (V.get_required v "val")
    })

let view_from_trigger t =
  V.from_list
    (("val", view_from_dtpval t.triggerval)
     :: 
     (viewpair_from_allparams t.triggerparam))
  
let trigger_from_view v =
  Error.exit_on_error ( fun () ->
    { triggerparam = allparams_from_view_opt (V.get v "params");
      triggerval  = dtpval_from_view (V.get_required v "val")
    })

let view_from_tzname t =
  V.from_list
    (("val", V.new_value t.tznameval)
     :: 
     (viewpair_from_allparams t.tznameparam))
      
let tzname_from_view v =
  Error.exit_on_error ( fun () ->
    { tznameparam = allparams_from_view_opt (V.get v "params");
      tznameval  = V.get_value (V.get_required v "val")
    })

let view_from_offset_time { positive = p; off_h = h; off_m = m; off_s = s } =
  V.from_list ["positive", view_from_bool p;
               "hour", V.new_value (string_of_int h);
               "minute", V.new_value (string_of_int m);
               "second", V.new_value (string_of_int s)]

let offset_time_from_view v =
  Error.exit_on_error ( fun () ->
    { positive = bool_from_view (V.get_required v "positive");
      off_h = int_of_string (V.get_value (V.get_required v "hour"));
      off_m = int_of_string (V.get_value (V.get_required v "minute"));
      off_s = int_of_string (V.get_value (V.get_required v "second"))
    })

let rec view_from_comp_prop c =
  V.from_list (Safelist.flatten [
    (match c.comp_action with
    | None -> []
    | Some (xpl, act) ->
        [ "action", V.from_list (("val", 
                                     V.new_value (ICalendar.tostring ICalendar_print.print_actionval act))
                               :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_attach with
    | [] -> []
    | l -> ["attach", V.structure_from_list (Safelist.map view_from_attach l)]);
    (match c.comp_attendee with
    | [] -> []
    | l -> ["attendee", V.structure_from_list (Safelist.map view_from_attendee l)]);
    (match c.comp_categories with
    | [] -> []
    | l -> ["categories", V.structure_from_list (Safelist.map view_from_categories l)]);
    (match c.comp_class with
    | None -> []
    | Some (xpl, cl) ->
        [ "class", V.from_list (("val", 
                                     V.new_value (ICalendar.tostring ICalendar_print.print_classval cl))
                               :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_comment with
    | [] -> []
    | l -> ["comment", V.structure_from_list (Safelist.map view_from_comment l)]);
    (match c.comp_completed with
    | None -> []
    | Some () ->  failwith "COMPLETED not implemented yet"
    );
    (match c.comp_contact with
    | [] -> []
    | l -> ["contact", V.structure_from_list (Safelist.map view_from_contact l)]);
    (match c.comp_created with
    | None -> []
    | Some d -> ["created", view_from_dtxp d]);
    (match c.comp_daylightc with
    | [] -> []
    | l -> ["daylightc", V.structure_from_list (Safelist.map view_from_comp_prop l)]);
    (match c.comp_description with
    | None -> []
    | Some d ->  ["description", view_from_description d]
    );
    (match c.comp_dtend with
    | None -> []
    | Some d ->  ["dtend", view_from_dt d]
    );
    (match c.comp_dtstamp with
    | None -> []
    | Some d -> ["dtstamp", view_from_dt d]);
    (match c.comp_dtstart with
    | None -> []
    | Some d ->  ["dtstart", view_from_dt d]
    );
    (match c.comp_due with
    | None -> []
    | Some () ->  failwith "DUE not implemented yet"
    );
    (match c.comp_duration with
    | None -> []
    | Some (xpl, d) ->
        [ "duration", V.from_list (("val", view_from_duration d)
                               :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_exdate with
    | [] -> []
    | l -> ["exdate", V.structure_from_list (Safelist.map view_from_dtpl l)]);
    (match c.comp_exrule with
    | [] -> []
    | l -> [ "exrule", 
              V.structure_from_list (Safelist.map (fun (xpl, r) -> 
                                       V.from_list (   ("val", view_from_recur r)
                                                    :: (viewpair_from_xplist xpl)))
                                       l)
    ]);
    (match c.comp_freebusy with
    | [] -> []
    | l ->  failwith "FREEBUSY not implemented yet"
    );
    (match c.comp_geo with
    | None -> []
    | Some (xpl, (lat, lon)) ->
        [ "geo", V.from_list (    ("latitude", V.new_value (string_of_float lat))
                               :: ("longitude", V.new_value (string_of_float lon))
                               :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_lastmod with
    | None -> []
    | Some d -> ["lastmod", view_from_dtxp d]);
    (match c.comp_location with
    | None -> []
    | Some d ->  ["location", view_from_location d]
    );
    (match c.comp_organizer with
    | None -> []
    | Some d ->  ["organizer", view_from_organizer d]
    );
    (match c.comp_percent with
    | None -> []
    | Some _ ->  failwith "PERCENT not implemented yet"
    );
    (match c.comp_priority with
    | None -> []
    | Some (xpl, p) ->
        [ "priority", V.from_list (("val", V.new_value (string_of_int p))
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_rdate with
    | [] -> []
    | l -> ["rdate", V.structure_from_list (Safelist.map view_from_dtpl l)]);
    (match c.comp_recurid with
    | None -> []
    | Some d -> ["recurid", view_from_recurid d]);
    (match c.comp_related_to with
    | [] -> []
    | l -> ["related_to", V.structure_from_list (Safelist.map view_from_related_to l)]);
    (match c.comp_repeat with
    | None -> []
    | Some (xpl, r) ->
        [ "repeat", V.from_list (("val", V.new_value (string_of_int r))
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_resources with
    | [] -> []
    | l -> ["resources", V.structure_from_list (Safelist.map view_from_resources l)]);
    (match c.comp_rrule with
    | [] -> []
    | l -> [ "rrule", 
              V.structure_from_list (Safelist.map (fun (xpl, r) -> 
                                       V.from_list (   ("val", view_from_recur r)
                                                    :: (viewpair_from_xplist xpl)))
                                       l)
    ]);
    (match c.comp_rstatus with
    | [] -> []
    | l -> ["rstatus", V.structure_from_list (Safelist.map view_from_rstatus l)]);
    (match c.comp_seq with
    | None -> []
    | Some (xpl, r) ->
        [ "seq", V.from_list (("val", V.new_value (string_of_int r))
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_standardc with
    | [] -> []
    | l -> ["standardc", V.structure_from_list (Safelist.map view_from_comp_prop l)]);
    (match c.comp_status with
    | None -> []
    | Some (xpl, r) ->
        [ "status", V.from_list (("val", V.new_value (ICalendar.tostring ICalendar_print.print_status_val r))
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_summary with
    | None -> []
    | Some d ->  ["summary", view_from_summary d]
    );
    (match c.comp_transp with
    | None -> []
    | Some (xpl, r) ->
        [ "transp", V.from_list (("val", V.new_value (ICalendar.tostring ICalendar_print.print_transvalue r))
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_trigger with
    | None -> []
    | Some t -> ["trigger", view_from_trigger t]);
    (match c.comp_tzid with
    | None -> []
    | Some (xpl, glob, v) ->
        [ "tzid", V.from_list (    ("global", view_from_bool glob)
                               :: ("val", V.new_value v)
                               :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_tzname with
    | [] -> []
    | l -> ["tzname", V.structure_from_list (Safelist.map view_from_tzname l)]);
    (match c.comp_tzoffsetto with
    | None -> []
    | Some (xpl, o) ->
        [ "tzoffsetto", V.from_list (("val", view_from_offset_time o)
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_tzoffsetfrom with
    | None -> []
    | Some (xpl, o) ->
        [ "tzoffsetfrom", V.from_list (("val", view_from_offset_time o)
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_tzurl with
    | None -> []
    | Some (xpl, o) ->
        [ "tzurl", V.from_list (("val", V.new_value o)
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_uid with
    | None -> []
    | Some (xpl, o) ->
        [ "uid", V.from_list (("val", V.new_value o)
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_url with
    | None -> []
    | Some (xpl, o) ->
        [ "url", V.from_list (("val", V.new_value o)
                                   :: (viewpair_from_xplist xpl)) ]);
    (match c.comp_xprop with
    | [] -> []
    | _ -> 
        [ "xprop",
          V.structure_from_list
            (Safelist.map
               (fun (name, params, s) -> V.from_list   
                                                (Safelist.flatten
                                                  [ [ "name", V.new_value name ];
                                                    viewpair_from_allparams params;
                                                    [ "val", V.new_value s ]
                                                  ])
                )
               c.comp_xprop
            )
        ])
  ])

let rec comp_prop_from_view v =
  Error.exit_on_error ( fun () ->
    { 
      comp_action       =
        (match V.get v "action" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           ICalendar_lextypes.actionval_from_string (V.get_value (V.get_required v' "val")))
      );
      comp_attach       =
        (match V.get v "attach" with
        | None -> []
        | Some v' -> Safelist.map attach_from_view (V.list_from_structure v')
      );
      comp_attendee     =
        (match V.get v "attendee" with
        | None -> []
        | Some v' -> Safelist.map attendee_from_view (V.list_from_structure v')
      );
      comp_categories   =
        (match V.get v "categories" with
        | None -> []
        | Some v' -> Safelist.map categories_from_view (V.list_from_structure v')
      );
      comp_class        =
        (match V.get v "class" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           ICalendar_lextypes.classvalue_from_string 
                                   (V.get_value (V.get_required v' "val")))
      );
      comp_comment     =
        (match V.get v "comment" with
        | None -> []
        | Some v' -> Safelist.map comment_from_view (V.list_from_structure v')
      );
      comp_completed    =
        (match V.get v "completed" with
        | None -> None
        | Some v' -> V.error_msg [`String " the 'completed' field is not implemented yet ";
                                `Tree v]
      );
      comp_contact      =
        (match V.get v "contact" with
        | None -> []
        | Some v' -> Safelist.map contact_from_view (V.list_from_structure v')
      );
      comp_created      =
        (match V.get v "created" with
        | None -> None
        | Some v' -> Some (dtxp_from_view v')
      );
      comp_daylightc      =
        (match V.get v "daylightc" with
        | None -> []
        | Some v' -> Safelist.map comp_prop_from_view (V.list_from_structure v')
      );
      comp_description  =
        (match V.get v "description" with
        | None -> None
        | Some v' -> Some (description_from_view v')
      );
      comp_dtend        =
        (match V.get v "dtend" with
        | None -> None
        | Some v' -> Some (dt_from_view v')
      );
      comp_dtstamp      =
        (match V.get v "dtstamp" with
        | None -> None
        | Some v' -> Some (dt_from_view v')
      );
      comp_dtstart      =
        (match V.get v "dtstart" with
        | None -> None
        | Some v' -> Some (dt_from_view v')
      );
      comp_due          =
        (match V.get v "due" with
        | None -> None
        | Some v' -> V.error_msg [`String " the 'due' field is not implemented yet ";
                                `Tree v]
      );
      comp_duration     =
        (match V.get v "duration" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           duration_from_view (V.get_required v' "val"))
      );
      comp_exdate       =
        (match V.get v "exdate" with
        | None -> []
        | Some v' -> Safelist.map dtpl_from_view (V.list_from_structure v')
      );
      comp_exrule       =
        (match V.get v "exrule" with
        | None -> []
        | Some v' -> Safelist.map ( fun v'' ->
                           (xplist_from_view_opt (V.get v'' "xplist"),
                           recur_from_view (V.get_required v' "val")))
                        (V.list_from_structure v')
      );
      comp_freebusy     =
        (match V.get v "freebusy" with
        | None -> []
        | Some v' -> V.error_msg [`String " the 'FREEBUSY' field is not implemented yet ";
                                `Tree v]
      );
      comp_geo          =
        (match V.get v "geo" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           (float_of_string (V.get_value (V.get_required v' "latitude")),
                            float_of_string (V.get_value (V.get_required v' "longitude"))))
      );
      comp_lastmod      =
        (match V.get v "lastmod" with
        | None -> None
        | Some v' -> Some (dtxp_from_view v')
      );
      comp_location     =
        (match V.get v "location" with
        | None -> None
        | Some v' -> Some (location_from_view v')
      );
      comp_organizer    =
        (match V.get v "organizer" with
        | None -> None
        | Some v' -> Some (organizer_from_view v')
      );
      comp_percent      =
        (match V.get v "percent" with
        | None -> None
        | Some v' -> V.error_msg [`String " the 'PERCENT' field is not implemented yet ";
                                `Tree v]
      );
      comp_priority     =
        (match V.get v "priority" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           int_of_string (V.get_value (V.get_required v' "val")))
      );
      comp_rdate        =
        (match V.get v "rdate" with
        | None -> []
        | Some v' -> Safelist.map dtpl_from_view (V.list_from_structure v')
      );
      comp_recurid      =
        (match V.get v "recurid" with
        | None -> None
        | Some v' -> Some (recurid_from_view v')
      );
      comp_related_to   =
        (match V.get v "related_to" with
        | None -> []
        | Some v' -> Safelist.map related_to_from_view (V.list_from_structure v')
      );
      comp_repeat       =
        (match V.get v "repeat" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           int_of_string (V.get_value (V.get_required v' "val")))
      );
      comp_resources    =
        (match V.get v "resources" with
        | None -> []
        | Some v' -> Safelist.map resources_from_view (V.list_from_structure v')
      );
      comp_rrule        =
        (match V.get v "rrule" with
        | None -> []
        | Some v' -> Safelist.map ( fun v'' ->
                           (xplist_from_view_opt (V.get v'' "xplist"),
                           recur_from_view (V.get_required v'' "val")))
                        (V.list_from_structure v')
      );
      comp_rstatus      =
        (match V.get v "rstatus" with
        | None -> []
        | Some v' -> Safelist.map rstatus_from_view (V.list_from_structure v')
      );
      comp_seq          =
        (match V.get v "seq" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           int_of_string (V.get_value (V.get_required v' "val")))
      );
      comp_standardc    =
        (match V.get v "standardc" with
        | None -> []
        | Some v' -> Safelist.map comp_prop_from_view (V.list_from_structure v')
      );
      comp_status       =
        (match V.get v "status" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           ICalendar_lextypes.status_from_string (V.get_value (V.get_required v' "val")))
      );
      comp_summary      =
        (match V.get v "summary" with
        | None -> None
        | Some v' -> Some (summary_from_view v')
      );
      comp_transp       =
        (match V.get v "transp" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           ICalendar_lextypes.transvalue_from_string (V.get_value (V.get_required v' "val")))
      );
      comp_trigger      =
        (match V.get v "trigger" with
        | None -> None
        | Some v' -> Some (trigger_from_view v')
      );
      comp_tzid         =
        (match V.get v "tzid" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           bool_from_view (V.get_required v' "global"),
                           V.get_value (V.get_required v' "val"))
      );
      comp_tzname       =
        (match V.get v "tzname" with
        | None -> []
        | Some v' -> Safelist.map tzname_from_view (V.list_from_structure v')
      );
      comp_tzoffsetto   =
        (match V.get v "tzoffsetto" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           offset_time_from_view (V.get_required v' "val"))
      );
      comp_tzoffsetfrom =
        (match V.get v "tzoffsetfrom" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           offset_time_from_view (V.get_required v' "val"))
      );
      comp_tzurl        =
        (match V.get v "tzurl" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           V.get_value (V.get_required v' "val"))
      );
      comp_uid          =
        (match V.get v "uid" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           V.get_value (V.get_required v' "val"))
      );
      comp_url          =
        (match V.get v "url" with
        | None -> None
        | Some v' -> Some (xplist_from_view_opt (V.get v' "xplist"),
                           V.get_value (V.get_required v' "val"))
      );
      comp_xprop       =
                 (match V.get v "xprop" with
                 | None -> []
                 | Some l -> Safelist.map 
                               ( fun v' -> ( V.get_value (V.get_required v' "name"),
                                             allparams_from_view_opt (V.get v' "params"),
                                             V.get_value (V.get_required v' "val") ))
                               (V.list_from_structure l)
                 )
    })

let viewpair_from_component_list s l =
  match (Safelist.map view_from_comp_prop l) with
  | [] -> []
  | l' -> [s, V.structure_from_list l']

let component_list_from_view_opt = function
  | None -> []
  | Some v -> Safelist.map comp_prop_from_view (V.list_from_structure v)
      
let view_from_component = function
  | Eventc { event_comp = c; event_alarms = a } ->
      V.from_list ["type", V.new_value "Eventc";
                   "val",  V.from_list (("props", view_from_comp_prop c) ::
                                        (viewpair_from_component_list "alarms" a))
    ]
  | Timezonec v ->
      V.from_list ["type", V.new_value "Timezonec";
                   "val",  view_from_comp_prop v]

let component_from_view v =
  Error.exit_on_error ( fun () ->
    let v' = V.get_required v "val" in
    match (V.get_value (V.get_required v "type")) with
    | "Eventc" -> Eventc { event_comp = comp_prop_from_view (V.get_required v' "props");
                           event_alarms = component_list_from_view_opt (V.get v' "alarms") }
    | "Timezonec" -> Timezonec (comp_prop_from_view v')
    | s -> V.error_msg [`String s; `String " is not implemented yet in "; `Tree v])
    
let view_from_components c = V.structure_from_list (Safelist.map view_from_component c)

let components_from_view v = Safelist.map component_from_view (V.list_from_structure v)

let view_from_calprops p =
  let l = Safelist.flatten [
    (match p.prodid with
    | xpl, s -> ["prodid", V.from_list (("val", V.new_value s) :: (viewpair_from_xplist xpl)) ]);
    (match p.version with
    | xpl, s -> ["version", V.from_list (("val", V.new_value s) :: (viewpair_from_xplist xpl)) ]);
    (match p.calscale with
    | None -> []
    | Some (xpl, cv) ->
        ["calscale", V.from_list (("val", 
                                     V.new_value (ICalendar.tostring ICalendar_print.print_calvalue cv))
                              :: (viewpair_from_xplist xpl))
        ]);
    (match p.imethod with
    | None -> []
    | Some (xpl, s) ->
        ["method", V.from_list (("val", V.new_value s) ::
                                (viewpair_from_xplist xpl))
        ]);
    (match p.xprop with
    | [] -> []
    | _ -> 
        [ "xprop",
          V.structure_from_list
            (Safelist.map
               (fun (name, params, s) -> V.from_list   
                                                (Safelist.flatten
                                                  [ [ "name", V.new_value name ];
                                                    viewpair_from_allparams params;
                                                    [ "val", V.new_value s ]
                                                  ])
                )
               p.xprop
            )
        ])
  ]
  in
  V.from_list l

let calprops_from_view v =
  Error.exit_on_error ( fun () ->
    { prodid   = (let v' = V.get_required v "prodid" in
                 ( xplist_from_view_opt (V.get v' "xplist"), 
                   V.get_value (V.get_required v' "val") ));
      version  = (let v' = V.get_required v "version" in
                 ( xplist_from_view_opt (V.get v' "xplist"), 
                   V.get_value (V.get_required v' "val") ));
      calscale = (match V.get v "calscale" with
                 | None -> None
                 | Some v' -> Some ( xplist_from_view_opt (V.get v' "xplist"),
                                     ICalendar_lextypes.calvalue_from_string 
                                       (V.get_value (V.get_required v' "val")) ));
      imethod  = (match V.get v "method" with
                 | None -> None
                 | Some v' -> Some ( xplist_from_view_opt (V.get v' "xplist"), 
                                     V.get_value (V.get_required v' "val") ));
      xprop    = (match V.get v "xprop" with
                 | None -> []
                 | Some l -> Safelist.map 
                               ( fun v' -> ( V.get_value (V.get_required v' "name"),
                                             allparams_from_view_opt (V.get v' "params"),
                                             V.get_value (V.get_required v' "val") ))
                               (V.list_from_structure l)
                 )
    })

let view_from_icalobject { calprops = p; components = c} = 
  V.from_list ["calprops", view_from_calprops p; "components", view_from_components c]

let icalobject_from_view v =
  match V.get v "calprops", V.get v "components" with
  | Some vp, Some vc -> { calprops = calprops_from_view vp; components = components_from_view vc }
  | _, _ -> V.error_msg [`String "missing 'calprops' or 'components'"; `Tree v]

let view_from_icalendar i = 
  V.structure_from_list (Safelist.map view_from_icalobject i)

let icalendar_from_view v = 
  Safelist.map icalobject_from_view (V.list_from_structure v)
