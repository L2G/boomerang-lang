{
(*
  this lexer accepts both \r\n and \n end of line
  this lexer requires the version to be 2.0
  *)
  
  open ICalendarparse
  open ICalendar_lextypes

(* To buffer string literals *)

let string_start_pos = ref 0;;
let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let store_string s =
  let l = String.length s in
  if !string_index + l > String.length (!string_buff) then begin
    let new_buff = String.create ((String.length (!string_buff) + l) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.blit s 0 (!string_buff) (!string_index) l;
  string_index := (!string_index) + l

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  (*
  print_endline s;
  flush stdout;
  *)
  s
}

let eol = "\r\n" | "\n"
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let wsp = [' ' '\t']
(* the following is an empirical approximation of chars allowed in uri*)
let uri_chars = alpha | digit | ['<' '>' '/' '.' '@' ':' '=' '%' ',' '?' '(' ')' '-' '_' '~' '\\']
let non_us_ascii = ['\128' - '\248']
(*     NON-US-ASCII       = %x80-F8*)
let safe_char = wsp | ['!' '\035'-'\043' '\045'-'\057' '\060'-'\126'] | non_us_ascii 
(*     SAFE-CHAR  = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E / NON-US-ASCII *)
let qsafe = wsp | ['!' '\035'-'\126'] | non_us_ascii
(*     QSAFE-CHAR = WSP / %x21 / %x23-7E / NON-US-ASCII *)
let tsafe = [' ' '!' '\035'-'\043' '\045'-'\057' '\060'-'\091' '\093'-'\126'] | non_us_ascii
(*     TSAFE-CHAR = %x20-21 / %x23-2B / %x2D-39 / %x3C-5B %x5D-7E / NON-US-ASCII *)
let float = ( '+' ? | '-' ) digit + ('.' digit +)?
let b_char = alpha | digit | '+' | '/'

let xname = "X-" (alpha|digit|'-')+
let iana_tok = (alpha | digit | '-' | '/')+  (* I added the '/' which is not in the rfc
                                                but which is used in mime types *)

rule line = parse
| "BEGIN:VCALENDAR" eol   { BVCALENDAR }
| "END:VCALENDAR" eol     { EVCALENDAR }
| "BEGIN:VEVENT" eol      { BVEVENT }
| "END:VEVENT" eol        { EVEVENT }
| "BEGIN:VALARM" eol      { BVALARM }
| "END:VALARM" eol        { EVALARM }
| "BEGIN:VTIMEZONE" eol   { BVTIMEZONE }
| "END:VTIMEZONE" eol     { EVTIMEZONE }
| "BEGIN:STANDARD" eol    { BSTANDARD }
| "END:STANDARD" eol      { ESTANDARD }
| "BEGIN:DAYLIGHT" eol    { BDAYLIGHT }
| "END:DAYLIGHT" eol      { EDAYLIGHT }
| "PRODID"                { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let pidval = text lexbuf in
                            crlf lexbuf;
                            PRODID (xplist, pidval) }
| "VERSION"               { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            version2 lexbuf;
                            crlf lexbuf;
                            VERSION (xplist,"2.0") }
| "CALSCALE"              { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let calval = calvalue lexbuf in
                            crlf lexbuf;
                            CALSCALE (xplist, calval) }                           
| "METHOD"                { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let metval = iana_token lexbuf in
                            crlf lexbuf;
                            METHOD (xplist, metval) }                           
| xname                   { let name = Lexing.lexeme lexbuf in
                            let params = all_param_list lexbuf in 
                            col lexbuf;
                            let txt = text lexbuf in
                            crlf lexbuf;
                            XPROP (name, params, txt) }                           
| "CLASS"                 { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let classval = classvalue lexbuf in
                            crlf lexbuf;
                            CLASS (xplist, classval) }                           
| "CREATED"               { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let dt = date_time lexbuf in
                            crlf lexbuf;
                            CREATED (xplist, dt) }                           
| "DESCRIPTION"           { let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            DESCRIPTION (params, text) }
| "DTSTART"               { let params = all_param_list lexbuf in
                            col lexbuf;
                            let dval = dtp lexbuf in
                            crlf lexbuf;
                            DTSTART (params, dval) }
| "GEO"                   { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let coord = geoval lexbuf in
                            crlf lexbuf;
                            GEO(xplist, coord) }
| "LAST-MODIFIED"         { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let dt = date_time lexbuf in
                            crlf lexbuf;
                            LASTMODIFIED(xplist, dt) }
| "LOCATION"              { let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            LOCATION (params, text) }
| "ORGANIZER"             { let params = all_param_list lexbuf in
                            col lexbuf;
                            let uri = uri lexbuf in
                            crlf lexbuf;
                            ORGANIZER (params, uri) }
| "PRIORITY"              { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let pr = integer_val lexbuf in
                            crlf lexbuf;
                            PRIORITY (xplist, pr) }                           
| "DTSTAMP"               { let allparams = all_param_list lexbuf in
                            col lexbuf;
                            let dt = dtp lexbuf in
                            crlf lexbuf;
                            DTSTAMP(allparams, dt) }
| "SEQUENCE"              { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let seq = integer_val lexbuf in
                            crlf lexbuf;
                            SEQUENCE (xplist, seq) }                           
| "STATUS"                { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let status = status lexbuf in
                            crlf lexbuf;
                            STATUS (xplist, status) }                           
| "SUMMARY"               { let params = all_param_list lexbuf in
                            col lexbuf;
                            let txt = text lexbuf in
                            crlf lexbuf;
                            SUMMARY (params, txt) }
| "TRANSP"                { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let transval = transvalue lexbuf in
                            crlf lexbuf;
                            TRANSP (xplist, transval) }                           
| "UID"                   { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let uid = text lexbuf in
                            crlf lexbuf;
                            UID (xplist, uid) }
| "URL"                   { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let url = uri lexbuf in
                            crlf lexbuf;
                            URL (xplist, url) }
| "RECURRENCE-ID"         { let params = all_param_list lexbuf in
                            col lexbuf;
                            let rval = dtp lexbuf in
                            crlf lexbuf;
                            RECURRENCEID (params, rval) }
| "DTEND"                 { let params = all_param_list lexbuf in
                            col lexbuf;
                            let dval = dtp lexbuf in
                            crlf lexbuf;
                            DTEND (params, dval) }
| "DURATION"              { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let duration = durval lexbuf in
                            crlf lexbuf;
                            DURATION (xplist, duration) }
| "ATTACH"                { let params = all_param_list lexbuf in
                            col lexbuf;
                            let aval =  
                              if List.exists (fun elt -> elt = Valuetypeparam ICalendar_syntax.Binary) params
                              then ICalendar_syntax.AttBinary (binary lexbuf)
                              else ICalendar_syntax.AttUri (uri lexbuf)
                            in
                            crlf lexbuf;
                            ATTACH (params, aval) }
| "ATTENDEE"              { let params = all_param_list lexbuf in
                            col lexbuf;
                            let aval = uri lexbuf in
                            crlf lexbuf;
                            ATTENDEE (params, aval) }
| "CATEGORIES"            { let params = all_param_list lexbuf in
                            col lexbuf;
                            let t = text lexbuf in
                            let tl = text_list lexbuf in
                            crlf lexbuf;
                            CATEGORIES (params, t :: tl) }
| "COMMENT"               { let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            COMMENT (params, text) }
| "CONTACT"               { let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            CONTACT (params, text) }
| "EXDATE"                { let params = all_param_list lexbuf in
                            col lexbuf;
                            let v = dtp lexbuf in
                            let vl = dtp_list lexbuf in
                            crlf lexbuf;
                            EXDATE (params, (v::vl)) }
| "EXRULE"                { let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = recur lexbuf in
                            crlf lexbuf;
                            EXRULE (xpl, v) }
| "REQUEST-STATUS"        { let params = all_param_list lexbuf in
                            col lexbuf;
                            let i = integer_val lexbuf in
                            let il = dot_integer_list lexbuf in
                            semi lexbuf;
                            let sd = text lexbuf in
                            let ed =
                            if is_semi lexbuf then
                              Some (text lexbuf)
                            else 
                              None
                            in
                            crlf lexbuf;
                            RSTATUS (params, (i::il), sd, ed) }
| "RELATED-TO"            { let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            RELATED_TO (params, text) }
| "RESOURCES"             { let params = all_param_list lexbuf in
                            col lexbuf;
                            let t = text lexbuf in
                            let tl = text_list lexbuf in
                            crlf lexbuf;
                            RESOURCES (params, t :: tl) }
| "RDATE"                 { let params = all_param_list lexbuf in
                            col lexbuf;
                            let v = dtp lexbuf in
                            let vl = dtp_list lexbuf in
                            crlf lexbuf;
                            RDATE (params, (v::vl)) }
| "RRULE"                 { let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = recur lexbuf in
                            crlf lexbuf;
                            RRULE (xpl, v) }
| "ACTION"                { let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = actionval lexbuf in
                            crlf lexbuf;
                            ACTION (xpl, v) }
| "TRIGGER"               { let params = all_param_list lexbuf in
                            col lexbuf;
                            let v = dtp lexbuf in
                            crlf lexbuf;
                            TRIGGER (params, v) }
| "REPEAT"                { let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = integer_val lexbuf in
                            crlf lexbuf;
                            REPEAT (xpl, v) }
| "TZID"                  { let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let global = is_slash lexbuf in
                            let v = text lexbuf in
                            crlf lexbuf;
                            TZID (xpl, global, v) }
| "TZURL"                 { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let url = uri lexbuf in
                            crlf lexbuf;
                            TZURL (xplist, url) }
| "TZOFFSETTO"            { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let off = utcoffset lexbuf in
                            crlf lexbuf;
                            TZOFFSETTO (xplist, off) }
| "TZOFFSETFROM"          { let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let off = utcoffset lexbuf in
                            crlf lexbuf;
                            TZOFFSETFROM (xplist, off) }
| "TZNAME"                { let params = all_param_list lexbuf in
                            col lexbuf;
                            let t = text lexbuf in
                            crlf lexbuf;
                            TZNAME (params, t) }
| eof                     { EOF }
| ""                      {  let s = aline lexbuf in failwith (s ^ " is not implemented yet") }

and aline = parse
| (tsafe | '\\' | ';' | ',' | ':' | '"')* { Lexing.lexeme lexbuf }
                             
and col = parse
| ':'                     { () }
| _                       { print_endline (Lexing.lexeme lexbuf);
                            failwith "expected a :" }

and crlf = parse
| eol                     { () }
| _                       { print_endline (Lexing.lexeme lexbuf); failwith "expected eol" }

and quote = parse
| '"'                     { () }
| _                       { failwith "expected a \"" }

and is_semi = parse
| ';'                     { true }
| ""                      { false }

and semi = parse
| ';'                     { () }
| ""                      { failwith "expected a ';'" }

and version2 = parse
| "2.0"                   { () }
| _                       { failwith "version number different from 2.0" }

and uri = parse
| uri_chars*              { Lexing.lexeme lexbuf }

and text_list = parse
| ","                     { let t = text lexbuf in t :: (text_list lexbuf) }
| ""                      { [] }

and text = parse
| ""                      { reset_string_buffer ();
                            let string_start = Lexing.lexeme_start lexbuf in
                            string_start_pos := string_start;
                            intext lexbuf;
                            (*
                            lexbuf.Lexing.lex_start_pos <-
                                string_start - lexbuf.Lexing.lex_abs_pos;
                                *)
                            (*
                            let s = get_stored_string () in
                            print_endline s;
                            s
                            *)
                            get_stored_string ()
                          }

and binary = parse
| ""                      { reset_string_buffer ();
                            let string_start = Lexing.lexeme_start lexbuf in
                            string_start_pos := string_start;
                            inbinary lexbuf;
                            get_stored_string ()
                          }

and xparam_list = parse
| ';' xname               { let s = Lexing.lexeme lexbuf in
                            let s' = String.sub s 1 ((String.length s) - 1) in
                            let r = (s', param_value_equal lexbuf) in r :: (xparam_list lexbuf) }
| ""                      { [] }

and param_value_equal = parse
| '='                     { let pv = param_value lexbuf in
                            pv :: (param_value_list lexbuf) }

and param_value = parse
| (safe_char*) | ('"' (qsafe*) '"') { Lexing.lexeme lexbuf }

and param_value_list = parse
| ','                     { let pv = param_value lexbuf in
                            pv :: (param_value_list lexbuf) }
| ""                      { [] }

(*the following does not follow rfc 1766 ...*)
and lang_rfc1766 = parse
| (alpha | '-')*         { Lexing.lexeme lexbuf }
  
and paramtext = parse
| safe_char *           { Lexing.lexeme lexbuf }

and valuetype = parse
| "DATE-TIME" | "DATE"     | "BINARY"   | "PERIOD"   | "DURATION"  | "TEXT"
                        { ICalendar_lextypes.valuetypeparam_from_string (Lexing.lexeme lexbuf) }

and encodingval = parse
| "8BIT" | "BASE64" | iana_tok | xname  { ICalendar_lextypes.encoding_from_string (Lexing.lexeme lexbuf) }

and iana_or_x = parse
| iana_tok | xname      { Lexing.lexeme lexbuf }

and status = parse
| "TENTATIVE"    | "CONFIRMED"    | "CANCELLED"    | "NEEDS-ACTIONS" | "COMPLETED"    | "IN-PROCESS"   
| "DRAFT"        | "FINAL"          { ICalendar_lextypes.status_from_string (Lexing.lexeme lexbuf) }

and transvalue = parse
| "TRANSPARENT" | "OPAQUE"      { ICalendar_lextypes.transvalue_from_string (Lexing.lexeme lexbuf) }

and cutype = parse
| "INDIVIDUAL" | "GROUP" | "RESOURCE" | "ROOM" | "UNKNOWN" | iana_tok | xname
  { ICalendar_lextypes.cutype_from_string (Lexing.lexeme lexbuf) }

and quoted_uri_list = parse
| ",\""                 { let r = uri lexbuf in quote lexbuf; r :: (quoted_uri_list lexbuf) }
| ""                    { [] }

and role = parse
| "CHAIR"          | "REQ-PARTICIPANT" | "OPT-PARTICIPANT" | "NON-PARTICIPANT" | iana_tok | xname
                        { ICalendar_lextypes.roleparam_from_string (Lexing.lexeme lexbuf) }

and partstatparam = parse
| "NEEDS-ACTION" | "ACCEPTED" | "DECLINED" | "TENTATIVE" | "DELEGATED" | "COMPLETED" | "IN-PROCESS" | iana_tok | xname                { ICalendar_lextypes.partstatparam_from_string (Lexing.lexeme lexbuf) }

and reltypeparam = parse
| "PARENT" | "CHILD"  | "SIBLING" | iana_tok | xname 
                      { ICalendar_lextypes.reltypeparam_from_string (Lexing.lexeme lexbuf) }
              
and rangeparam = parse
| "THISANDPRIOR" | "THISANDFUTURE"  { ICalendar_lextypes.rangeparam_from_string (Lexing.lexeme lexbuf) }

and trigrelparam = parse
| "START" | "END" { ICalendar_lextypes.trigrelparam_from_string (Lexing.lexeme lexbuf) }

and all_param_list = parse
| ";ALTREP=\""           { let res = uri lexbuf in
                           quote lexbuf;
                           let r = Altrepparam res in r :: (all_param_list lexbuf) }
| ";CN="                 { let r = CNparam (param_value lexbuf) in r :: (all_param_list lexbuf) }
| ";CUTYPE="             { let r = Cutypeparam (cutype lexbuf) in r :: (all_param_list lexbuf) }
| ";DELEGATED-FROM=\""   { let u = uri lexbuf in
                           quote lexbuf;
                           let r = Delfromparam (u :: (quoted_uri_list lexbuf)) in
                           r :: (all_param_list lexbuf) }
| ";DELEGATED-TO=\""     { let u = uri lexbuf in
                           quote lexbuf;
                           let r = Deltoparam (u :: (quoted_uri_list lexbuf)) in
                           r :: (all_param_list lexbuf) }
| ";DIR=\""              { let uri = uri lexbuf in
                           quote lexbuf;
                           let r = Dirparam uri in r :: (all_param_list lexbuf) }
| ";ENCODING="           { let r = Encparam (encodingval lexbuf) in r :: (all_param_list lexbuf) }
| ";FMTTYPE="            { let r = Fmtparam (iana_or_x lexbuf) in r :: (all_param_list lexbuf) }
| ";LANGUAGE="           { let r = Langparam (lang_rfc1766 lexbuf) in r :: (all_param_list lexbuf) }
| ";MEMBER=\""           { let u = uri lexbuf in
                           quote lexbuf;
                           let r = Memberparam (u :: (quoted_uri_list lexbuf)) in
                           r :: (all_param_list lexbuf) }
| ";PARTSTAT="           { let r = Partstatparam (partstatparam lexbuf) in r :: (all_param_list lexbuf) }
| ";RANGE="              { let r = Rangeparam (rangeparam lexbuf) in r :: (all_param_list lexbuf) }
| ";RELATED=START"       { Trigrelparam ICalendar_syntax.RelStart :: (all_param_list lexbuf) }
| ";RELATED=END"         { Trigrelparam ICalendar_syntax.RelEnd :: (all_param_list lexbuf) }
| ";RELTYPE="            { let r = Reltypeparam (reltypeparam lexbuf) in r :: (all_param_list lexbuf) }
| ";RSVP=TRUE"           { (Rsvpparam true) :: (all_param_list lexbuf) }
| ";RSVP=FALSE"          { (Rsvpparam false) :: (all_param_list lexbuf) }
| ";ROLE="               { let r = Roleparam (role lexbuf) in r :: (all_param_list lexbuf) }
| ";SENT-BY=\""          { let uri = uri lexbuf in
                           quote lexbuf;
                           let r = Sentbyparam uri in r :: (all_param_list lexbuf) }
| ";TZID=/"              { let pt = paramtext lexbuf in 
                           let r = Tzidparam(true, pt) in r :: (all_param_list lexbuf) }
| ";TZID="               { let pt = paramtext lexbuf in 
                           let r = Tzidparam(false, pt) in r :: (all_param_list lexbuf) }
| ";VALUE="              { let vt = valuetype lexbuf in
                           (Valuetypeparam vt) :: (all_param_list lexbuf) }
| ';' xname              { let s = Lexing.lexeme lexbuf in
                            let s' = String.sub s 1 ((String.length s) - 1) in
                            let r = Xparam (s', param_value_equal lexbuf) in r :: (all_param_list lexbuf) }
| ""                     { [] }

and digits = parse
| digit +                { int_of_string (Lexing.lexeme lexbuf) }

and onetwod = parse
| digit | digit digit    { int_of_string (Lexing.lexeme lexbuf) }

and onetwosignedopt = parse
| "-" (digit digit | digit) { Some (int_of_string (Lexing.lexeme lexbuf))  }
| "+" (digit digit | digit) { let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              Some (int_of_string s')  }
| (digit digit | digit)     { Some (int_of_string (Lexing.lexeme lexbuf))  }
| ""                        { None }

and onetwosigned = parse
| "-" (digit digit | digit) {int_of_string (Lexing.lexeme lexbuf)  }
| "+" (digit digit | digit) { let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              int_of_string s'  }
| (digit digit | digit)     { int_of_string (Lexing.lexeme lexbuf)  }

and onetwodlist = parse
| ","                    { let v = onetwod lexbuf in v :: (onetwodlist lexbuf) }
| ""                     { [] }

and onetwosignedlist = parse
| ","                    { let v = onetwosigned lexbuf in v :: (onetwosignedlist lexbuf) }
| ""                     { [] }

and onethreesigned = parse
| "-" (digit digit digit | digit digit | digit) {int_of_string (Lexing.lexeme lexbuf)  }
| "+" (digit digit digit | digit digit | digit) { let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              int_of_string s'  }
| (digit digit digit | digit digit | digit)     { int_of_string (Lexing.lexeme lexbuf)  }

and onethreesignedlist = parse
| ","                    { let v = onethreesigned lexbuf in v :: (onethreesignedlist lexbuf) }
| ""                     { [] }

and weekday = parse
| "SU" | "MO" | "TU" | "WE" | "TH" | "FR" | "SA"    { ICalendar_lextypes.weekday_from_string
                                                        (Lexing.lexeme lexbuf) }

and weekdaynum = parse
| ""                     { let n = onetwosignedopt lexbuf in
                           n, weekday lexbuf }

and wdaylist = parse
| ","                    { let w = weekdaynum lexbuf in
                           w :: (wdaylist lexbuf) }
| ""                     { [] }                         
                           
and recurstuff = parse
| ";UNTIL="              { let d = dtp lexbuf in (RecDate d) :: (recurstuff lexbuf) }
| ";COUNT="              { let c = digits lexbuf in (RecCount c) :: (recurstuff lexbuf) }
| ";INTERVAL="           { let c = digits lexbuf in (RecInterval c) :: (recurstuff lexbuf) }
| ";BYSECOND="           { let v = onetwod lexbuf in 
                           let l = onetwodlist lexbuf in (RecBySecond (v :: l)) :: (recurstuff lexbuf) }
| ";BYMINUTE="           { let v = onetwod lexbuf in
                           let l = onetwodlist lexbuf in (RecByMinute (v :: l)) :: (recurstuff lexbuf) }
| ";BYHOUR="             { let v = onetwod lexbuf in
                           let l = onetwodlist lexbuf in (RecByHour (v :: l)) :: (recurstuff lexbuf) }
| ";BYDAY="              { let v = weekdaynum lexbuf in
                           let l = wdaylist lexbuf in (RecByDay (v :: l)) :: (recurstuff lexbuf) }
| ";BYMONTHDAY="         { let v = onetwosigned lexbuf in
                           let l = onetwosignedlist lexbuf in 
                           (RecByMonthDay (v :: l)) :: (recurstuff lexbuf) }
| ";BYYEARDAY="           { let v = onethreesigned lexbuf in
                           let l = onethreesignedlist lexbuf in 
                           (RecByYearDay (v :: l)) :: (recurstuff lexbuf) }
| ";BYWEEKNO="           { let v = onetwosigned lexbuf in
                           let l = onetwosignedlist lexbuf in 
                           (RecByWeekNo (v :: l)) :: (recurstuff lexbuf) }
| ";BYMONTH="            { let v = onetwod lexbuf in
                           let l = onetwodlist lexbuf in (RecByMonth (v :: l)) :: (recurstuff lexbuf) }
| ";BYSETPOS="           { let v = onethreesigned lexbuf in
                           let l = onethreesignedlist lexbuf in 
                           (RecBySetPos (v :: l)) :: (recurstuff lexbuf) }
| ";WKST="               { let w = weekday lexbuf in (RecWkStart w) :: (recurstuff lexbuf) }
| ';' xname              { let s = Lexing.lexeme lexbuf in
                            let s' = String.sub s 1 ((String.length s) - 1) in
                            let r = RecX (s', text lexbuf) in r :: (recurstuff lexbuf) }
| ""                     { [] }

and freq = parse
| "SECONDLY" | "MINUTELY" | "HOURLY"  | "DAILY"   | "WEEKLY"  | "MONTHLY" | "YEARLY"  
                         { ICalendar_lextypes.freq_from_string (Lexing.lexeme lexbuf) }
                           
and recur = parse
| "FREQ="                { let freq = freq lexbuf in
                           freq, (recurstuff lexbuf) }

and intext = parse
| '\\' ['\\' ';' ',' 'n' 'N']
    { store_string_char('\\');
      store_string_char(Lexing.lexeme_char lexbuf 1);
      intext lexbuf }
    (* following to accept iCal broken thing *)
| "\\\""
    { store_string_char(Lexing.lexeme_char lexbuf 1);
      intext lexbuf }
| tsafe | ':' | '"' 
    { store_string_char(Lexing.lexeme_char lexbuf 0);
      intext lexbuf }
| "" { () }

and inbinary = parse
| b_char b_char b_char b_char  { store_string (Lexing.lexeme lexbuf);
                                 inbinary lexbuf }
| ""                           { inendbinary lexbuf }

and inendbinary = parse
| b_char b_char "=="           { store_string (Lexing.lexeme lexbuf) }
| b_char b_char b_char '='     { store_string (Lexing.lexeme lexbuf) }
| ""                           { }

and iana_token = parse
| iana_tok                  { Lexing.lexeme lexbuf }                         

and calvalue = parse
| "GREGORIAN" | iana_tok    { ICalendar_lextypes.calvalue_from_string (Lexing.lexeme lexbuf) }

and classvalue = parse
| "PUBLIC" | "PRIVATE" | "CONFIDENTIAL" | xname | iana_tok  
                            { ICalendar_lextypes.classvalue_from_string (Lexing.lexeme lexbuf) }

and date_time = parse
| ""                        { let d = date lexbuf in
                              single_T lexbuf;
                              let t = time lexbuf in
                              { ICalendar_syntax.date = d;
                                ICalendar_syntax.time = t}
                              }

and two_digits = parse
| digit digit               { Lexing.lexeme lexbuf }

and opt_two_digits = parse
| digit digit               { Some (Lexing.lexeme lexbuf) }
| ""                        { None }

and get_sign = parse
| '+'                       { true }
| '-'                       { false }
| ""                        { failwith "Expected a '+' or a '-'" }

and utcoffset = parse
| ""                        { let pos = get_sign lexbuf in
                              let h = int_of_string (two_digits lexbuf) in
                              let m = int_of_string (two_digits lexbuf) in
                              let s =
                                match (opt_two_digits lexbuf) with
                                | Some t -> int_of_string t
                                | None -> 0
                              in
                              { ICalendar_syntax.positive = pos;
                                ICalendar_syntax.off_h = h;
                                ICalendar_syntax.off_m = m;
                                ICalendar_syntax.off_s = s; }
                            }

and date = parse
| ""                        { match opt_date lexbuf with
                              | Some d -> d
                              | None -> failwith "was expecting a date = 8 digits" }

and opt_date = parse
| digit digit digit digit   { let year = int_of_string (Lexing.lexeme lexbuf) in
                              let month = int_of_string (two_digits lexbuf) in
                              let day = int_of_string (two_digits lexbuf) in
                              Some (ICalendar_lextypes.mk_date (year, month, day)) }
| ""                        { None }

and is_slash = parse
| '/'                        { true }
| ""                         { false }

                              
and single_T = parse
| 'T'                        { () }
| ""                         { failwith "was expecting a T" }

and is_T = parse
| 'T'                        { true }
| ""                         { false }

and time = parse
| ""                         { let hour = int_of_string (two_digits lexbuf) in
                               let minute = int_of_string (two_digits lexbuf) in
                               let second = int_of_string (two_digits lexbuf) in
                               let t = (hour, minute, second) in
                               let res = 
                                 if (single_Z lexbuf) then (t, true)
                                 else (t, false) 
                               in
                               ICalendar_lextypes.mk_time res
                             }

and single_Z = parse
| 'Z'                                  { true }
| ""                                   { false }

and dtp = parse
| ""                         { match opt_date lexbuf with
                               | Some d ->
                                   if (is_T lexbuf) then
                                     let dts = {ICalendar_syntax.date = d;
                                                ICalendar_syntax.time = (time lexbuf)}
                                     in
                                     if (is_slash lexbuf) then
                                       ICalendar_syntax.PeriodVal (
                                         match (opt_durval lexbuf) with
                                         | Some d -> ICalendar_syntax.PeriodStart (dts, d)
                                         | None -> ICalendar_syntax.PeriodExplicit (dts, date_time lexbuf)
                                       )
                                     else
                                       ICalendar_syntax.DateTimeVal dts
                                   else
                                     ICalendar_syntax.DateVal d
                               | None -> ICalendar_syntax.DurationVal (durval lexbuf) }

and dtp_list = parse
| ","                        { let v = dtp lexbuf in v :: (dtp_list lexbuf) }
| ""                         { [] }
                                 
and float_val = parse
| float                      { float_of_string (Lexing.lexeme lexbuf) }

and integer_val = parse
| "-" (digit +)             { int_of_string (Lexing.lexeme lexbuf)  }
| "+" (digit +)             { let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              int_of_string s'  }
| digit+                    { int_of_string (Lexing.lexeme lexbuf)  }

and geoval = parse
| ""                         { let f1 = float_val lexbuf in
                               semi lexbuf;
                               (f1, float_val lexbuf) }

and duropt = parse
| digit+ 'W'                 { let s = Lexing.lexeme lexbuf in
                               Week (int_of_string (String.sub s 0 ((String.length s) - 1))) }
| digit+ 'D'                 { let s = Lexing.lexeme lexbuf in
                               if is_T lexbuf then
                                 DayT (int_of_string (String.sub s 0 ((String.length s) - 1)), duropt lexbuf) 
                               else
                                 Day (int_of_string (String.sub s 0 ((String.length s) - 1))) 
                               }
| digit+ 'H'                 { let s = Lexing.lexeme lexbuf in
                               Hour (int_of_string (String.sub s 0 ((String.length s) - 1)), duropt lexbuf) }
| digit+ 'M'                 { let s = Lexing.lexeme lexbuf in
                               Minute (int_of_string (String.sub s 0 ((String.length s) - 1)), duropt lexbuf) }
| digit+ 'S'                 { let s = Lexing.lexeme lexbuf in
                               Second (int_of_string (String.sub s 0 ((String.length s) - 1))) }
| ""                         { DurEnd }

and opt_durval = parse
| "-PT"                      { Some (neg_dur (convert_duration_time (duropt lexbuf))) }
| "-P"                       { Some (neg_dur (convert_duration_week_date (duropt lexbuf))) }
| "PT" | "+PT"               { Some (convert_duration_time (duropt lexbuf)) }
| "P" | "+P"                 { Some (convert_duration_week_date (duropt lexbuf)) }
| ""                         { None }

and durval = parse
| ""                         { match (opt_durval lexbuf) with
                               | Some d -> d
                               | None -> failwith "expected a duration" }

and dot_integer_list = parse
| "."                        { let i = integer_val lexbuf in
                               i :: (dot_integer_list lexbuf) }
| ""                         { [] }

and actionval = parse
| "AUDIO"    | "DISPLAY"  | "EMAIL"    | "PROCEDURE" | iana_tok | xname
                             { ICalendar_lextypes.actionval_from_string (Lexing.lexeme lexbuf) }
