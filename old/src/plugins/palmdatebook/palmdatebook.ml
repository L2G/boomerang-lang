open Pervasives_plugin
open Experimental_plugin

(* Deprecated section *)

let old_rename n1 n2 =
  let alist = [n1,n2] in
  (* first filter out all equal values, because they're allowed but nops *)
  let alist = Safelist.filter (fun (a,b) -> not (a = b)) alist in
  let inv_alist = Safelist.map (fun (a,b) -> (b,a)) alist in
  let swaplist = alist @ inv_alist in
  let check_bijection alist =
    let dom,rng = Safelist.split alist in
    let sdom = Safelist.sort compare dom in
    let srng = Safelist.sort compare rng in
    if not (Misc.uniq dom) then
      (prerr_endline "[Lens.old_rename] domain not unique!";
       prerr_endline ("domain: [" ^ (String.concat "; " dom) ^ "]"); false)
    else if not (Misc.uniq rng) then
      (prerr_endline "[Lens.oid_rename] range not unique!";
       prerr_endline ("range: [" ^ (String.concat "; " rng) ^ "]"); false)
    else if not (sdom = srng) then
      (prerr_endline "[Lens.old_rename] domain & range not the same!";
       prerr_endline ("dom: [" ^ (String.concat "; " sdom) ^ "]");
       prerr_endline ("rng: [" ^ (String.concat "; " srng) ^ "]"); false)
    else true
  in
  assert(check_bijection swaplist);
  let translate v =
    let newbinds =
      V.fold
        (fun k vk' acc ->
          (Misc.safeassoc swaplist k,Some vk')::acc)
        v [] in
    V.create_star newbinds in
    Lens.native 
      (fun c -> translate c)
      (fun a co -> translate a)

(* end of deprecated section *)


(* ----------------------------------------------------------------------- *)
(* Datebook record reader/writer *)

open Palm

let alarmFlag =  0x40
let repeatFlag = 0x20
let noteFlag =   0x10
let exceptFlag = 0x08
let descFlag =   0x04

let makelist m f =
  let rec loop n acc =
    if n >= m then Safelist.rev acc
    else loop (n+1) ((f n)::acc)
  in loop 0 []

let rec compose = function
  | [] -> id
  | [l] -> l
  | l :: r -> compose2 l (compose r)

(* f1 and f2 are record field names, sep should be a character not occuring*)
(* in any of the field values for f1; fout is the resulting field name *)
let strcat_lens f1 f2 fout sep =
  let get vf =
      let f1val = 
        match V.get vf f1 with
        | Some(vsub) -> V.get_value vsub
        | None -> "" 
      in
      let f2val = 
        match V.get vf f2 with
        | Some(vsub) -> V.get_value vsub
        | None -> "" 
      in
      if (String.contains f1val sep) then
        Lens.error [`String "Palmdatebook.strcat_lens(get): separator ";
        `String (Printf.sprintf ("\'%c\'") sep);
        `String " occurs in field \"";
        `String (Misc.whack f1);
        `String "\": ";
        `View vf];
        V.set_field_value (V.set (V.set vf f2 None) f1 None) fout 
        (f1val ^ (Printf.sprintf ("%c") sep) ^ f2val)
  in
  let put vf' _ =
      let foutval = V.get_field_value vf' fout in
      if not (String.contains foutval sep) then
        Lens.error [
          `String "Palmdatebook.strcat_lens(put): separator ";
          `String (Printf.sprintf ("\'%c\'") sep);
          `String " does not occur in field \"";
          `String (Misc.whack fout);
          `String "\": ";
          `View vf'];
        let seppos = String.index foutval sep in
        let f1val' = String.sub foutval 0 seppos in
        let f2val' = String.sub foutval (seppos+1) 
        (String.length foutval - seppos - 1) in
        V.set_field_value (V.set_field_value (V.set vf' fout None) f1 f1val')
        f2 f2val' 
  in
    Lens.native get put

let datebook_rcd_rd src =
  let beginhours = getbyte src in
  let beginmins = getbyte src in
  let endhours = getbyte src in
  let endmins = getbyte src in
  let rawdate = getshortBE src in
  let flagsraw = getbyte src in
  let pad1 = getbyte src in		(* padding *)
  let times =
    ["beginhours", V.new_value (string_of_int beginhours);
     "beginmins", V.new_value (string_of_int beginmins);
     "endhours", V.new_value (string_of_int endhours);
     "endmins", V.new_value (string_of_int endmins);
     "date", V.new_value (string_of_int rawdate);
     "pad1", V.new_value (string_of_int pad1); 
     "flagsraw", V.new_value (string_of_int flagsraw)] in
  let alarm =
    if (flagsraw land alarmFlag) <> 0 then
      let a = string_of_int (getbyte src) in
      let u = string_of_int (getbyte src) in
      ["alarm", V.from_list ["advance", V.new_value a; 
			     "advanceunits", V.new_value u]]
    else [] in
  let repeat =
    if (flagsraw land repeatFlag) <> 0 then
      let repeattype = getbyte src in
      let pad2 = getbyte src in
      let enddate = getshortBE src in
      let repeatfrequency = getbyte src in
      let repeaton = getbyte src in
      let repeatweekstart = getbyte src in
      let pad3 = getbyte src in
      ["repeat", 
       V.from_list
	 (Safelist.map (fun (k,i) -> (k, V.new_value (string_of_int i)))
	    [("type",repeattype); ("pad2",pad2); ("enddate",enddate);
	     ("frequency",repeatfrequency); ("on",repeaton);
	     ("weekstart",repeatweekstart); ("pad3",pad3)])]
    else [] in
  let except =
    if (flagsraw land exceptFlag) <> 0 then
      let numexcepts = getshortBE src in
      ["exceptions",
       V.structure_from_list
         (makelist numexcepts
            (fun _ -> V.new_value (string_of_int (getshortBE src))))]
    else [] in
  let desc = 
    if (flagsraw land descFlag) <> 0 then 
      ["desc",V.new_value (get_var_length_string src)]
    else [] in
  let note = 
    if (flagsraw land noteFlag) <> 0 then
      ["note",V.new_value (get_var_length_string src)]
    else [] in
  V.from_list (times @ alarm @ repeat @ note @ except @ desc)
  
let datebook_rcd_wr snk v =
  let beginhours = int_of_string (V.get_field_value v "beginhours") in
  let beginmins = int_of_string (V.get_field_value v "beginmins") in
  let endhours = int_of_string (V.get_field_value v "endhours") in
  let endmins = int_of_string (V.get_field_value v "endmins") in
  let pad1 = int_of_string (V.get_field_value v "pad1") in
  let oldflags = int_of_string (V.get_field_value v "flagsraw") in
  putbyte snk beginhours;
  putbyte snk beginmins;
  putbyte snk endhours;
  putbyte snk endmins;
  let rawdate = int_of_string (V.get_field_value v "date") in
  putshortBE snk rawdate;

  let has_alarm = ((V.get v "alarm") <> None) in
  let has_repeat = ((V.get v "repeat") <> None) in
  let has_except = ((V.get v "exceptions") <> None) in
  let has_desc = ((V.get v "desc") <> None) in
  let has_note = ((V.get v "note") <> None) in
  let set_bit byte bit = byte lor bit in
  let clear_bit byte bit = byte land (lnot bit) in
  let flags =
    Safelist.fold_left
      (fun acc (h,fl) -> if h then set_bit acc fl else clear_bit acc fl)
      oldflags [(has_alarm,alarmFlag); (has_repeat,repeatFlag); 
		(has_except,exceptFlag); (has_desc,descFlag); 
		(has_note,noteFlag)] in
  putbyte snk flags;
  putbyte snk pad1 (*0x00*);			(* padding *)
  if has_alarm then begin
    let alarmv = V.get_required v "alarm" in
    let a = int_of_string (V.get_field_value alarmv "advance") in
    let u = int_of_string (V.get_field_value alarmv "advanceunits") in
    putbyte snk a;
    putbyte snk u
  end;
  if has_repeat then begin
    let repeatv = V.get_required v "repeat" in
    let rtv = V.get_field_value repeatv "type" in
    let rtype = 
      try 
	int_of_string rtv 
      with Failure s ->
	Format.print_string ("FAILURE rtype is not an int but is: " ^ rtv);
	raise (Failure s)
    in
    let pad2 = int_of_string (V.get_field_value repeatv "pad2") in
    let enddate = int_of_string (V.get_field_value repeatv "enddate") in
    let freq = int_of_string (V.get_field_value repeatv "frequency") in
    let on = int_of_string (V.get_field_value repeatv "on") in
    let weekstart = int_of_string (V.get_field_value repeatv "weekstart") in
    let pad3 = int_of_string (V.get_field_value repeatv "pad3") in
    putbyte snk rtype;
    putbyte snk pad2 (*0x00*);			(*padding*)
    putshortBE snk enddate;
    putbyte snk freq;
    putbyte snk on;
    putbyte snk weekstart;
    putbyte snk pad3 (*0x00*)			(*padding*)
  end;
  if has_except then begin
    let ev = V.get_required v "exceptions" in
    let es = 
      Safelist.map (fun v -> int_of_string (V.get_value v)) 
	(V.list_from_structure ev) in
    let num_excepts = Safelist.length es in
    putshortBE snk num_excepts;
    Safelist.iter (putshortBE snk) es
  end;
  if has_desc then begin
    let desc = V.get_field_value v "desc" in
    Buffer.add_string snk desc; 
    Buffer.add_char snk '\000'
  end;
  if has_note then begin
    let n = V.get_field_value v "note" in
    Buffer.add_string snk n; 
    Buffer.add_char snk '\000'
  end

let datebook_rcd_rw = {rd=datebook_rcd_rd; wr=datebook_rcd_wr}

let format_time hs ms =
  let h = int_of_string hs in
  let m = int_of_string ms in
  Printf.sprintf "%02d:%02d" h m

let deformat_time s = 
  let norm s = string_of_int (int_of_string s) in
  match Misc.splitIntoWords s ':' with
    [h;m] -> (norm h,norm m)
  | _ -> assert false

let beginend_lens =
  let get vf =
      let beginhours = V.get_field_value vf "beginhours" in
      let beginmins = V.get_field_value vf "beginmins" in
      let endhours = V.get_field_value vf "endhours" in
      let endmins = V.get_field_value vf "endmins" in
      let basev = 
        V.set (V.set (V.set (V.set vf "endmins" None)
        "endhours" None)
        "beginmins" None)
        "beginhours" None in
      if (beginhours = "255") && (beginmins = "255") then basev else
        V.set_field_value 
        (V.set_field_value basev "end" (format_time endhours endmins))
        "begin" (format_time beginhours beginmins)
  in
  let put vf' _ =
      let begino = V.get_field_value_option vf' "begin" in
      let endo = V.get_field_value_option vf' "end" in
      let beginh,beginm,endh,endm =
        match begino,endo with
        | None,None -> "255","255","255","255"
        | Some(b),Some(e) -> 
            let bh,bm = deformat_time b in
            let eh,em = deformat_time e in
            bh,bm,eh,em
        | _ -> assert false in
      V.set_field_value (V.set_field_value (
        V.set_field_value (V.set_field_value (V.set (V.set vf' "end" None) "begin" None)
        "endmins" endm)
        "endhours" endh)
      "beginmins" beginm)
      "beginhours" beginh in
  Lens.native get put

let repeat_type_enum =
  Misc.enum 
    ["none"; "daily"; "weekly"; "monthlybyday"; "monthlybydate"; "yearly"]

let repeat_type_lens = compose (
    (Safelist.map
       (fun (i,k) -> old_rename (string_of_int i) k) repeat_type_enum))

let dayofweek_enum =
  Misc.enum
    ["Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"]

let dayofweek_lens = compose (
    (Safelist.map
       (fun (i,k) -> old_rename (string_of_int i) k) dayofweek_enum))

let dayofmonth_enum =
  Misc.enum
    ["1stSun";"1stMon";"1stTue";"1stWed";"1stThu";"1stFri";"1stSat";
     "2ndSun";"2ndMon";"2ndTue";"2ndWed";"2ndThu";"2ndFri";"2ndSat";
     "3rdSun";"3rdMon";"3rdTue";"3rdWed";"3rdThu";"3rdFri";"3rdSat";
     "4thSun";"4thMon";"4thTue";"4thWed";"4thThu";"4thFri";"4thSat";
     "LastSun";"LastMon";"LastTue";"LastWed";"LastThu";"LastFri";"LastSat"]

let dayofmonth_lens = compose (
    (Safelist.map
       (fun (i,k) -> old_rename (string_of_int i) k) dayofmonth_enum))

let unmarshall_date rawdate =
  Printf.sprintf "%02d-%02d-%04d"
    ((rawdate lsr 5) land 0xF) 
    (rawdate land 0x1F)
    ((rawdate lsr 9) + 1904)  
    
let marshall_date s =
  try
    let m = int_of_string (String.sub s 0 2) in
    let d = int_of_string (String.sub s 3 2) in
    let y = int_of_string (String.sub s 6 4) in
    d lor (m lsl 5) lor ((y - 1904) lsl 9)
  with Invalid_argument _ ->
    Misc.bad (Printf.sprintf "Invalid date: '%s'" s)

let parsedate_lens =
  let get v = V.new_value (unmarshall_date (int_of_string (V.get_value v))) in
  let put v' _ = V.new_value (string_of_int (marshall_date (V.get_value v'))) in
  Lens.native get put

let enddate_lens =
  let get vf =
      let datev = V.get_required vf "enddate" in
      let datestr = V.get_value datev in
      if (datestr = "65535") then
        V.set vf "enddate" None
      else
        V.set vf "enddate"
        (Some(V.new_value (unmarshall_date (int_of_string datestr))))
  in
  let put vf' _ =
      if (V.get vf' "enddate") = None then
        V.set vf' "enddate" (Some (V.new_value "65535"))
      else
        let datev' = V.get_required vf' "enddate" in
        let datestr' = V.get_value datev' in
        V.set vf' "enddate"
          (Some (V.new_value (string_of_int (marshall_date datestr')))) 
  in
  Lens.native get put

let repeat_on_lens =
  let get vf =
      let rtype = V.get_field_value vf "type" in
      let onint = int_of_string (V.get_field_value vf "on") in
      let vabs1 =
        match rtype with
        | "monthlybyday" -> 
          V.set_field_value vf "onday" (Safelist.assoc onint dayofmonth_enum)
        | "weekly" ->
            V.set vf "ondays"
            (Some (Safelist.fold_left
            (fun accv (i,daystr) ->
              if ((1 lsl i) land onint) <> 0 then
                V.set accv daystr (Some(V.empty))
              else accv)
            V.empty dayofweek_enum))
        | _ -> vf in
      V.set vabs1 "on" None
  in
  let put vf' _ = 
      let rtype = V.get_field_value vf' "type" in
      let onint,vconc1' = 
        match rtype with
        | "monthlybyday" ->
            Safelist.assoc (V.get_field_value vf' "onday")
              (Safelist.map (fun (i,s) -> (s,i)) dayofmonth_enum),
              V.set vf' "onday" None
        | "weekly" ->
            let dayv = V.get_required vf' "ondays" in
            V.fold (
              fun k _ acc ->
                let dayint = Safelist.assoc k
                (Safelist.map (fun (i,s) -> (s,i)) dayofweek_enum) in
                acc lor (1 lsl dayint))
            dayv 0,
            V.set vf' "ondays" None
        | _ -> 0,vf' in
    V.set_field_value vconc1' "on" (string_of_int onint) in
  Lens.native get put

let repeat_lens =
  compose [
  filter (Prd.neg (Prd.s "pad2"))
    (V.set_field_value V.empty "pad2" (string_of_int 0x00));
  filter (Prd.neg (Prd.s "pad3"))
    (V.set_field_value V.empty "pad3" (string_of_int 0x00));
  Lens.tracepoint "enddate_lens" enddate_lens;
  Lens.tracepoint "repeat_type_lens" (
    mapp (Prd.s "type") repeat_type_lens;
  );
  Lens.tracepoint "dayofweek_lens" (
    mapp (Prd.s "weekstart") dayofweek_lens;
  );
  Lens.tracepoint "repeat_on_lens" repeat_on_lens;
]

let datebooklens =
  compose [
  Palm.generic_pdb_rcd_lens;
  map
    (Lens.tracepoint "datebook record" (
     compose [
     Lens.tracepoint "datebook_rcd_rw" (Palm.rw_lens datebook_rcd_rw);
     filter (Prd.neg (Prd.s "pad1"))
       (V.set_field_value V.empty "pad1" (string_of_int 0x00));
     filter (Prd.neg (Prd.s "flagsraw"))
       (V.set_field_value V.empty "flagsraw" (string_of_int 0x00));
     mapp (Prd.s "date")
       (Lens.tracepoint "date" parsedate_lens);
     Lens.tracepoint "beginend_lens" beginend_lens;
     xfork (Prd.m ["begin";"end"]) (Prd.s "time")
       (efork (plunge "time"))
       id;
     (* next trick: if there are exceptions, then there must be repeat
	information. we want to move the exceptions into the repeat part of
	the record *)
     mapp (Prd.s "repeat")
       (Lens.tracepoint "repeat_lens" repeat_lens);
     mapp (Prd.s "exceptions") (list_map parsedate_lens);
     Lens.tracepoint "repeat/except" (
     xfork (Prd.m ["repeat";"exceptions"]) (Prd.s "repeat")
       (Lens.tracepoint "repeathoist" (
        compose [
	xfork (Prd.s "exceptions") (Prd.s "exceptions")
	  id
	  (efork (hoist "repeat"));
	efork (plunge "repeat")
        ]
      ))
       id;
   );
   ])
    )
]

let rtype_lens =
  let get vf =
      match (V.get_value (V.get_required vf "type")) with
      | "monthlybyday" ->
	V.set V.empty "type"
	  (Some
	     (V.set V.empty "monthlybyday"
		(Some (V.new_value (V.get_field_value vf "onday")))))
      | "weekly" ->
          V.set V.empty "type" (
            Some (V.set V.empty "weekly" (Some (V.get_required vf "ondays"))))
      | rt -> V.set V.empty "type" (Some (V.new_value rt)) 
  in
  let put vf' _ =
      let typev = V.get_required vf' "type" in
      let tv =
        let tdom = V.dom typev in
        assert (Name.Set.cardinal tdom = 1);
        Name.Set.choose tdom
      in
      match tv with
      | "monthlybyday" ->
        V.set
        (V.set_field_value V.empty "type" "monthlybyday")
        "onday" (Some (V.get_required typev "monthlybyday"))
      | "weekly" ->
          V.set
          (V.set_field_value V.empty "type" "weekly")
          "ondays" (Some (V.get_required typev "weekly"))
      | rt -> 
          V.set_field_value V.empty "type" rt
      in
  Lens.native get put

let dbk2ical_lens =
  map
    (compose [
     (* hide all notes *)
     filter (Prd.neg (Prd.s "note")) V.empty;
     mapp (Prd.s "alarm")
       (compose [
        old_rename "advance" "notice";
        old_rename "advanceunits" "units";
	(* XXX should check that the following is the correct interpretation
	   of advanceunits *)
	mapp (Prd.s "units")
          (compose [
            old_rename "0" "Minutes";
            old_rename "1" "Hours";
            old_rename "2" "Days"])
      ]);
     mapp (Prd.s "repeat")
       (compose [
        old_rename "frequency" "freq";
	xfork (Prd.m ["type";"onday";"ondays"]) (Prd.s "type")
	  rtype_lens
	  id;
	filter (Prd.neg (Prd.s "weekstart"))
	  (V.set_field_value V.empty "weekstart" "Sun");
      ]);
   ])

let _ =
  (* FIXME *)
  let etest filename copt = false in
  let encoding = {
    Surveyor.description = "Palm date book";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Plain.reader;
    Surveyor.writer = Plain.writer;
    Surveyor.from_string = (fun s -> Some (V.new_value s));
    Surveyor.to_string = (fun v -> V.get_value v);
    Surveyor.base_type = ["palm"; "datebook"];
  }
  in
    (* FIXME: i don't know how this should work! *)
    Surveyor.register_encoding "pdbk" encoding;
    Optometrist.register_lens ["palm"; "datebook"] ["datebook"] 
      Schemas.empty
      datebooklens;
    Optometrist.register_lens ["datebook"] ["appointments"] 
      Schemas.appointments 
      dbk2ical_lens;
    Key_factory.register_keyfactory Palm.ugen_factory ("pdbk", ["datebook"]);
    Key_factory.register_keyfactory Palm.ugen_factory ("pdbk", ["appointments"])
