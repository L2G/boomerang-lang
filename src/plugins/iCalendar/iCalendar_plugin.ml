(* TODO:
 * change repeat exception representation from list to set
*)

open ICalendar_syntax
open Pervasives_plugin
open Experimental_plugin

(* deprecated lenses from v2 src *)
let rec compose = function
    [] -> id
  | [l] -> l
  | l::t -> compose2 l (compose t)

let tracepoint s ll = Lens.tracepoint s (compose ll)
let rec map_list l = wmap (fun n -> 
			 if (n = V.head_tag) then l
			 else if (n = V.tail_tag) then (map_list l)
			 else id)

(* extract: takes a predicate and a list, and returns a list: either the list
 * if no element of it satisfies the predicate, or the first element satisfying
 * the predicate as the head of the new list, and the rest of the list without
 * this element as the tail of the list. 
*) 

(* TIDY: still needs to be converted *)
let extract p = Lens.native
    ( fun v ->
      let l = V.list_from_structure v in
      let rec loop acc = function
        | [] -> Safelist.rev acc
        | elt :: rest ->
            if p elt then
              elt :: (Safelist.rev_append acc rest)
            else
              loop (elt :: acc) rest
      in
      V.structure_from_list (loop [] l)
    )
    ( fun v' v ->
      let l' = V.list_from_structure v' in
      let l = 
        match v with
        | None -> []
        | Some vv -> V.list_from_structure vv in
      let elt, rest =
        match l' with
        | elt :: rest -> elt, rest
        | [] -> V.error_msg [
          `String "The abstract view should contain at least one element";
          `View v']
      in
      let rec loop acc = function
        | l1', [] ->
            (* the elt was not in the concrete list, we put it in first position *)
            elt :: (Safelist.rev_append acc l1')
        | (e' :: r') as l1', e :: r ->
            if p e then
              (* the element should go here, we're done *)
              Safelist.rev_append acc (elt :: l1')
            else
              loop (e' :: acc) (r',r)
        | [], _ ->
            (* we got to the end of the abstract list without finding the element
            * in the concrete view. It might be the next one, or later, so we
            * just put it at the end *)
            Safelist.rev (elt :: acc)
      in
      V.structure_from_list (loop [] (rest, l))
    )

let zfork asp afu csp cfu l1 l2 =
  let check_put a a1 a2 c' c1' c2' =
    let c1'', c2'' = csp c' in
    if not (V.equal (c1') (c1'')) then
      Lens.error [`String "zfork (put or create): c1' = l1 \\ a1 c1:"; `View (c1');
             `String "is different from fst(Cs(Cf(c1', c2'))):"; `View (c1'')];
    if not (V.equal (c2') (c2'')) then
      Lens.error [`String "zfork (put or create): c2' = l2 \\ a2 c2:"; `View (c2');
             `String "is different from snd(Cs(Cf(c1', c2'))):"; `View (c2'')];
    let af = afu a1 a2 in
    if not (V.equal (a) (af)) then
      Lens.error [`String "zfork (put or create): a:"; `View (a);
             `String "is different from Af(As(a)):"; `View (af)];
    let a1', a2' = asp a (Some c') in
    if not (V.equal (a1) (a1')) then
      Lens.error [`String "zfork (put or create): fst(As(a,c)):"; `View (a1);
             `String "is different from fst(As(a,c')):"; `View (a1')];
    if not (V.equal (a2) (a2')) then
      Lens.error [`String "zfork (put or create): snd(As(a,c)):"; `View (a2);
             `String "is different from snd(As(a,c')):"; `View (a2')];
    ()
  in
  Lens.native
    (fun c ->
      let c1, c2 = csp c in
      let a1' = Lens.get l1 c1 in
      let a2' = Lens.get l2 c2 in
      let a' = afu a1' a2' in

      let a1'', a2'' = asp a' (Some c) in
      if not (V.equal (a1') (a1'')) then
      Lens.error [`String "zfork (get): l1 / c:"; `View (a1');
             `String "is different from fst(As(Af(l1/c, l2/2),c)):"; `View (a1'')];
    if not (V.equal (a2') (a2'')) then
      Lens.error [`String "zfork (get): l2 / c:"; `View (a2');
             `String "is different from snd(As(Af(l1/c, l2/c),c)):"; `View (a2'')];
    let cf = cfu c1 c2 in
    if not (V.equal (c) (cf)) then
      Lens.error [`String "zfork (get): c:"; `View (c);
             `String "is different from Cf(Cs(c)):"; `View (cf)];
    a')
    (fun a c ->
      let a1, a2 = asp a c in
      let c1, c2 = 
        match c with 
        | None -> None, None
        | Some c -> (fun (c1,c2) -> Some c1, Some c2) (csp c) in
      let c1' = Lens.put l1 a1 c1 in
      let c2' = Lens.put l2 a2 c2 in
      let c' = cfu c1' c2' in
      check_put a a1 a2 c' c1' c2';
      c'
    )

let flatten_atomic_list =
  let check_list direction v =
    if (not (V.is_list v)) then
      Lens.error [`String "Lens.flatten_atomic_list(";
             `String direction;
             `String "): was expecting a list, got: ";
	     `View v];
    let kids = V.list_from_structure v in
    let _,newkidsrev =
      Safelist.fold_left
        (fun (domacc,kidrev) kid -> 
           let dom = V.dom kid in
             if Name.Set.is_empty dom then
               Lens.error [
		 `String "Lens.flatten_atomic_list("; `String direction;
		 `String "): one element is the empty view:";
		 `View v]
             else
               let i = Name.Set.inter domacc dom in
		 if Name.Set.is_empty i then
		   Name.Set.union domacc dom, kid:: kidrev
		 else
		   let istr =
		     String.concat ", " (Safelist.map Misc.whack (Name.Set.elements i))
		   in
		     Lens.error [
		       `String "Lens.flatten_atomic_list("; `String direction;
		       `String "): some of the list elements ";
		       `String "do not have disjoint domains (multiple occurrences of ";
		       `String istr; `String ")"; `View v];)
        (Name.Set.empty,[]) kids
    in
      Safelist.rev newkidsrev
  in
  let extra_single leftover v' =
    let leftoverk's = Safelist.sort compare (Name.Set.elements leftover) in
    Safelist.map (fun k -> V.set V.empty k (Some (V.get_required v' k))) leftoverk's
  in
  let get v = 
    (
      let vf = v in
      let kids = check_list "get" vf in
      Safelist.fold_left (fun vacc kid -> V.concat vacc kid) V.empty kids) in
  let put v' v = 
    let v =
      match v with
      | None -> V.empty_list
      | Some c -> c
    in
    let vf = v in
    let kids = check_list "put" vf in
      (* for each element of the original list, see if it is in the new
	 abstract view. if so, retain the new version, else discard
	 it. remember any leftover elements of the new abstract view's domain *)
    let kids'rev,leftover =
      Safelist.fold_left (
        fun (kids'acc,dom) kid ->
          let inter = Name.Set.inter dom (V.dom kid) in
            if Name.Set.is_empty inter then
	      (kids'acc, dom)
	    else 
	      let kkid' = 		
		  V.set_star V.empty (
		    Name.Set.fold (
		      fun k acc -> (k,Some (V.get_required v' k))::acc)
					inter []) in
		(kkid'::kids'acc, Name.Set.diff dom inter))
        ([],V.dom v') kids in
      (* take the remaining elements in some order and put at the end of the
	 new concrete list *)
    let kids' = Safelist.rev_append kids'rev (extra_single leftover v') in
      V.structure_from_list kids'
  in
    Lens.native get put

(* the following lens is used to emulate the idea of equivalence classes.
 * It should be used when several equivalent representations are allowed
 * to convert them to the same abstract representation.
*)
let equi_merge p f =
  Lens.native 
    (fun c ->    
       let vf = c in
	 if p vf then
	   let v' = f vf in
	     if not (p v') then 
	       v' 
	     else 
	       Lens.error [
		 `String "equi_merge(get): concrete view satisfied the predicate:";
		 `View vf;
		 `String "but result of applying the function to it also did:";
		 `View v']
	 else
	   vf
    )
    (fun a co ->
       match co with 
	   None -> a
	 | Some c -> 
	     let vf' = c in
	       if p vf' then
		 Lens.error [
		   `String "equi_merge(put): the abstract view satisfied the predicate:";
		   `View vf']
	       else begin
		 let vf = a in
		   if p vf then
		     if (V.equal vf' (f vf)) then 
		       vf
		     else
		       vf'
		   else
		     vf'
	       end
    )

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


(* end deprecated section *)

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
  (`String s) :: (`Break) :: (List.map (fun v -> `View v) vl)

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
          try
          ( V.get_value (V.get_required e "name"),
            Safelist.map V.get_value (V.list_from_structure (V.get_required e "val"))
          )
          with
          | V.Illformed (s, vl) -> V.error_msg ( (`String "xplist not well formed") :: 
                                                 (`View e) ::
                                                 print_illformed_exc s vl)
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
              (try
                 Some (bool_from_view (V.get_required v "global"),
                       V.get_value (V.get_required v "val"))
               with
               | V.Illformed (s, vl) -> V.error_msg ( (`String "view should have 'global' and 'val'") :: 
                                                      (`View v) ::
                                                      print_illformed_exc s vl)
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
  try
    { attachparam = allparams_from_view_opt (V.get v "params");
      attachval   =
        let v' = V.get_required v "val" in
        match V.get v' "AttBinary", V.get v' "AttUri" with
        | Some c, None -> AttBinary (V.get_value c)
        | None, Some c -> AttUri (V.get_value c) 
        | _ -> V.error_msg [`String "View should have exactly one of AttBinary of AttUri";
                            `View v]
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in attach_from_view, missing 'val':") :: 
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_attendee a =
  V.from_list
    (("val", V.new_value a.attendeeval)
     :: 
     (viewpair_from_allparams a.attendeeparam))
      
let attendee_from_view v =
  try
    { attendeeparam = allparams_from_view_opt (V.get v "params");
      attendeeval   = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in attendee_from_view, missing 'val':") :: 
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_categories c =
  V.from_list
    (("val", V.structure_from_list (Safelist.map V.new_value c.categoriesval))
     :: 
     (viewpair_from_allparams c.categoriesparam))
      
let categories_from_view v =
  try
    { categoriesparam = allparams_from_view_opt (V.get v "params");
      categoriesval   = Safelist.map V.get_value (V.list_from_structure (V.get_required v "val"))
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in categories_from_view, missing 'val':") :: 
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_comment a =
  V.from_list
    (("val", V.new_value a.commenttext)
     :: 
     (viewpair_from_allparams a.commentparam))
      
let comment_from_view v =
  try
    { commentparam = allparams_from_view_opt (V.get v "params");
      commenttext   = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in comment_from_view, missing 'val':") :: 
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_contact a =
  V.from_list
    (("val", V.new_value a.contacttext)
     :: 
     (viewpair_from_allparams a.contactparam))
      
let contact_from_view v =
  try
    { contactparam = allparams_from_view_opt (V.get v "params");
      contacttext   = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in contact_from_view, missing 'val':") :: 
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_date { year = y; month = m; day = d} =
  V.from_list [
    "year", V.new_value (Printf.sprintf "%04d" y);
    "month", V.new_value (Printf.sprintf "%02d" m);
    "day", V.new_value (Printf.sprintf "%02d" d)
  ]

let date_from_view v =
  try
    { year = int_of_string (V.get_value (V.get_required v "year"));
      month = int_of_string (V.get_value (V.get_required v "month"));
      day = int_of_string (V.get_value (V.get_required v "day"));
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in date_from_view, missing 'year', 'month', or 'day':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_time { hour = h; minute = m; second = s; zulu = z } =
  V.from_list [
    "hour", V.new_value (Printf.sprintf "%02d" h);
    "minute", V.new_value (Printf.sprintf "%02d" m);
    "second", V.new_value (Printf.sprintf "%02d" s);
    "zulu", view_from_bool z
    ]

let time_from_view v =
  try
    { hour = int_of_string (V.get_value (V.get_required v "hour"));
      minute = int_of_string (V.get_value (V.get_required v "minute"));
      second = int_of_string (V.get_value (V.get_required v "second"));
      zulu = bool_from_view (V.get_required v "zulu");
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in time_from_view, missing 'hour', 'minute', 'second', or 'zulu:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_date_time { date = d; time = t} =
  V.from_list [ "date", view_from_date d; "time", view_from_time t]
  
let date_time_from_view v =
  try
    { date = date_from_view (V.get_required v "date");
      time = time_from_view (V.get_required v "time");
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in date_time_from_view, missing 'date' or 'time':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_dtxp (xpl,dt) =
  V.from_list
    ( ("val", (view_from_date_time dt)) ::
       viewpair_from_xplist xpl)

let dtxp_from_view v =
  try
    ( xplist_from_view_opt (V.get v "xplist"), date_time_from_view (V.get_required v "val"))
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in dtxp_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_description d =
  V.from_list
    (("val", V.new_value d.desctext)
     :: 
     (viewpair_from_allparams d.descparam))
      
let description_from_view v =
  try
    { descparam = allparams_from_view_opt (V.get v "params");
      desctext  = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in description_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_dur_time { dur_hour = h; dur_minute = m; dur_second = s } =
  V.from_list ["hour", V.new_value (string_of_int h);
               "minute", V.new_value (string_of_int m);
               "second", V.new_value (string_of_int s)]

let dur_time_from_view v =
  try
    { dur_hour = int_of_string (V.get_value (V.get_required v "hour"));
      dur_minute = int_of_string (V.get_value (V.get_required v "minute"));
      dur_second = int_of_string (V.get_value (V.get_required v "second"))
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "dur_time has 'hour', 'minute', or 'second' missing:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_dur_date = function
  | i, None -> V.from_list ["Day", V.new_value (string_of_int i)]
  | i, Some t -> V.from_list ["Day", V.new_value (string_of_int i); "Time", view_from_dur_time t]

let dur_date_from_view v =
  try
    (int_of_string (V.get_value (V.get_required v "Day")),
     match V.get v "Time" with
     | None -> None
     | Some t -> Some (dur_time_from_view t))
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "missing 'Day' in dur_date:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
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
                      `View v]
  
let view_from_duration {dur_neg = b; dur_length = dl} =
  V.from_list ["negative", view_from_bool b;
               "length", view_from_dur_length dl] 

let duration_from_view v =
  try
    { dur_neg = bool_from_view (V.get_required v "negative");
      dur_length = dur_length_from_view (V.get_required v "length") }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in duration_from_view, missing 'negative' or 'length':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_period = function
  | PeriodExplicit (ds, de) -> V.from_list ["PeriodExplicit",
                                               V.from_list ["start", view_from_date_time ds;
                                                            "end",   view_from_date_time de]]
  | PeriodStart (ds, dur) -> V.from_list ["PeriodStart",
                                               V.from_list ["start", view_from_date_time ds;
                                                            "duration", view_from_duration dur]]

let period_from_view v =
  try
    match V.get v "PeriodExplicit", V.get v "PeriodStart" with
    | Some v', None -> PeriodExplicit (date_time_from_view (V.get_required v' "start"), 
                                       date_time_from_view (V.get_required v' "end"))
    | None, Some v' -> PeriodStart (date_time_from_view (V.get_required v' "start"), 
                                    duration_from_view (V.get_required v' "duration"))
    | _ -> V.error_msg [`String "This view should have exactly one of ";
                        `String "PeriodExplicit or PeriodStart:";
                        `View v]
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "not well-formed period:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
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
                      `View v]
  
let view_from_dt d =
  V.from_list
    (("val", view_from_dtpval d.dtval)
     :: 
     (viewpair_from_allparams d.dtparam))
      
let dt_from_view v =
  try
    { dtparam = allparams_from_view_opt (V.get v "params");
      dtval  = dtpval_from_view (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in dt_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_dtpl d = 
  V.from_list
    (("val", V.structure_from_list (Safelist.map view_from_dtpval d.dtplval))
     :: 
     (viewpair_from_allparams d.dtplparam))

let dtpl_from_view v =
  try
    { dtplparam = allparams_from_view_opt (V.get v "params");
      dtplval  = Safelist.map dtpval_from_view (V.list_from_structure (V.get_required v "val"))
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in dtpl_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
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
      | _ -> V.error_msg [`String "This recur_end is not well-formed:"; `View v]
      )
  | None -> RecNone
  
let view_from_bydayelt (i, w) =
  V.from_list (
    ("weekday", V.new_value (ICalendar.tostring ICalendar_print.print_weekday w)) ::
    (match i with
    | None -> []
    | Some i -> ["which", V.new_value (string_of_int i)]))

let bydayelt_from_view v =
  try
    (
      (match V.get v "which" with
      | None -> None
      | Some i -> Some (int_of_string (V.get_value i))),
      ICalendar_lextypes.weekday_from_string (V.get_value (V.get_required v "weekday"))
    )
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "missing 'weekday' value:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
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
  try
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
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "this recur view is not well formed:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_location l =
  V.from_list
    (("val", V.new_value l.loctext)
     :: 
     (viewpair_from_allparams l.locparam))
      
let location_from_view v =
  try
    { locparam = allparams_from_view_opt (V.get v "params");
      loctext  = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in location_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_organizer o =
  V.from_list
    (("val", V.new_value o.org_cal_address)
     :: 
     (viewpair_from_allparams o.orgparam))
      
let organizer_from_view v =
  try
    { orgparam = allparams_from_view_opt (V.get v "params");
      org_cal_address  = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in organizer_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_recurid r =
  V.from_list
    (("val", view_from_dtpval r.recurval)
     :: 
     (viewpair_from_allparams r.recurparam))
  
let recurid_from_view v =
  try
    { recurparam = allparams_from_view_opt (V.get v "params");
      recurval  = dtpval_from_view (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in recurid_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_related_to r =
  V.from_list
    (("val", V.new_value r.relatedtotext)
     :: 
     (viewpair_from_allparams r.relatedtoparam))
      
let related_to_from_view v =
  try
    { relatedtoparam = allparams_from_view_opt (V.get v "params");
      relatedtotext  = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in related_to_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_resources r =
  V.from_list
    (("val", V.structure_from_list (Safelist.map V.new_value r.resourcesval))
     :: 
     (viewpair_from_allparams r.resourcesparam))
      
let resources_from_view v =
  try
    { resourcesparam = allparams_from_view_opt (V.get v "params");
      resourcesval  = Safelist.map V.get_value (V.list_from_structure (V.get_required v "val"))
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in resources_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

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
  try
    { rstatparam = allparams_from_view_opt (V.get v "params");
      statcode  = Safelist.map (fun e -> int_of_string (V.get_value e)) 
                        (V.list_from_structure (V.get_required v "code"));
      stattext = V.get_value (V.get_required v "text");
      extdata = 
        match V.get v "extdata" with
        | None -> None
        | Some t -> Some (V.get_value t) 
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in rstatus_from_view, missing 'code' or 'text':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_summary s =
  V.from_list
    (("val", V.new_value s.sumtext)
     :: 
     (viewpair_from_allparams s.sumparam))
      
let summary_from_view v =
  try
    { sumparam = allparams_from_view_opt (V.get v "params");
      sumtext  = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in summary_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_trigger t =
  V.from_list
    (("val", view_from_dtpval t.triggerval)
     :: 
     (viewpair_from_allparams t.triggerparam))
  
let trigger_from_view v =
  try
    { triggerparam = allparams_from_view_opt (V.get v "params");
      triggerval  = dtpval_from_view (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in trigger_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
  
let view_from_tzname t =
  V.from_list
    (("val", V.new_value t.tznameval)
     :: 
     (viewpair_from_allparams t.tznameparam))
      
let tzname_from_view v =
  try
    { tznameparam = allparams_from_view_opt (V.get v "params");
      tznameval  = V.get_value (V.get_required v "val")
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "in tzname_from_view, missing 'val':") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

let view_from_offset_time { positive = p; off_h = h; off_m = m; off_s = s } =
  V.from_list ["positive", view_from_bool p;
               "hour", V.new_value (string_of_int h);
               "minute", V.new_value (string_of_int m);
               "second", V.new_value (string_of_int s)]

let offset_time_from_view v =
  try
    { positive = bool_from_view (V.get_required v "positive");
      off_h = int_of_string (V.get_value (V.get_required v "hour"));
      off_m = int_of_string (V.get_value (V.get_required v "minute"));
      off_s = int_of_string (V.get_value (V.get_required v "second"))
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "offset_time has 'positive', 'hour', 'minute', or 'second' missing:") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)

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
  try
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
                                `View v]
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
                                `View v]
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
                                `View v]
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
                                `View v]
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
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "view does not correspond to component property") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)


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
  try
    let v' = V.get_required v "val" in
    match (V.get_value (V.get_required v "type")) with
    | "Eventc" -> Eventc { event_comp = comp_prop_from_view (V.get_required v' "props");
                           event_alarms = component_list_from_view_opt (V.get v' "alarms") }
    | "Timezonec" -> Timezonec (comp_prop_from_view v')
    | s -> V.error_msg [`String s; `String " is not implemented yet in "; `View v]
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "view does not correspond to component") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)
      
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
  try
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
    }
  with
  | V.Illformed (s, vl) -> V.error_msg ( (`String "view does not correspond to calprops") ::
                                         (`View v) ::
                                         print_illformed_exc s vl)


let view_from_icalobject { calprops = p; components = c} = 
  V.from_list ["calprops", view_from_calprops p; "components", view_from_components c]

let icalobject_from_view v =
  match V.get v "calprops", V.get v "components" with
  | Some vp, Some vc -> { calprops = calprops_from_view vp; components = components_from_view vc }
  | _, _ -> V.error_msg [`String "missing 'calprops' or 'components'"; `View v]

let view_from_icalendar i = 
  V.structure_from_list (Safelist.map view_from_icalobject i)

let icalendar_from_view v = 
  Safelist.map icalobject_from_view (V.list_from_structure v)

let now () =
  let tm = Unix.localtime (Unix.time ()) in
  { date = { year = tm.Unix.tm_year + 1900;
             month = tm.Unix.tm_mon + 1;
             day = tm.Unix.tm_mday };
    time = { hour = tm.Unix.tm_hour;
             minute = tm.Unix.tm_min;
             second = tm.Unix.tm_sec;
             zulu = false } }
  
let time_parse s =
  try
      Scanf.sscanf s "%2s:%2s" (fun a b -> a,b) 
  with
  | Scanf.Scan_failure _ -> Scanf.sscanf s "%1s:%2s" (fun a b -> a,b) 

(* assumes a view with exactly children "hour" and "minute" *)
let time_str_lens =
  let tp s =
    let h,m = time_parse s in
    V.from_desc (V.V ["hour", V.Val h; "minute", V.Val m]) in
  Lens.native
    (fun vf ->
        if not (Name.Set.equal (Name.Set.add "hour" 
                               (Name.Set.add "minute" 
                                 Name.Set.empty)) (V.dom vf)) then
          Lens.error [
            `String "This view should exactly have \"hour\" and \"minute\" as children:";
            `View vf ]
        else
          V.new_value (Printf.sprintf "%s:%s" (V.get_value (V.get_required vf "hour"))
                                              (V.get_value (V.get_required vf "minute"))))
    (fun v' _ ->
       tp (V.get_value v'))
    
let date_parse s =
  let m,d,y = Scanf.sscanf s "%2s-%2s-%4s" (fun a b c -> a,b,c) in
  y,m,d
    
let date_to_view s =
  let y,m,d = date_parse s in
  V.from_desc (V.V ["day", V.Val d; "month", V.Val m; "year", V.Val y])
    
(* assumes a view with exactly children "year", "month", and "day" *)
let date_str_lens = 
  Lens.native
    (fun vf ->
        if not (Name.Set.equal (Name.Set.add "year" 
                               (Name.Set.add "month" 
                               (Name.Set.add "day" 
                                Name.Set.empty))) (V.dom vf)) then
          Lens.error [
            `String "This view should exactly have \"year\", \"month\", and \"day\" as children:";
            `View vf ]
        else
          V.new_value (Printf.sprintf "%s-%s-%s" (V.get_value (V.get_required vf "month"))
                                                 (V.get_value (V.get_required vf "day"))
                                                 (V.get_value (V.get_required vf "year"))))
    ( fun v' _ -> date_to_view (V.get_value v'))
  
let fix_dates create_fun =
  Lens.native
    (fun v -> V.set v "date_end" None) 
    (fun vf' vfo ->
       match vfo with
         None ->
          (* Creation case: *)
          let d = V.get_required vf' "date_start" in
          V.from_list ["date_start", d; "date_end", (create_fun d)]
       | Some vf -> 
          match V.get vf "date_start", V.get vf "date_end" with
          | Some ds, Some de ->
              let dds = date_from_view ds in
              let dde = date_from_view de in
              let ts, _ = Unix.mktime {
                Unix.tm_sec   = 0;
                Unix.tm_min   = 0;
                Unix.tm_hour  = 0;
                Unix.tm_mday  = dds.day;
                Unix.tm_mon   = dds.month - 1;
                Unix.tm_year  = dds.year - 1900;
                Unix.tm_wday  = 0;
                Unix.tm_yday  = 0;
                Unix.tm_isdst = false; } in 
              let te, _ = Unix.mktime {
                Unix.tm_sec   = 0;
                Unix.tm_min   = 0;
                Unix.tm_hour  = 0;
                Unix.tm_mday  = dde.day;
                Unix.tm_mon   = dde.month - 1;
                Unix.tm_year  = dde.year - 1900;
                Unix.tm_wday  = 0;
                Unix.tm_yday  = 0;
                Unix.tm_isdst = false; } in 
              let time_diff = (te -. ts) in
              if time_diff < 0. || time_diff > (24. *. 3600.) then
                V.error_msg [`String "Cannot faithfully represent an appointment with these dates"; `View vf; `String ("Diff in time = " ^ (string_of_float time_diff))];
              let newd = V.get_required vf' "date_start" in
              if time_diff = 0. then
                V.from_list ["date_start", newd; "date_end", newd]
              else
                let newdd = date_from_view newd in
                let newdde = compute_end_date_and_time ~days:1
                  { date = newdd;
                    time = { hour = 0; minute = 0; second = 0; zulu = false} } in
                V.from_list ["date_start", newd; "date_end", (view_from_date newdde.date) ]
        | Some ds, None -> vf'
        | _, _ -> assert false
    )
  
let wkday_to_short_str = function
  | Monday -> "Mon"
  | Tuesday -> "Tue"
  | Wednesday -> "Wed"
  | Thursday -> "Thu"
  | Friday -> "Fri"
  | Saturday -> "Sat"
  | Sunday -> "Sun"

let short_str_to_wkday = function
  | "Mon" -> Monday   
  | "Tue" -> Tuesday  
  | "Wed" -> Wednesday
  | "Thu" -> Thursday 
  | "Fri" -> Friday   
  | "Sat" -> Saturday 
  | "Sun" -> Sunday   
  | s     -> Lens.error [
    `String "this is no a well-formed short weekday:";
    `String s]
  
type rtype =
  | RDaily
  | RWeekly of weekday list
  | RMonthlyByDay of (int * weekday)
  | RMonthlyByDate
  | RYearly
  
type appt_repeat =
  { rtype : rtype;
    rfreq : int;
    renddate : date option;
    rexceptions : date list;
  }

let view_from_appt_repeat a errp=
  let vty =
    match a.rtype with
    | RDaily -> V.new_value "daily"
    | RWeekly l -> 
        V.set V.empty "weekly" (Some (
          V.structure_from_list (
            List.map (
              fun w -> V.new_value (wkday_to_short_str w)) l)))
    | RMonthlyByDay (i,w) -> 
        V.set V.empty "monthlybyday"  (Some (
          V.new_value(
            (match i with
            | 1 -> "1st"
            | 2 -> "2nd"
            | 3 -> "3rd"
            | 4 -> "4th"
            | -1 -> "Last"
            | _ -> Lens.error [
              `String errp;
              `String "cannot convert a monthlybyday number that is not -1, 1, 2, 3, or 4. Got:";
              `String (string_of_int i)
            ])
            ^
            (wkday_to_short_str w)
          )))
    | RMonthlyByDate -> V.new_value "monthlybydate"
    | RYearly -> V.new_value "yearly"
  in
  V.from_desc (
    V.V (
      ("type", V.In vty) ::
      ("freq", V.Val (string_of_int a.rfreq)) :: (
        (match a.renddate with
        | None -> []
        | Some d -> ["enddate", V.In (Lens.get date_str_lens (view_from_date d))]) @
        (match a.rexceptions with
        | [] -> []
        | l -> ["exceptions", V.In (
          V.structure_from_list (
            List.map (
              fun d -> Lens.get date_str_lens (view_from_date d))
            l))])))
  )
  
let appt_repeat_from_view v =
  try
    let vty = V.get_required v "type" in
    let fr = int_of_string (V.get_value (V.get_required v "freq")) in
    let endd = match V.get v "enddate" with
    | None -> None
    | Some v -> 
        let y,m,d = date_parse (V.get_value v) in
        Some { year = int_of_string y; month = int_of_string m; day = int_of_string d;}
    in
    let t =
      match (
        let d = V.dom vty in
          assert (Name.Set.cardinal d = 1);
          Name.Set.choose d)
      with
      | "daily" -> RDaily
      | "weekly" ->
          RWeekly (
            List.map (fun v -> short_str_to_wkday (V.get_value v))
                     (V.list_from_structure (V.get_required vty "weekly")))
      | "monthlybyday" ->
          let s = (V.get_value (V.get_required vty "monthlybyday")) in
          RMonthlyByDay (
            try Scanf.sscanf s "%d%2s%s" (fun a _ b -> a, short_str_to_wkday b) 
            with
            | Scanf.Scan_failure _ -> Scanf.sscanf s "Last%s" (fun b -> -1, short_str_to_wkday b))
      | "monthlybydate" -> RMonthlyByDate
      | "yearly" -> RYearly
      | _ -> failwith "repeat type not understood"
    in
    let ex = match V.get v "exceptions" with
    | None -> []
    | Some l ->
        List.map (fun v -> let y,m,d = date_parse (V.get_value v) in
                           { year = int_of_string y; month = int_of_string m; day = int_of_string d;})
                 (V.list_from_structure l)
    in
    { rtype = t;
      rfreq = fr;
      renddate = endd;
      rexceptions = ex;
    }
  with
  | e -> V.error_msg [
           `String "error when building an appt_repeat from the view ";
           `View v;
           `String "got the exception: ";
           `String (Printexc.to_string e)]

let appt_repeat_from_recur r dtpl (y,m,d) errp=
  let rt =
    match r.recur_freq with
          | Daily -> RDaily
          | Weekly -> 
              (match r.recur_byday with
              | [] -> RWeekly [ compute_day_of_week y m d ]
              | l -> RWeekly (List.map (fun (o, wd) -> 
                                          match o with
                                          | None -> wd
                                          | _ -> Lens.error [
                                            `String errp;
                                            `String "weekly reapeat should not specify the numbered weekday within a month"]) 
                              l)
              )
          | Monthly ->
              (match r.recur_byday, r.recur_bymonthday with
              | [],[] ->
                  Lens.error [
                    `String errp;
                    `String "tell Alan to implement current day monthly repeat"]
              | [o,wd],[] -> RMonthlyByDay (match o with
                                         | Some i -> i,wd
                                         | _ -> Lens.error [
                                           `String errp;
                                           `String "monthly reapeat should specify the numbered weekday within a month"]) 
              | _,[] -> Lens.error [
                `String errp;
                `String "Monthly by day only support one day at the moment"]
              | [],[i] ->
                  if i <> (int_of_string d) then
                     Lens.error [
                       `String errp;
                       `String "cannot deal with a monthly by date appointment that has a day different from the one in the record"]
                  else RMonthlyByDate
              | [], _ -> Lens.error [
                `String errp;
                `String "Monthly by date only support one date at the moment"]
              | _, _ -> Lens.error [
                `String errp;
                `String "cannot specify both monthly by day and monthly by date"]
              )
          | Yearly ->
              (match r.recur_byday, r.recur_bymonthday with
              | [], [] -> RYearly
              | _, _ -> Lens.error [
                `String errp;
                `String "yearly repeat only implemented for current date"]
              )
          | (Secondly | Minutely | Hourly) as f -> 
              Lens.error [
                `String errp;
                `String "repeat_lens, get: this repetition type is not implemented: ";
                `String (ICalendar_print.print_freq (fun s -> s) f)]
  in
  let fr = match r.recur_interval with
           | None -> 1
           | Some n -> n
  in
  let endd = match r.recur_end with
             | RecNone -> None
             | RecUntil (DateVal d) -> Some d
             | RecUntil (DateTimeVal dt) -> Some dt.date
             | RecUntil _ -> Lens.error [
               `String errp;
               `String "repeat_lens, get: do not know how to deal with end date as period or duration"]
             | RecCount i -> (
               let ds = {
                 date = { year  = int_of_string y;
                          month = int_of_string m;
                          day   = int_of_string d };
                 time = { hour = 0; minute = 0; second = 0; zulu = false } } in
               let n = fr * (i - 1) in
               match r.recur_freq with
               | Daily -> Some (compute_end_date_and_time ~days:n ds).date
               | Weekly -> Some (compute_end_date_and_time ~weeks:n ds).date
               | Monthly -> Some (compute_end_date_and_time ~months:n ds).date
               | Yearly -> Some (compute_end_date_and_time ~years:n ds).date
               | (Secondly | Minutely | Hourly) as f -> Lens.error [
                 `String errp;
                 `String "repeat_lens, get: this repetition type is not implemented: ";
                 `String (ICalendar_print.print_freq (fun s -> s) f)]
             )
  in
  let ex =
    Safelist.fold_left
      (fun acc -> fun {dtplval = l} -> 
        acc@ (List.map
                (fun e ->
                 match e with
                 | DateTimeVal dt -> dt.date
                 | DateVal d -> d
                 | _ -> Lens.error [
                   `String errp;
                   `String "repeat_lens, get: do not know how to deal with exception date as period or duration"]
                )
                l
        )
      )
      []
      dtpl
  in
  { rtype = rt;
    rfreq = fr;
    renddate = endd;
    rexceptions = ex;
  }                            
  
let usual_exdate_create_func _ l =
  [{dtplparam = { (noparam ()) with valuetypeparam = Some Date} ; 
    dtplval = List.map (fun d -> DateVal d) l}]

let iCal_exdate_create_func t =
  match t with
  | None -> 
      List.map (fun d ->
        {dtplparam = { (noparam ()) with valuetypeparam = Some Date  } ;
           dtplval = [ DateVal d ]
        }
      )
  | Some t ->
      let h,m = time_parse (V.get_value (V.get_required t "begin")) in
      let time = {hour = (int_of_string h); 
                  minute = (int_of_string m); 
                  second = 0; 
                  zulu = false} in
      List.map (fun d ->
        {dtplparam = noparam () ;
           dtplval = [ DateTimeVal {date = d; time = time} ]
        }
      )
  
let recur_from_appt_repeat a errp exdate_create =
  let f, byday =
    match a.rtype with
    | RDaily -> Daily, []
    | RWeekly [] -> Lens.error [
      `String errp;
      `String "weekly appt should have at least one week day"]
    | RWeekly l -> Weekly, (List.map (fun e -> (None, e)) l)
    | RMonthlyByDay (i,w) -> Monthly, [Some i, w]
    | RMonthlyByDate -> Monthly, []
    | RYearly -> Yearly, []
  in
  let res = new_recur f in
  res.recur_byday <- byday;
  res.recur_end <-
    (match a.renddate with
     | None -> RecNone
     | Some d -> RecUntil (DateVal d));
  res.recur_interval <- Some (a.rfreq);
  let dtpl =
    match a.rexceptions with
    | [] -> []
    | l -> exdate_create l
  in
  res, dtpl
  
(* assumes a view with at most children "rrule", "exrule", "rdate", and "exdate"
 * fails if "exrule" or "rdate" is set
 * *)

let repeat_lens exdate_create =
  let get dir vf =
      let date = V.get_value (V.get_required vf "date") in
      let app_date = date_parse date in
      let d = V.dom vf in
      if (Name.Set.mem "exrule" d) || (Name.Set.mem "rdate" d) then
        Lens.error [
          `String "repeat_lens(";
          `String dir;
          `String "): we do not know how to deal with exrule or rdate in: ";
          `View vf];
      match (V.get vf "rrule") with
      | None ->  vf
      | Some rr -> 
          let rrv =
            match V.list_from_structure rr with
            | [elt] -> recur_from_view (V.get_required elt "val")
            | _ -> Lens.error [
              `String "repeat_lens(";
              `String dir;
              `String "): should have exactly one repeating rule: ";
              `View vf]
          in
          let dtpl = 
            match V.get vf "exdate" with
            | None -> []
            | Some v' -> Safelist.map dtpl_from_view (V.list_from_structure v')
          in
          let appt_repeat = appt_repeat_from_recur rrv dtpl app_date ("repeat_lens("^dir^"): ") in
          V.from_desc (V.V ( 
            (match V.get vf "time" with
            | None -> []
            | Some t -> ["time", V.In t]) @
            ["date", V.Val date;
             "repeat", V.In (view_from_appt_repeat appt_repeat ("repeat_lens("^dir^"): "))]))
  in
  let create dir v' =
    match V.get v' "repeat" with
    | None -> v'
    | Some rep ->
        let d = ["date", V.In  (V.get_required v' "date")] in
        let t = V.get v' "time" in
        let td =
          match t with
          | None -> d
          | Some ti -> ("time", V.In ti) :: d
        in
        let r, ex = 
          (recur_from_appt_repeat (appt_repeat_from_view rep) 
                                  ("repeat_lens("^dir^")")
                                  (exdate_create t))
        in
        let extd = match ex with
        | [] -> td
        | l -> ("exdate", V.In (V.structure_from_list (Safelist.map view_from_dtpl l))) :: td
        in
        V.from_desc (V.V ((
          "rrule", V.In (
            V.structure_from_list [
              V.from_desc (V.V ["val", V.In (view_from_recur r)])]))
        :: extd))
  in
    Lens.native
      (get "get")
      (fun vf' vfo -> 
         match vfo with
           None -> create "create" vf'
         | Some vf -> 
             let a = get "get for put" vf in
             if V.equal a vf' then vf else
               create "put" vf')
  
let alarm_lens =
  let get_dur dir v =
    match V.get_value (V.get_required (V.get_required v "action") "val") with
    | "DISPLAY" ->
        (match dtpval_from_view (V.get_required (V.get_required v "trigger") "val") with
        | DurationVal { dur_neg = true; dur_length = DurTime { dur_hour = h;
                                                               dur_minute = m;
                                                               dur_second = s; }} ->
            (match h,m,s with
            | n,0,0 -> "Hours", n
            | 0,n,0 -> "Minutes",n
            | _ -> V.error_msg [
              `String "alarm_lens(";
              `String dir;
              `String"): Can only deal with alarms containing days, hours, or minutes:";
              `View v])
        | DurationVal { dur_neg = true; dur_length = DurDate (d, None) } ->
            "Days", d
        | DurationVal { dur_neg = false; dur_length = DurTime { dur_hour = 0;
                                                               dur_minute = 0;
                                                               dur_second = 0; }} ->
            "Minutes", 0
        | _ -> V.error_msg [
          `String "alarm_lens(";
          `String dir;
          `String "): Can only deal with alarms containing days, hours, or minutes:";
          `View v])
    | _ -> V.error_msg [
      `String "alarm_lens(";
      `String dir;
      `String"): Can only deal with DISPLAY alarms";
      `View v]
  in
  let get v =
    let units, len = get_dur "get" v in
    V.from_desc (V.V [ "units", V.Val units; "notice", V.Val (string_of_int len) ])
  in
  let put vf vf'=
      let a = comp_prop_from_view vf in
      let new_units = V.get_value (V.get_required vf' "units") in
      let new_notice = int_of_string (V.get_value (V.get_required vf' "notice")) in
      (match a.comp_action with
      | Some (_, ActDisplay) ->
          (match a.comp_trigger with
          | Some t ->
              (match t.triggerval with
              | DurationVal d ->
                  d.dur_neg <- true;
                  (match new_units with
                  | "Hours" ->
                      d.dur_length <- DurTime {dur_hour = new_notice; dur_minute = 0; dur_second = 0;}
                  | "Minutes" ->
                      d.dur_length <- DurTime {dur_hour = 0; dur_minute = new_notice; dur_second = 0;}
                  | "Days" ->
                      d.dur_length <- DurDate (new_notice, None)
                  |  _ -> assert false)
              | _ -> V.error_msg [
                `String "alarm_lens(put): expected a duration in concrete view:";
                `View vf])
          | None -> V.error_msg [
            `String "alarm_lens(put): expected a trigger in concrete view:";
            `View vf])
      | _ -> V.error_msg [
        `String "alarm_lens(put): only know how to update DISPLAY alarms";
        `View vf]);
    view_from_comp_prop a
  in
  let create v' =
    let a = no_comp_prop () in
    a.comp_action <- Some ([], ActDisplay);
    a.comp_trigger <- Some {
      triggerparam = noparam (); 
      triggerval = DurationVal { 
        dur_neg = true;
        dur_length = DurDate (1, None); }
      };
    a.comp_description <- Some {
      descparam = noparam ();
      desctext = "Alarm created by Harmony synchronization"};
    put (view_from_comp_prop a) v'
  in
    Lens.native
      get
      (fun a co -> match co with None -> create a | Some c -> put c a)
  
(* we only deal with the first alarm *)
let alarms_lens = compose [hd V.empty_list; alarm_lens]
  
let text_rename_lens =
  let esc s =
    Pcre.qreplace ~pat:";" ~templ:"\\;" (
      Pcre.qreplace ~pat:"," ~templ:"\\," (
        Pcre.qreplace ~pat:"û" ~templ:"\\\\^u" (
          Pcre.qreplace ~pat:"\n" ~templ:"\\n" (
            Pcre.qreplace ~pat:"é" ~templ:"\195\169" (
            Pcre.qreplace ~pat:"à" ~templ:"\195\160" (
            Pcre.qreplace ~pat:"â" ~templ:"\195\162" (
            Pcre.qreplace ~pat:"ê" ~templ:"\195\170" (
            Pcre.qreplace ~pat:"è" ~templ:"\195\168" (
            Pcre.qreplace ~pat:"î" ~templ:"\195\174" (
            Pcre.qreplace ~pat:"ï" ~templ:"\195\175" (
            Pcre.qreplace ~pat:"ë" ~templ:"\195\171" (
          s))))))))))))
  in
  let unesc s =
            Pcre.qreplace ~pat:"\195\171" ~templ:"ë" (
            Pcre.qreplace ~pat:"\195\175" ~templ:"ï" (
            Pcre.qreplace ~pat:"\195\174" ~templ:"î" (
            Pcre.qreplace ~pat:"\195\168" ~templ:"è" (
            Pcre.qreplace ~pat:"\195\170" ~templ:"ê" (
            Pcre.qreplace ~pat:"\195\162" ~templ:"â" (
            Pcre.qreplace ~pat:"\195\160" ~templ:"à" (
            Pcre.qreplace ~pat:"\195\169" ~templ:"é" (
    Pcre.qreplace ~pat:"\\\\n" ~templ:"\n" (
      Pcre.qreplace ~pat:"\\\\\\\\\\^u" ~templ:"û" (
        Pcre.qreplace ~pat:"\\\\;" ~templ:";" (
          Pcre.qreplace ~pat:"\\\\," ~templ:"," s)))))))))))
  in
    Lens.native
      (fun v -> V.new_value (unesc (V.get_value v)))
      (fun v' _ -> V.new_value (esc (V.get_value v')))

let xharm = "X-HARMONY-ID"
  
let has_xharm_in_xprop v =
  match V.get v "xprop" with
  | None -> false
  | Some v' ->
      let l = V.list_from_structure v' in
      Safelist.exists (fun v -> V.get_field_value v "name" = xharm) l

      (*
          Lens.list_filter (fun v -> V.get_field_value v "type" = "Eventc");
          Lens.focus "val" (V.from_desc (V.V ["type", V.Val "Eventc"])) ;
      *)
let icalendar_lens exdate_create=
    tracepoint "icalendar_lens" [
    (* I assume only one Calendar component *)
    compose [
      hd V.empty_list;
      focus "components" (V.from_desc ( V.V [ "calprops", 
                                                     V.V ["prodid", 
                                                            V.V ["val", V.Val "Harmony"];
                                                          "version",
                                                          V.V ["val", V.Val "2.0"]]]));
      tracepoint "after focus components" [
        map_list
          (compose [
            tracepoint "in a component" [
              tracepoint "end at dup harm_uid to uid" [ 
              tracepoint "end at equi_merge harm_uid uid" [ 
              acond
                (fun c -> 
		   match V.get_field_value_option c "type" with 
		       Some "Timezonec" -> true | _ -> false)
                (fun a -> 
		   match V.get_field_value_option a "type" with 
		       Some "Timezonec" -> true | _ -> false)
                (* for timezones we use the tzid as uid *)
                (mapp (Prd.s "val") (old_rename "tzid" "uid"))
                (mapp (Prd.s "val") (
                  compose [
                    mapp (Prd.s "props") (
                      compose [
                        tracepoint "xprop 1" [
                          acond 
                            has_xharm_in_xprop 
                            (fun a -> V.get a "harm_uid" <> None)

                            ( tracepoint "in xprop acond true" [
                              xfork
                              (Prd.s "xprop")
                              (Prd.m ["xprop"; "harm_uid"; "harm_props"])
                              (compose [
                                tracepoint "end promotes harm_props" [
                                tracepoint "end promotes harm_uid" [
                                mapp (Prd.s "xprop") (
                                  compose [
                                    tracepoint "extract" [
                                    extract (fun v -> V.get_field_value v "name" = xharm);
                                    ];
                                    tracepoint "xfork xprop" [
                                    list_fork (Prd.m ["harm_uid"; "harm_props"])
                                      (compose [
                                        prune "name" (V.from_desc (V.Val xharm));
                                        xfork (Prd.s "val") (Prd.s "harm_uid")
                                          (old_rename "val" "harm_uid")
                                          (plunge "harm_props")
                                      ]) 
                                    id
                                    ]
                                  ]);
                                promote "xprop" "harm_uid";
                                ];
                                promote "xprop" "harm_props"
                                ]
                              ])
                              id])
                            id;
                        ];
                      ]
                    );
                    promote "props" "uid";
                    promote "props" "harm_uid";
                  ]));
		tracepoint "pinpoint1" [ promote "val" "uid";
		  tracepoint "pinpoint2" [mapp (Prd.s "uid") (old_rename "xprop" "uid_param");
		  tracepoint "pinpoint3" [promote "uid" "uid_param";
		  tracepoint "pinpoint4" [promote "uid" "global";
		  (* this is allowed since now only xprops allowed under a uid *)
		  tracepoint "pinpoint5" [mapp (Prd.s "uid") (focus "val" V.empty);
		  tracepoint "pinpoint6" [promote "val" "harm_uid"]
	      ]]]]]];
              (* TODO: Reimplement using an exact fork *)
              acond
                (fun c -> V.get c "harm_uid" <> None)
                (fun a -> V.get a "orig_uid" <> None)
                (old_rename "uid" "orig_uid")
                (old_rename "uid" "harm_uid")
              ];
              pivot "harm_uid";
            ]
          ]);
        flatten_unique;
        (* need to remove stuff that is not an Eventc *)
        zfork
            (fun a _ -> V.empty, a)
            (fun a1 a2 -> a2)
            (fun c -> 
              let p v = V.get_field_value v "type" = "Timezonec" in
              let binds1,binds2 =
                V.fold
                (fun k kv (v1acc,v2acc) ->
                  if p kv then
                    ((k,Some kv)::v1acc,v2acc)
                  else
                    (v1acc, (k,Some kv)::v2acc))
                c ([],[]) 
              in
              (V.create_star binds1), (V.create_star binds2))
            (fun c1 c2 -> V.concat c1 c2)
            (* we throw away the Timezonec *)
            (const V.empty V.empty)
            id;
          wmap ~log:true ( fun uid_val ->
            tracepoint "under a uid" [
              (* we remove orig_uid, creating it from the current uid_val in
               * case of creation *)
              prune "orig_uid" (V.new_value uid_val);
              focus "val" (V.from_desc (V.V ["type", V.Val "Eventc"]));
              fork (Prd.s "alarms")
                id
                (hoist "props");
                tracepoint "filter" [
                      filter (Prd.m ["summary"; "dtend"; "dtstart"; "duration"; 
                                          "rrule"; "rdate"; "exrule"; "exdate"; "alarms"])
                        (V.from_list ["dtstamp", (view_from_dt 
                                                   { dtval = DateTimeVal (now ());
                                                     dtparam = noparam () });
                                      "harm_props", V.empty_list;
                                      (* must create a xprop, that will contain
                                       * the harm_uid *) 
                                      "xprop", V.empty_list ]);
                      ];
                      xfork (Prd.s "dtstart") (Prd.m ["date_start"; "time_start"])
                        (compose [
                           hoist "dtstart";
                           focus "val" V.empty;
                           tracepoint "exfork_start" [
                             exfork (V.from_list ["DateVal", V.empty])
				    (V.from_list ["date_start", V.empty])
				       (old_rename "DateVal" "date_start")
                                       (compose [
                                          hoist "DateTimeVal";
                                          old_rename "date" "date_start"; 
					  old_rename "time" "time_start"
					]
                                       )
                           ]
                        ]
                      )
                        (id);
                      tracepoint "before dtend fork" [
                      xfork (Prd.s "dtend") (Prd.m ["date_end"; "time_end"])
                        (efork 
                        (compose [
                           hoist "dtend";
                           focus "val" V.empty;
                           tracepoint "exfork_end" [
                           exfork (V.from_list ["DateVal",V.empty])
                                       (V.from_list ["date_end",V.empty])
                                       (old_rename "DateVal" "date_end")
                                       (compose [
                                          hoist "DateTimeVal";
                                          old_rename "date" "date_end"; 
					  old_rename "time" "time_end"
                                       ]
                                       )
                           ]
                        ]
                      )
                      )
                        (id);
                      ];
                      xfork
                        (Prd.m ["date_start"; "time_start"; "date_end"; "time_end"; "duration"])
                        (Prd.m ["date_start"; "time_start"; "date_end"; "time_end"])
                        (compose [
                          tracepoint "first equi" [
                          (equi_merge 
                            (fun v -> Name.Set.equal (V.dom v) 
                                      (Name.Set.add "date_start" 
                                      (Name.Set.add "time_start" 
                                      (Name.Set.add "duration" Name.Set.empty))))
                            (fun v -> 
                              let d = duration_from_view (V.get_required
                                                           (V.get_required v "duration")
                                                          "val") in
                              if d.dur_neg then Lens.error [
                                `String "icalendar_lens(get) error: duration in event negative";
                                `View v];
                              match d.dur_length with
                              | DurTime { dur_hour = dh; dur_minute = dm; dur_second = ds } ->
                                  let t = time_from_view (V.get_required v "time_start") in
                                  let dummy_date =
                                    { year = 1970; month = 1; day = 1 } in
                                  let d = { 
                                    date = dummy_date;
                                    time = t } in
                                  let te = compute_end_date_and_time ~hours:dh 
                                                                     ~minutes:dm
                                                                     ~seconds:ds
                                                                     d
                                  in
                                  if te.date <> dummy_date then Lens.error [
                                    `String "icalendar_lens(get) error: event spanning accross days unsupported";
                                    `View v];
                                  let time_end = view_from_time te.time in
                                  V.set (V.set v "time_end" (Some time_end)) "duration" None
                              | _ -> Lens.error [
                                `String "icalendar_lens(get) error: duration must be in hour, minutes, and seconds";
                                `View v]));
                          ];
                          (equi_merge 
                            (fun v -> Name.Set.equal (V.dom v) 
                                      (Name.Set.add "date_start" 
                                      (Name.Set.add "duration" Name.Set.empty)))
                            (fun v -> 
                              let d = duration_from_view (V.get_required
                                                           (V.get_required v "duration")
                                                          "val") in
                              if d.dur_neg then Lens.error [
                                `String "icalendar_lens(get) error: duration in event negative";
                                `View v];
                              match d.dur_length with
                              | DurDate (days, None) ->
                                  let d = date_from_view (V.get_required v "date_start") in
                                  let de = compute_end_date_and_time ~days:days
                                      { date = d;
                                        time = {hour = 0; minute = 0; second = 0; zulu = false} 
                                      } in
                                  let date_end = view_from_date de.date in
                                  V.set (V.set v "date_end" (Some date_end)) "duration" None
                              | _ -> Lens.error [
                                `String "icalendar_lens(get) error: duration must be days only";
                                `View v]));
                        ])
                        id
                        ; 
                      mapp (Prd.s "time_start")
                        (compose [
                          filter (Prd.m ["hour"; "minute"])
                                      (V.from_desc (V.V [ "second", V.Val "00";
                                                          "zulu", V.Val "#false" ]));
                          time_str_lens
                        ]);
                      mapp (Prd.s "time_end")
                        (compose [
                          filter (Prd.m ["hour"; "minute"])
                                      (V.from_desc (V.V [ "second", V.Val "00";
                                                          "zulu", V.Val "#false" ]));
                          time_str_lens
                        ]);
                      xfork (Prd.m ["time_end"; "time_start"]) (Prd.s "time")
                        (efork (plunge "time"))
                        id;
                      mapp (Prd.s "time") (
                        compose [old_rename "time_end" "end"; 
				 old_rename "time_start" "begin"]);
                      xfork (Prd.m ["date_start"; "date_end"; "time"]) (Prd.m ["date"; "time"])
                        (acond
                          (fun c ->
                            match V.get c "time" with
                            | Some t -> (match V.get t "begin",V.get t "end" with
                                        | None, None -> true
                                        | Some v1, Some v2 ->
                                            (V.get_value v1 = "00:00") &&
                                            (V.get_value v2 = "00:00")
                                        | _ -> false)
                            | None -> true)
                          (fun a ->
                            match V.get a "time" with
                              | Some _ -> false
                              | None -> true)
                         (compose [
                           xfork (Prd.m ["date_start"; "date_end"]) (Prd.s "date_start")
                             (fix_dates
                               (fun x ->
                                 let ds = date_from_view x in
                                 let de = compute_end_date_and_time ~days:1
                                   { date = ds;
                                     time = { hour = 0; minute = 0; second = 0; zulu = false}
                                   } in
                                 view_from_date de.date))
                             id;
                           old_rename "date_start" "date";
                           filter (Prd.s "date") V.empty;
                         ])
                         (compose [
                           xfork (Prd.m ["date_start"; "date_end"]) (Prd.s "date_start")
                             (fix_dates (fun x -> x))
                             id;
                           (old_rename "date_start" "date");
                         ])
                        )
                        (id);
                      (mapp (Prd.s "date") date_str_lens);
                      xfork (Prd.m ["rrule"; "exrule"; "rdate"; "exdate"; "date"; "time"]) 
                                 (Prd.m ["repeat"; "date"; "time"])
                        (repeat_lens exdate_create)
                        id;
                      mapp (Prd.s "repeat") (
                        mapp (Prd.s "type") (
                          tracepoint "weekly atomic flatten" [
                          mapp (Prd.s "weekly") flatten_atomic_list ]
                        ));
                      mapp (Prd.s "summary") (
                        compose [
                          focus "val" V.empty;
                          text_rename_lens]);
                      old_rename "summary" "desc";
                      mapp (Prd.s "alarms") alarms_lens;
                      old_rename "alarms" "alarm"
                      ]
                    )
         ]
      ];
  ]

let reader eol inname =
  try
    let inc = open_in inname in
    Some (view_from_icalendar (ICalendar.read (fun () -> input_char inc)))
  with 
  | Sys_error s -> 
      print_endline ("Warning: unable to read from " ^ inname);
      None

let writer eol vo outname = 
  match vo with
  | None -> raise
      (Misc.Unimplemented "iCalendar.writer: writing empty view not supported")
  | Some v -> ICalendar.write (open_out outname) eol (icalendar_from_view v)


let chars_from_str s =
  let len = String.length s in
  let cur_pos = ref 0 in
  let get_char () =
    let pos = !cur_pos in
    if pos = len then raise End_of_file;
    incr cur_pos;
    s.[pos]
  in
  get_char

let count = ref 0
  
let ugen_factory _ =
  let st = ICalendar.tostring ICalendar_print.print_date_time (now ()) in
  let pid = Unix.getpid () in
  let hn = Unix.gethostname () in
  (fun () ->
    let res = Printf.sprintf "%s-%d-%d@%s" st pid (!count) hn in
    incr count;
    res)

let encoding =
  let etest filename copt = Misc.filename_extension filename = "ics" in
  {
  Surveyor.description = "iCalendar file format";
  Surveyor.encoding_test = etest;
  Surveyor.reader = reader false;
  Surveyor.writer = writer false;
  Surveyor.from_string = (fun s ->
			    Some (view_from_icalendar (ICalendar.read (chars_from_str s))));
  Surveyor.to_string = (fun ic -> ICalendar.tostring ICalendar_print.print_icalendar 
                          (icalendar_from_view ic));
  Surveyor.base_type = ["iCalendar_conc"];
  }

let _ = Surveyor.register_encoding "iCalendar" encoding

let bogus = Types.string2abstract_type ""

let _ = Optometrist.register_lens ["iCalendar_conc"] ["appointments"] 
	  Schemas.appointments
          (icalendar_lens usual_exdate_create_func)
  
let _ = Key_factory.register_keyfactory ugen_factory ("iCalendar", ["appointments"])

let conc_ical_schema = Types.string2abstract_type "" (* BOGUS *)
let encoding_ical =
  let etest filename copt = false in
  {
  Surveyor.description = "iCalendar file format (iCal version)";
  Surveyor.encoding_test = etest;
  Surveyor.reader = reader false;
  Surveyor.writer = writer false;
  Surveyor.from_string = (fun s ->
			    Some (view_from_icalendar (ICalendar.read (chars_from_str s))));
  Surveyor.to_string = (fun ic -> ICalendar.tostring ICalendar_print.print_icalendar 
                          (icalendar_from_view ic));
  Surveyor.base_type = ["iCalendar_conc_iCal"];
  }
  
let _ = Surveyor.register_encoding "iCal" encoding_ical
	  
let _ = Optometrist.register_lens ["iCalendar_conc_iCal"] ["appointments"] 
	  Schemas.appointments
          (icalendar_lens iCal_exdate_create_func)
	  
let _ = Key_factory.register_keyfactory ugen_factory ("iCal", ["appointments"])
	  
(* Design notes:
 * DTEND is completely ignored
 *)
