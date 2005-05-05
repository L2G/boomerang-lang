open Pervasives_plugin
open Experimental_plugin

(* --------------------------------------------------------------------- *)
(* generic reader/writer stuff *)

type sink = Buffer.t
type source = {buf:string; mutable pos:int}

type rdwr = {rd: source->V.t; wr: sink->V.t->unit }

let badrd src s =
  raise (Misc.Bad (Printf.sprintf "%s from \"%s\" at position %d" s 
		    (String.escaped src.buf) src.pos))

let debug = Trace.debug "palm"

let debug_rw realrw =
  let rd src =
    let beg = src.pos in
    let res = realrw.rd src in
    debug (fun() -> Util.msg "read from %d to %d: " beg (src.pos-1);
                    V.format res;
                    Format.print_flush();
                    Util.msg "\n");
    res
  in let wr snk v =
    realrw.wr snk v
  in {rd=rd;wr=wr}

let record_rw (fieldlist: (Name.t * rdwr) list) =
  let rd src = 
    let fields = Safelist.map (fun (k,rw) -> (k, rw.rd src)) fieldlist in
    let realfields = Safelist.filter (fun (k,_) -> k<>"") fields in
    V.from_list realfields in
  let wr snk v =
    Safelist.iter
      (fun (k,rw) ->
         if k="" then rw.wr snk V.empty
         else rw.wr snk (V.get_required v k))
      fieldlist in
  {rd=rd;wr=wr}

(* ------------------------------------------------------------------ *)
(* low-level string parsing stuff *)

let getbyte src =
  if src.pos >= String.length src.buf then badrd src "Read past end";
  let r = int_of_char src.buf.[src.pos] in
  src.pos <- src.pos+1;
  r

let putbyte snk b = 
  assert ((b land 0x7FFFFF00) = 0);
  Buffer.add_char snk (char_of_int b)

let getshort src =
  let lo = getbyte src in
  let hi = getbyte src in
  (hi lsl 8) lor lo

let putshort snk i =
  if i land 0x7FFF0000 <> 0 then Misc.bad "Tried to output short > 2^16";
  putbyte snk (i land 0xFF);
  putbyte snk (i lsr 8)  

let getcard24 src =
  let b3 = getbyte src in
  let b2 = getbyte src in
  let b1 = getbyte src in
  (b1 lsl 16) lor (b2 lsl 8) lor b3

let putcard24 snk i =
  if i land 0x7F000000 <> 0 then Misc.bad "Tried to output short > 2^24";
  putbyte snk (i land 0xFF);
  putbyte snk ((i lsr 8) land 0xFF);
  putbyte snk (i lsr 16)  

let putcard24BE snk i =
  if i land 0x7F000000 <> 0 then Misc.bad "Tried to output short > 2^24";
  putbyte snk (i lsr 16);
  putbyte snk ((i lsr 8) land 0xFF);
  putbyte snk (i land 0xFF)

let getint src =
  let b4 = getbyte src in
  let b3 = getbyte src in
  let b2 = getbyte src in
  let b1 = getbyte src in
  if b1 land 0x80 = 0x80 then badrd src "can't represent 32-bit ints";
  ((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4)

let putint snk i =
  putbyte snk (i          land 0xFF);
  putbyte snk ((i lsr 8)  land 0xFF);
  putbyte snk ((i lsr 16) land 0xFF);
  putbyte snk ((i lsr 24) land 0xFF)

let getshortBE src =
  let hi = getbyte src in
  let lo = getbyte src in
  (hi lsl 8) lor lo

let putshortBE snk i =
  if i land 0x7FFF0000 <> 0 then Misc.bad "Tried to output short > 2^16";
  putbyte snk (i lsr 8);
  putbyte snk (i land 0xFF)

let getcard24BE src =
  let b1 = getbyte src in
  let b2 = getbyte src in
  let b3 = getbyte src in
  (b1 lsl 16) lor (b2 lsl 8) lor b3

let getintBE src =
  let b1 = getbyte src in
  let b2 = getbyte src in
  let b3 = getbyte src in
  let b4 = getbyte src in
  if b1 land 0x80 = 0x80 then badrd src "can't represent 32-bit ints";
  let res = ((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4) in
  res

let putintBE snk i =
  putbyte snk ((i lsr 24) land 0xFF);
  putbyte snk ((i lsr 16) land 0xFF);
  putbyte snk ((i lsr 8)  land 0xFF);
  putbyte snk (i          land 0xFF)

let peek f src =
  let pos = src.pos in
  let res = f src in
  src.pos <- pos;
  res


(* var-length string RW; length given in the header *)
let cstring_rw = 
  let rd src = 
    let len = let b = getbyte src in if b = 0xFF then getshort src else b in
    if src.pos + len >= String.length src.buf then
      badrd src "Ill-formed cstring (not enough chars left in buffer)"
    else
      let res = String.sub src.buf src.pos len in
      src.pos <- src.pos + len;
      V.new_value res 
  in let wr snk v = 
    let s = V.get_value v in
    if String.length s > 0x8FFF then Misc.bad "Can't write: Cstring too big";
    if String.length s < 255 then putbyte snk (String.length s)
    else (putbyte snk 0xFF; putshort snk (String.length s));
    Buffer.add_string snk s 
  in {rd=rd;wr=wr}

let fixed_length_string_rw len = 
  let rd src = 
    if src.pos + len >= String.length src.buf then
      badrd src "Bad fixed length string (not enough chars left in buffer)"
    else
      let reallen =
        try (Misc.bounded_index_from src.buf src.pos len '\000') - src.pos
        with Not_found -> len in
      let res = String.sub src.buf src.pos reallen in
      src.pos <- src.pos + len;
      V.new_value res 
  in let wr snk v = 
    let s = V.get_value v in
    if String.length s > len then Misc.bad "Can't write: string too big";
    Buffer.add_string snk s;
    Buffer.add_string snk (String.make (len - String.length s) '\000')
  in {rd=rd;wr=wr}

let get_var_length_string src = 
  let len = String.length src.buf - src.pos in
  let reallen =
    try (Misc.bounded_index_from src.buf src.pos len '\000') - src.pos
    with Not_found -> len in
  let res = String.sub src.buf src.pos reallen in
  src.pos <- src.pos + reallen;
  let _ = getbyte src in
  res

(* var length string RW, null terminated *)
let var_length_string_rw maxlen = 
  let rd src = 
    let res = get_var_length_string src in
    V.new_value res 
  in let wr snk v = 
    let s = V.get_value v in
    if String.length s + 1 > maxlen then
      Misc.bad ("Can't write string longer than "^string_of_int maxlen);
    Buffer.add_string snk s;
    Buffer.add_char snk '\000'
  in {rd=rd;wr=wr}

let value_rw get put =
  let rd src = 
    V.new_value (get src)
  in let wr snk v = 
    let i = V.get_value v in
    put snk i
  in {rd=rd;wr=wr}

let byte_rw = 
  value_rw 
    (fun src -> string_of_int (getbyte src))
    (fun snk v -> putbyte snk (int_of_string v))

let short_rw =
  value_rw
    (fun src -> string_of_int (getshort src))
    (fun snk v -> putshort snk (int_of_string v))

let shortBE_rw =
  value_rw
    (fun src -> string_of_int (getshortBE src))
    (fun snk v -> putshortBE snk (int_of_string v))


let int_rw =
  value_rw
    (fun src -> string_of_int (getint src))
    (fun snk v -> putint snk (int_of_string v))

let intBE_rw =
  value_rw
    (fun src -> string_of_int (getintBE src))
    (fun snk v -> putintBE snk (int_of_string v))

let bool_rw =
  value_rw
    (fun src -> if (getint src) = 0 then "false" else "true")
    (fun snk v -> putint snk (if v = "false" then 0 else 1))

let literal_byte_rw b =
  let rd src =
    let b' = getbyte src in
    if b'=b then V.empty
    else badrd src (Printf.sprintf
           "Bad input file: expected literal %d but found %d" b b')
  in let wr snk v =
    putbyte snk b
  in {rd=rd;wr=wr}

let literal_int_rw b =
  let rd src =
    let b' = getint src in
    if b'=b then V.empty
    else badrd src (Printf.sprintf
           "Bad input file: expected literal %d but found %d" b b')
  in let wr snk v =
    putint snk b
  in {rd=rd;wr=wr}

let literal_byte_prefix_rw b rw =
  let rd src =
    let b' = getbyte src in
    if b'<>b then
      badrd src (Printf.sprintf
           "Bad input file: expected literal %d but found %d" b b')
    else rw.rd src
  in let wr snk v =
    putbyte snk b;
    rw.wr snk v
  in {rd=rd;wr=wr}

(* Palm DBs have this funny convention where the count in the file is
   the total number of *bytes*, so you have to divide what you see by
   some known [factor] to get the number of records to read *)
(*  
let generic_list_rw getcount putcount itemrw factor =
  let rd src =
    let len = getcount src in
    let count = len / factor in
    let rec loop n acc =
      if n=count then Safelist.rev acc
      else loop (n+1) (("", itemrw.rd src) :: acc) in
    let items = loop 0 [] in
    V.from_list items

  in let wr snk v =
    putcount snk ((Safelist.length (V.dom v)) * factor);
    Safelist.iter (fun ch -> itemrw.wr snk ch) (V.dom v)
  in {rd=rd;wr=wr}

let list_rw itemrw factor =
  generic_list_rw getint putint itemrw factor *)

open Unix

type date = float

let palm_epoch =  (* Jan 1, 1904 0:00:00 GMT *)
  { tm_sec = 0;
    tm_min = 0;
    tm_hour = 0;
    tm_mday = 1;
    tm_mon = 0;
    tm_year = (*19*)04;
    tm_wday = 0;  (* don't know, don't care *)
    tm_yday = 0;
    tm_isdst = false }

let unix_epoch =  (* Jan 1, 1970 0:00:00 GMT *)
  { tm_sec = 0;
    tm_min = 0;
    tm_hour = 0;
    tm_mday = 1;
    tm_mon = 0;
    tm_year = (*19*)70;
    tm_wday = 0;  (* don't know, don't care *)
    tm_yday = 0;
    tm_isdst = false }

(* let delta = fst (mktime unix_epoch) -. fst (mktime palm_epoch) *)

(* let date_rw =
  let tag = "date" in
  let rd src = 
    let hi = getshortBE src in
    let lo = getshortBE src in
    let time = float hi *. (2.**16.) +. float lo in
    make_simple_tree tag (string_of_float (time -. delta)) 
  in let wr snk v =
    let f = use_simple_tree tag v in
    let time = (float_of_string f) +. delta in
    let lo = int_of_float (time /. 2.**16.) in
    let hi = int_of_float (time -. ((float lo) *. 2.**16.)) in
    putshortBE snk hi;
    putshortBE snk lo
  in {rd=rd;wr=wr} *)

(* The previous date parsing code doesn't seem to work; needs more thought.
   (It's based on ECC's, but I'm not confident that his works either.)
   Let's do something simpler (and more obviously broken!) for now. *)
let date_rw =
  let tag = "date" in
  let rd src = 
    let hi = getshortBE src in
    let lo = getshortBE src in
    V.from_list 
      ["hi", V.new_value (string_of_int hi);
       "lo", V.new_value (string_of_int lo)]
  in let wr snk v =
    let hi = int_of_string (V.get_field_value v "hi") in
    let lo = int_of_string (V.get_field_value v "lo") in
    putshortBE snk hi;
    putshortBE snk lo
  in {rd=rd;wr=wr}

(* --------------------------------------------------------------------- *)
(* pdb-specific reader/writers *)

let pdb_header_rw = 
    record_rw
      ["filename",         (fixed_length_string_rw 32);
       "flags",            shortBE_rw;
       "version",          shortBE_rw;
       "create_date",      date_rw; 
       "backup_date",      date_rw; 
       "modify_date",      date_rw; 
       "mod_num",          intBE_rw; 
       "appinfo_pos",      intBE_rw; 
       "sortinfo_pos",     intBE_rw; 
       "db_type",          (fixed_length_string_rw 4); 
       "creator",          (fixed_length_string_rw 4); 
       "uid_seed",         intBE_rw; 
       "next_list",        intBE_rw; 
      ]

let header_size = 78

let gap_size = 2  (* There is "traditionally" a 2-byte gap between the header
                     and the appinfo block.  Dunno why. *)

type rcd_pointer = {rcd_pos:int; attrs:int; uid:int}

let rcd_pointer_size = 8

let pointer_rd src = 
  let p = getintBE src in
  let a = getbyte src in
  let u = getcard24BE src in
  {rcd_pos = p; attrs = a; uid = u} 

let pdb_rd src =
  let headerv = pdb_header_rw.rd src in
  let npointers = getshortBE src in
  assert (src.pos = header_size);
  let pointers = 
    Array.init npointers (* why is this an array and not a list? *)
      (fun i -> pointer_rd src) in
  let appinfo_pos = 
    int_of_string(V.get_field_value headerv "appinfo_pos") in
  let sortinfo_pos = 
    int_of_string(V.get_field_value headerv "sortinfo_pos") in
  let appinfo_len =
    if sortinfo_pos <> 0 then sortinfo_pos - appinfo_pos
    else if npointers = 0 then String.length src.buf - appinfo_pos
    else pointers.(0).rcd_pos - appinfo_pos in
  let appinfov =
    if appinfo_pos = 0 then (V.new_value "") else 
    V.new_value (String.sub src.buf appinfo_pos appinfo_len) in
  let sortinfov = 
    if sortinfo_pos <> 0 then Misc.bad "Non-empty sortinfo"
    else (V.new_value "") in
  let rec loop i acc =
    if i<0 then acc
    else begin
      let endpos = if i=npointers-1 then (String.length src.buf) 
                   else pointers.(i+1).rcd_pos in
      let len = endpos - pointers.(i).rcd_pos in
  
      if len < 0 || pointers.(i).rcd_pos + len > String.length src.buf then
        Misc.bad ("Short record " ^ (string_of_int i));
      let r = String.sub src.buf pointers.(i).rcd_pos len in
      let attr = 
	V.new_value (string_of_int (pointers.(i).attrs land 0xf0)) in
      let cat = 
	V.new_value (string_of_int (pointers.(i).attrs land 0x0f)) in
      let uid =
	V.new_value (string_of_int pointers.(i).uid) in
      let rv = V.set V.empty "rcd"
          (Some (V.from_list 
		   ["attrs",attr; "category",cat;
		    "uid",uid; "contents",V.new_value r])) in
      loop (i-1) (rv::acc)
    end in
  let children = loop (npointers-1) [] in
  let recordsv = V.structure_from_list children in
  V.from_list 
    ["header", headerv;
     "appinfo", appinfov;
     "sortinfo", sortinfov;
     "records", recordsv]

(* This is buggy: we need to specify the header, appinfo and startinfo according
 * to the type of the pdb file *)
let pdb_wr snk v = 
  let headerv = V.get_required v "header" in
  let appinfo = V.get_field_value v "appinfo" in
  let sortinfo = V.get_field_value v "sortinfo" in
  let records = Safelist.map (fun v -> V.get_required v "rcd")
      (V.list_from_structure (V.get_required v "records")) in
  let npointers = Safelist.length records in
  let appinfo_pos = npointers * rcd_pointer_size + header_size + gap_size in
  let hv' =
    V.set_field_value headerv "appinfo_pos" (string_of_int appinfo_pos) in
  pdb_header_rw.wr snk hv';
  putshortBE snk npointers;
  
  let rec loop pos = function
      [] -> ()
    | rv::rvs ->
	if (V.get rv "attrs") = None then
	  Lens.error [`String "null attrs: "; `View rv] else begin
	let uid = int_of_string (V.get_field_value rv "uid") in
	let attrs = int_of_string (V.get_field_value rv "attrs") in
	let category = int_of_string (V.get_field_value rv "category") in
	let s = V.get_field_value rv "contents" in
	putintBE snk pos;
	putbyte snk (attrs lor category);
	putcard24BE snk uid;
	loop (pos + (String.length s)) rvs 
	  end in

  let startpos = header_size + gap_size
      + npointers * rcd_pointer_size
      + String.length appinfo
      + String.length sortinfo in
  loop startpos records;
  Buffer.add_string snk (String.make gap_size '\000');
  Buffer.add_string snk appinfo;
  Buffer.add_string snk sortinfo;                             
  Safelist.iter
    (fun rv -> 
      let s = V.get_field_value rv "contents" in
      Buffer.add_string snk s)
    records

let pdb_rw = {rd=pdb_rd; wr=pdb_wr}

(* generic lens for turning a bitmap of flags into a view with names *)
let bitmap_lens flaglist =
  let get vf = 
      let attrs = int_of_string (V.get_value vf) in
      Safelist.fold_left (
        fun vacc (flag,flagname) ->
          if (flag land attrs) = 0 then vacc else
            V.set vacc flagname (Some V.empty)) V.empty flaglist
  in
  let put vf' _ = 
      let attrs =
        Safelist.fold_left (
          fun acc (flag,flagname) ->
            if (V.get vf' flagname) = None then acc else acc lor flag) 
        0x0 flaglist 
      in
      V.new_value (string_of_int attrs)
  in
    Lens.native get put

(* attribute flags on .pdb records *)
let deleted_flag  = 0x80
let dirty_flag    = 0x40
let busy_flag     = 0x20
let secret_flag   = 0x10
let archived_flag = 0x08

let rcd_flags =
  [(deleted_flag,"deleted"); (dirty_flag,"dirty"); (busy_flag,"busy");
   (secret_flag,"secret"); (archived_flag,"archived")]

let attrbitmap_lens = bitmap_lens rcd_flags

(* preferences for handling the attributes on records *)
(* Breaks thinkgs, unsupported anymore
let arch_deletes = Prefs.createBool "palm-archive-deletes" false
    "*For Palm PDB files, archive deleted records" ""
*)
    

(* If the original record was absent or has been modified, set the dirty flag *)
let update_lens =
  let set_dirty_flag v' =
    let attrv' = V.get_required v' "attrs" in
    V.set v' "attrs" (Some (V.set attrv' "dirty" (Some V.empty))) in
  let get v = v in
  let put vf' vfo =
    match vfo with
      None ->
        let v'' =
          if (V.get vf' "attrs") <> None then vf' else V.set vf' "attrs" (Some V.empty) 
        in
        set_dirty_flag v''
    | Some vf ->
        if (V.equal vf vf') then vf' else set_dirty_flag vf' in
  Lens.native get put

let delete_map_lens =
  let get vf =
      let binds = 
        V.fold (
          fun k kv vacc ->
            if (V.get (V.get_required kv "attrs") "deleted" = None) &&
               (V.get (V.get_required kv "attrs") "archived" = None) then
              (k,Some kv) :: vacc
            else
              vacc)
        vf [] 
      in
      V.create_star binds
  in
  let put vf' vfo =
      let vf = match vfo with None -> V.empty | Some vf -> vf in
      let set_attr r a =
        let attrv = V.get_required r "attrs" in
        V.set r "attrs" (Some (V.set attrv a (Some V.empty))) in
      let check v =
        match V.get v "attrs" with
        | Some v' -> (
          match V.get v' "deleted" with
          | Some _ -> V.error_msg [
            `String "this view should not have its deleted flag set";
            `View v ]
          | None -> v)
        | None -> v
      in
      let binds =
        Name.Set.fold (
          fun k acc ->
            match V.get vf k, V.get vf' k with
            | _, Some vk' -> (k, Some (check vk')) :: acc
            | Some vk, None ->
                (k, Some (set_attr vk "deleted")) :: acc
            | None, None -> assert false
        )
        (Name.Set.union (V.dom vf) (V.dom vf'))
        []
      in
      V.create_star binds
  in
    Lens.native get put

(* this assumes the view we get is an optional value (meaning: it is empty, 
   or is a value) *)
let rw_lens rw = 
  let create_pd v' =
    let outbuf = Buffer.create 20 in
    rw.wr outbuf v';
    V.new_value (Buffer.contents outbuf) in
  Lens.native 
    (fun vf ->
        if (V.is_empty vf) then vf else
          let buf = V.get_value vf in
          rw.rd { buf = buf; pos = 0 })
    (fun vf' vfo -> 
        match vfo with
          None -> create_pd vf'
        | Some vf -> 
          if (V.is_empty vf) then create_pd vf' else
            let buf = V.get_value vf in
            let outbuf = Buffer.create (String.length buf) in
            rw.wr outbuf vf';
            V.new_value (Buffer.contents outbuf))

let rec compose = function
  | [] -> id
  | [l] -> l
  | l :: r -> compose2 l (compose r)

let generic_pdb_lens =
  compose [
  Lens.tracepoint "Palm rw_lens" (
    rw_lens pdb_rw; 
  );
  mapp (Prd.s "records")
    (compose [
     list_map (hoist "rcd");
     index "uid";
     map (
       compose [
         mapp (Prd.s "attrs") attrbitmap_lens;
         Lens.tracepoint "Palm generic_pdb update" (
	   update_lens;
         );
       ]);
     Lens.tracepoint "Palm generic_pdb delete" (
     delete_map_lens;
     );
    ])
  ]

let generic_pdb_rcd_lens = 
  compose [
  generic_pdb_lens;
  focus "records" V.empty;
  map 
    (compose [
     (filter (Prd.neg (Prd.s "category")) 
	(V.set_field_value V.empty "category" "0"));
     (focus "contents" V.empty)
   ]);
]

(* uid generation: take the original concrete view, find what the maximum
   uid is, use that as the seed for newly generated uids. note that there is
   a possibility for overflow here that we currently don't address. note
   that we build up a custom lens to get at the uid's; we would have used
   the generic_palm_* lenses, but they hide deleted records, which we need
   still need to avoid colliding with... *)
let ugen_factory vopt =
  let maxuid =
    match vopt with
      Some v ->
	let l = compose [
	  rw_lens pdb_rw;
	  focus "records" V.empty;
	  list_map
	    (compose [
	     focus "rcd" V.empty;
	     filter (Prd.s "uid") V.empty;
	   ]);
	] in
	let v' = Lens.get l v in
	let uidstrs = Safelist.map
	    (fun v -> int_of_string (V.get_field_value v "uid"))
	    (V.list_from_structure v') in
	Safelist.fold_left max 0 uidstrs
    | None -> 0 in
  let counter = ref (maxuid+1) in
  (fun () ->
    let s = string_of_int !counter in
    (incr counter; s))


let _ =
  (* FIXME: this test sux *)
  let etest filename copt = false in
  let encoding = {
    Surveyor.description = "Palm database (BROKEN!)";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Plain.reader;
    Surveyor.writer = Plain.writer;
    Surveyor.from_string = (fun s -> Some (V.new_value s));
    Surveyor.to_string = (fun v -> V.get_value v);
    Surveyor.base_type = ["palm"];
  }
  in
    (* I'm not familiar enough with this to do it right.  it should be considered
       BROKEN atm.. *)  
    (* Accrodingly, I've made the schemas empty --nate *)
  Surveyor.register_encoding "palm" encoding;
  Optometrist.register_lens ["palm"] ["generic_pdb"] 
    Schemas.empty
    Palm.generic_pdb_lens;
  Optometrist.register_lens ["palm"] ["generic_pdb_rcd"]
    Schemas.empty
    Palm.generic_pdb_rcd_lens;
  Key_factory.register_keyfactory Palm.ugen_factory ("palm", ["generic_pdb"]);
  Key_factory.register_keyfactory Palm.ugen_factory ("palm", ["generic_pdb_rcd"])
