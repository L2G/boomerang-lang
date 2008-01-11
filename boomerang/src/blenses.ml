(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/blenses.ml                                                   *)
(* Boomerang lens combinators                                                  *)
(* $Id$                                                                        *)
(*******************************************************************************)

(* abbreviations from other modules *)
module R = Bregexp
module RS = Bstring

let string_concat = (^)
let (^) = RS.append
let (@) = Safelist.append
let sprintf = Printf.sprintf

let no_check = Prefs.createBool "no-check" false 
  "don't type check lens arguments" 
  "don't type check lens arguments"

(* -------------------- utilities -------------------- *)
let lst_replace = [("\n","\\n");("\t","\\t");("\"","\\\"");("\\", "\\\\")]
let lst_regexp_replace = Safelist.map (fun (p,r) -> (Str.regexp p, r)) lst_replace
let whack s =  
  Safelist.fold_right
    (fun (p, r) s -> Str.global_replace p r s)
    lst_regexp_replace s
    

let get_str n = sprintf "%s get" n
let put_str n = sprintf "%s put" n
let create_str n = sprintf "%s create" n
let parse_str n = sprintf "%s parse" n
let cls_str n = sprintf "%s cls" n
let rep_str n = sprintf "%s rep" n
let key_str n = sprintf "%s key" n
    
let lift_r i n t1 f =  
  (fun x ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else (f x))
    
let lift_rr i n t1 t2 f = (fun x y ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else if not (R.match_str t2 y) then 
      Berror.type_error i (R.string_of_t t2) n (R.split_bad_prefix t2 y)
    else (f x y))

let lift_rsd i n t1 st2 f = (fun x y z ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else if not (st2 y) then 
      assert false
    else (f x y z))


let lift_rd i n t1 f = 
  (fun x y ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else (f x y))

let lift_rrd i n t1 t2 t3 f = 
  (fun x y z ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else if not (R.match_str t2 y) then 
      Berror.type_error i (R.string_of_t t2) n (R.split_bad_prefix t2 y)
    else (f x y z))

let lift_rx i n t1 f = 
  (fun x y ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else (f x y))

let lift_rrx i n t1 t2 f = 
  (fun x y z ->     
    if not (R.match_str t1 x) then 
      Berror.type_error i (R.string_of_t t1) n (R.split_bad_prefix t1 x)
    else if not (R.match_str t2 y) then 
      Berror.type_error i (R.string_of_t t2) n (R.split_bad_prefix t1 y)
    else (f x y z))

(* string maps *)
module SMap = Map.Make 
  (struct
     type t = RS.t
     let compare = RS.compare
   end   
  )


(* keys *)
type key = RS.t

type key_atom = 
    Var of Info.t * RS.t
    | Const of Info.t * RS.t
let string_of_key_atom = function
  | Var(_,x) -> RS.string_of_t x
  | Const(_,s) -> sprintf "\"%s\"" (whack (RS.string_of_t s)) 

module KMap = SMap 

(* tags *) 
type tag = RS.t 
module TMap = SMap

(* skeletons *)
type skeleton = 
  | S_string of RS.t
  | S_concat of (skeleton * skeleton)
  | S_star of skeleton list
  | S_box of tag
  | S_comp of (skeleton * skeleton)

let string_of_skel = function
  | S_string s -> s
  | _ -> assert false

let fst_concat_of_skel = function
  | S_concat (s,_) -> s
  | _ -> assert false

let snd_concat_of_skel = function
  | S_concat (_, s) -> s
  | _ -> assert false

let lst_of_skel = function
  | S_star sl -> sl
  | _ -> assert false

let comp_of_skel = function
  | S_comp sc -> sc
  | _ -> assert false


(* utilities for splitting *)
let split t1 t2 s = 
  match R.unambig_split t1 t2 s with
    | None -> assert false
    | Some s1s2 -> s1s2 

let split_one choose t1 t2 s = 
  let n = RS.length s in 
  let i = choose (R.split_positions t1 t2 s) in 
    (RS.sub s 0 i, RS.sub s i (n-i))

(* utils *)
let split2 t11 t12 t21 t22 (x1,x2) = 
  let x11,x12 = split t11 t12 x1 in        
  let x21,x22 = split t21 t22 x2 in 
    ((x11,x21),(x12,x22)) 

let do_split split f1 f2 combine = (fun x -> 
  let x1,x2 = split x in 
    combine (f1 x1) (f2 x2))

let do_split_thread split f1 f2 combine = (fun x y ->
  let x1,x2 = split x in 
  let x1,y1 = f1 x1 y in 
  let x2,y2 = f2 x2 y1 in 
    (combine x1 x2, y2))


(* utilities for iterating *)
let star_loop t f x = 
  Safelist.fold_left
    (fun acc xi -> RS.append acc (f xi))
    RS.empty (R.unambig_star_split t x) 

let branch t f1 f2 = 
  (fun x -> 
    if R.match_str t x then f1 x 
    else f2 x) 

let branch2 t f1 f2 = 
  (fun x y ->
     if R.match_str t x then f1 x y 
     else f2 x y)

(*** hack for uid ***)
type uid = int
let current_uid = ref 0
let next_uid () = 
  incr current_uid;
  !current_uid


(* -------------------- CANONIZERS -------------------- *)
module Canonizer = struct
  type t = 
      { (* --- meta data --- *)
        info: Info.t;
        string: string;
        (* --- types --- *)
        rtype : R.t;
        ctype : R.t;
        (* --- core functions --- *)
        cls : RS.t -> RS.t;                  (* class function *)
        rep : RS.t -> RS.t                   (* representative function *)
      }

  let info cn = cn.info
  let string cn = cn.string
  let rtype cn = cn.rtype
  let ctype cn = cn.ctype
  let cls cn = cn.cls
  let rep cn = cn.rep
  let mk_t i s rt ct c r = 
    { info = i;
      string = s;
      rtype = rt;
      ctype = ct;
      cls = c;
      rep = r; 
    }
      
  (* add primitives ... *)
  let concat i cn1 cn2 = 
    let n = sprintf "concat (%s) (%s)" cn1.string cn2.string in 
    let rt = R.unambig_seq i n cn1.rtype cn2.rtype in 
    let ct = R.seq cn1.ctype cn2.ctype in 
    { info = i;
      string = n;
      rtype = rt;
      ctype = ct;
      cls = lift_r i (cls_str n) rt 
        (do_split (split cn1.rtype cn2.rtype)
            cn1.cls cn2.cls (^));
      rep = lift_r i (rep_str n) ct 
        (do_split ((split_one Rint.Set.min_elt) cn1.ctype cn2.ctype)
            cn1.rep cn2.rep (^));
    }

  let union i cn1 cn2 = 
    let n = sprintf "union (%s) (%s)" cn1.string cn2.string in 
    let rt = R.disjoint_alt i n cn1.rtype cn2.rtype in 
    let ct = R.alt cn1.ctype cn2.ctype in 
    { info = i;
      string = n;
      rtype = rt;
      ctype = ct;
      cls = lift_r i (cls_str n) rt (branch cn1.rtype cn1.cls cn2.cls);
      rep = lift_r i (rep_str n) ct (branch cn1.ctype cn1.rep cn2.rep);
    }

  let star i cn1 = 
    let n = sprintf "(%s)*" cn1.string in 
    let rt = R.unambig_star i n cn1.rtype in 
    let ct = R.star cn1.ctype in 
    { info = i;
      string = n;
      rtype = rt;
      ctype = ct;
      cls = lift_r i (cls_str n) rt 
        (star_loop cn1.rtype cn1.cls);      
      rep = lift_r i (rep_str n) ct (fun cl -> 
        let rec loop cl acc = 
          if RS.length cl = 0 then acc
          else
            let cl1,clrest = (split_one Rint.Set.min_elt) cn1.ctype ct cl in 
              loop clrest (acc ^ (cn1.rep cl1)) in 
        loop cl RS.empty);
    }
end

(* simple sort checking for relations *)
(* we only track if it is the identity *)
type rel = Identity | Unknown
let combine_rel r1 r2 = match r1,r2 with 
  | Identity,Identity -> Identity 
  | _                 -> Unknown

(* -------------------- DICTIONARY LENSES -------------------- *)
module DLens = struct    

(* classical dictionaries *)
type cdict = 
  | CD_empty
  | CD of (((skeleton * cdict) list) KMap.t) TMap.t
   
(* ddict stands for double dict. Used for default *)
and ddict = { provisory_dict:dict;
	      main_dict:dict; }

and dict = 
  | CDict of cdict
  | DDict of ddict

(* dictionaries type type. At each level, we need to check that for
   one tag, there is only one unique id for this tag *)

and dict_type = uid TMap.t

and t = 
    { (* --- meta data --- *)
      info: Info.t;                           (* parsing info *)
      string: string;                         (* pretty printer *)
      (* --- types --- *)
      ctype: R.t;                             (* concrete type *)
      atype: R.t;                             (* abstract type *)
      dtype: dict_type;                       (* dictionary type *)
      stype: skeleton -> bool;                (* given a skeleton, returns if it is part
						 of the skeleton type of the lens*)
      crel: rel;                              (* concrete equiv rel type *)
      arel: rel;                              (* abstract equiv rel type *)
      (* --- core --- *)
      get: RS.t -> RS.t;                      (* get function *)
      put: RS.t -> skeleton -> dict -> (RS.t * dict);  (* put function *)
      parse: RS.t -> (skeleton * cdict);      (* parse function *)
      create: RS.t -> dict -> (RS.t * dict);  (* create function *)
      key: RS.t -> RS.t;                      (* key function *)
      (* --- hack --- *)
      uid: uid
    }

let assert_lens_type i l co ao = 
	(* shouldn'd be no-assert ? *)
  if not (Prefs.read no_check) then 
    begin 
      let check_rx s = function
        | None -> None
        | Some r -> 
            let equiv, f_suppl = R.check_equiv r s in 
            if equiv then None else Some (f_suppl) in
      match check_rx l.ctype co, check_rx l.atype ao with 
        | None, None -> ()
        | Some f, _ | _, Some f ->
            (* FIX: say which of C and A this comes from *)
            Berror.static_error i "assert_lens_type" ~suppl:f
            (Util.format_to_string  
               (fun () -> 
                  Util.format 
                    "expected @[<2>%s@ <->@ %s@]@ but@ found @[<2>%s@ <->@ %s@]"
                    (match co with None -> "?" | Some c -> R.string_of_t c)
                    (match ao with None -> "?" | Some a -> R.string_of_t a)
                    (R.string_of_t l.ctype)
                    (R.string_of_t l.atype)))
    end;
  l

let assert_lens_ctype i l c = assert_lens_type i l (Some c) None

let assert_lens_atype i l a = assert_lens_type i l None (Some a)
   
(* lookup function in dictionnaries of both type *)
let rec lookup tag k = function 
  | CDict CD_empty -> None 
  | CDict (CD d) ->
      (let km = try TMap.find tag d with Not_found -> KMap.empty in
       try 
       (match KMap.find k km with 
	  | c::kl -> Some (c, CDict (CD (TMap.add tag (KMap.add k kl km) d)))
	  | [] -> None)
       with Not_found -> None)
  | DDict dd ->
      (match lookup tag k dd.provisory_dict with
	 | None -> (* not found in the provisory dictionnary, look in the other one*)
	     (match lookup tag k dd.main_dict with
		| None -> None
		| Some (r, d) -> Some (r, DDict {dd with main_dict = d}))
	 | Some (r, d) -> Some (r, DDict {dd with provisory_dict = d}))
	


exception Incompatilbe_dict_type of tag
let fusion_dict_type dt1 dt2 = 
  TMap.fold 
    (fun t u acc ->
       if (not (TMap.mem t dt2)) || (TMap.find t dt2 = u) then
	 TMap.add t u acc
       else raise (Incompatilbe_dict_type t)) dt1 dt2

let safe_fusion_dict_type i dt1 dt2 = 
  try 
    fusion_dict_type dt1 dt2 
  with
    | Incompatilbe_dict_type t -> 
	raise (Error.Harmony_error 
		 (fun () -> 
		    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
		    Util.format "The tag \"%s\" is used twice with different lenses@]@\n" (RS.string_of_t t);))


(* helper: combine maps with merge. 
   Mapplus's combine is (combine fold find add (curry fst)) *)
let combine fold find add merge m1 m2 = 
  fold (fun k v -> 
    add k (try merge v (find k m2) with Not_found -> v))
    m1 m2  

(* smash two classic dictionaries *)
let (++) cd1 cd2 = match (cd1,cd2) with
  | CD_empty, CD_empty -> CD_empty
  | CD_empty, cd
  | cd, CD_empty -> cd
  | CD d1', CD d2' ->
      CD (combine TMap.fold TMap.find TMap.add 
	   (fun km1 km2 -> 
	      (combine KMap.fold KMap.find KMap.add 
		 (fun kl1 kl2 -> kl1 @ kl2)
		 km1 km2))
	   d1' d2')

  let info dl = dl.info
  let string dl = dl.string
  let ctype dl = dl.ctype
  let atype dl = dl.atype
  let stype dl = dl.stype
  let dtype dl = dl.dtype
  let crel dl = dl.crel
  let arel dl = dl.arel
  let get dl = dl.get
  let put dl = dl.put
  let parse dl = dl.parse
  let create dl = dl.create
  let k dl = dl.key
  let uid dl = dl.uid

  let mk_t i s ct at dt st cr ar g put parse c k uid = 
    { info = i;
      string = s;
      ctype = ct;
      atype = at;
      dtype = dt;
      stype = st;
      crel = cr;
      arel = ar;
      get = g;
      put = put;
      parse = parse;
      create = c; 
      key = k;
      uid = uid;
    }

(* pseudo rlenses function *)

  let rput_of_dl dl = 
    fun a c -> 
      let s,d = dl.parse c in
	fst (dl.put a s (CDict d))

  let rcreate_of_dl dl = 
    fun a ->
      fst (dl.create a (CDict CD_empty))


  let determinize_dlens dl =
    {dl with 
       ctype = R.determinize dl.ctype; 
       atype = R.determinize dl.atype}

  let forgetkey dl = 
    {dl with
       key = (fun _ -> RS.empty);
       uid = next_uid();}

  let canonizer_of_t i dl = 
    Canonizer.mk_t 
      i
      (sprintf "canonizer_of_lens(%s)" dl.string)
      dl.ctype
      dl.atype
      dl.get
      (rcreate_of_dl dl)


  (* ---------- copy ---------- *)
  let copy i r = 
    let n = sprintf "cp (%s)" (R.string_of_t r) in 
    let ct = r in 
    let at = r in
    let dt = TMap.empty in
    let st = function
      | S_string s -> R.match_str r s
      | _ -> false in
    { info = i;
      string = n;
      ctype = ct;
      atype = at;
      dtype = dt;
      stype = st;
      crel = Identity;
      arel = Identity;
      get = lift_r i (get_str n) ct (fun c -> c);
      put = lift_rsd i (put_str n) at st (fun a _ d -> (a,d));
      parse = lift_r i (parse_str n) ct (fun c -> (S_string c, CD_empty)); 
      create = lift_rd i (create_str n) at (fun a d -> a,d);
      key = lift_r i n at (fun _ -> RS.empty);
      uid = next_uid ();
    }

  let key i r = 
    let c = copy i r in
      {c with key = lift_r i c.string c.atype (fun a -> a)}


  (* ---------- const ---------- *)
  let const i r u_str def_str =
    let u = RS.string_of_t u_str in
    let def = RS.string_of_t def_str in
    let n = sprintf "const (%s) \"%s\" \"%s\"" (R.string_of_t r) (whack u) (whack def) in 
    let ct = r in 
    let at = R.str false u_str in
    let dt = TMap.empty in
    let st = function
      | S_string s -> R.match_str r s
      | _ -> false in
    let () = 
      if not (R.match_str r def_str) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def (R.string_of_t r)) in 
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
	dtype = dt;
	stype = st;
        crel = Identity;
        arel = Identity;
        get = lift_r i (get_str n) ct (fun c -> u_str);
        put = lift_rsd i (put_str n) at st (fun _ s d -> (string_of_skel s, d) );
	parse = lift_r i (parse_str n) ct (fun c -> (S_string c, CD_empty));  
        create = lift_rd i (create_str n) at (fun a d -> (def_str,d));
	key = lift_r i n at (fun _ -> RS.empty);
	uid = next_uid ();
      }


  (* ---------- quotient const ---------- *)
  let qconst i rc ra u_str def_str =
    let u = RS.string_of_t u_str in
    let def = RS.string_of_t def_str in
    let n = sprintf "qconst (%s) (%s) \"%s\" \"%s\"" (R.string_of_t rc) (R.string_of_t ra) (whack u) (whack def) in 
    let ct = rc in 
    let at = ra in
    let dt = TMap.empty in
    let st = function
      | S_string s -> R.match_str rc s
      | _ -> false in
    let () = 
      if not (R.match_str ra u_str) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def (R.string_of_t ra)) in 
    let () = 
      if not (R.match_str rc def_str) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def (R.string_of_t rc)) in 
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
	dtype = dt;
	stype = st;
        crel = Identity;
        arel = Unknown;
        get = lift_r i (get_str n) ct (fun c -> u_str);
        put = lift_rsd i (put_str n) at st (fun _ s d -> (string_of_skel s, d) );
	parse = lift_r i (parse_str n) ct (fun c -> (S_string c, CD_empty));  
        create = lift_rd i (create_str n) at (fun a d -> (def_str,d));
	key = lift_r i n at (fun _ -> RS.empty);
	uid = next_uid ();
      }



  (* ---------- concat ---------- *)
  let concat i dl1 dl2 = 
    let n = sprintf "%s . %s" dl1.string dl2.string in 
    let ct = R.unambig_seq i n dl1.ctype dl2.ctype in 
    let at = R.unambig_seq i n dl1.atype dl2.atype in 
    let dt = safe_fusion_dict_type i dl1.dtype dl2.dtype in
    let st = function
      | S_concat (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
    (* lens *) 
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;
	dtype = dt;
	stype = st;
        crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = lift_r i (get_str n) ct 
          (do_split (split dl1.ctype dl2.ctype)
              dl1.get dl2.get
              (^));
        put = lift_rsd i (put_str n) at st (fun a s d -> 
          let a1,a2 = split dl1.atype dl2.atype a in 
	  let c1,d1 = dl1.put a1 (fst_concat_of_skel s) d in
	  let c2,d2 = dl2.put a2 (snd_concat_of_skel s) d1 in
	    (c1 ^ c2, d2));
	parse = lift_r i (parse_str n) ct (fun c->
          let c1, c2 = split dl1.ctype dl2.ctype c in
	  let s1, d1 = dl1.parse c1 in
	  let s2, d2 = dl2.parse c2 in
	    (S_concat (s1, s2), d1 ++ d2));
	create = lift_rd i n at (fun a d ->
          let a1, a2 = split dl1.atype dl2.atype a in
	  let c1, d1 = dl1.create a1 d in
	  let c2, d2 = dl2.create a2 d1 in
	  (c1 ^ c2, d2));
	key = lift_r i n at (fun a ->
          let a1,a2 = split dl1.atype dl2.atype a in
	  (dl1.key a1) ^ (dl2.key a2));
	uid = next_uid ();}          

  let union i dl1 dl2 = 
    (* utilities *)
    let bare_get = branch dl1.ctype dl1.get dl2.get in
    let n = sprintf "(%s|%s)" dl1.string dl2.string in 
    let at = R.alt dl1.atype dl2.atype in 
    let ct = R.disjoint_alt i n  dl1.ctype dl2.ctype in 
      (**** We still need to check equality of keys ***)
    let dt = safe_fusion_dict_type i dl1.dtype dl2.dtype in
    let st s = dl1.stype s || dl2.stype s in
      { info = i;
        string = n;
        ctype = ct; 
        atype = at;
	dtype = dt;
	stype = st;
	crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = lift_r i (get_str n) ct bare_get;
        put = lift_rsd i (put_str n) at st (fun a s d -> 
          match R.match_str dl1.atype a, 
            R.match_str dl2.atype a,
            dl1.stype s with
              | true,_,true  -> dl1.put a s d
              | _,true,false -> dl2.put a s d
              | true,false,false -> dl1.create a d 
              | false,true,true  -> dl2.create a d
              | false,false,_    -> assert false);
	parse =  lift_r i (parse_str n) ct 
	           (branch dl1.ctype dl1.parse dl2.parse); 
        create = lift_rd i (create_str n) at 
          (branch2 dl1.atype dl1.create dl2.create);
	key = lift_r i n at 
	  (branch dl1.atype dl1.key dl2.key);
	uid = next_uid ();
      }

  let star i dl1 = 
    (* body *)
    let n = sprintf "(%s)*" dl1.string in
    let ct = R.unambig_star i n dl1.ctype in 
    let at = R.unambig_star i n dl1.atype in 
    let dt = dl1.dtype in
    let st = function
      | S_star sl -> Safelist.fold_left (fun b s -> b && dl1.stype s) true sl
      | _ -> false in
      { info = i;
	string = n;
	ctype = ct;
	atype = at;
	dtype = dt;
	stype = st;
        crel = dl1.crel;
        arel = dl1.arel;
        get = lift_r i (get_str n) ct (star_loop dl1.ctype dl1.get);
        put = lift_rsd i (put_str n) at st (fun a s d -> 
	  let rec loop al sl buf d = match al,sl with
              [],_ -> (buf, d)
            | a1::at,[] ->
		let c1,d1 = dl1.create a1 d in
		loop at [] (buf ^ c1) d1
            | a1::at,s1::st ->
		let c1, d1 = dl1.put a1 s1 d in 
		  loop at st (buf ^ c1) d1 in 
            loop 
              (R.unambig_star_split dl1.atype a)
              (lst_of_skel s)
	      RS.empty
	      d);
	create = lift_rd i (create_str n) at (fun a d -> 
          Safelist.fold_left (fun (buf, d) a1 -> 
            let c1,d' = dl1.create a1 d in 
            let buf' = RS.append buf c1 in
              (buf', d'))
            (RS.empty, d) (R.unambig_star_split dl1.atype a));
	parse = lift_r i (parse_str n) ct (fun c ->
          let (sl, d) = Safelist.fold_left (fun (buf, d) c1 -> 
            let s1,d1 = dl1.parse c1 in
	    let buf' = s1::buf  in
	    let d' = d ++ d1 in
	      (buf', d'))
            ([], CD_empty)
            (R.unambig_star_split dl1.ctype c) in
	  (S_star (Safelist.rev sl), d));
	key = lift_r i (key_str n) at 
          (star_loop dl1.atype dl1.key);
	uid = next_uid ();

      }

  (* non-standard lenses *)
  let swap i dl1 dl2 = 
    let n = sprintf "swap (%s) (%s)" dl1.string dl2.string in 
    let at = R.unambig_seq i n dl2.atype dl1.atype in 
    let ct = R.unambig_seq i n dl1.ctype dl2.ctype in 
    let dt = safe_fusion_dict_type i dl1.dtype dl2.dtype in
    let st = function
      | S_concat (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
     { info = i;
      string = n;
      ctype = ct; 
      atype = at;
      dtype = dt;
      stype = st;
      crel = combine_rel dl1.crel dl2.crel;
      arel = combine_rel dl1.arel dl2.arel;
      get = lift_r i n ct (fun c -> 
          let c1,c2 = split dl1.ctype dl2.ctype c in 
            (dl2.get c2) ^ (dl1.get c1));
      put = lift_rsd i (put_str n) at st (fun a s d -> 
        let a2,a1 = split dl2.atype dl1.atype a in 
        let c2,d1 = dl2.put a2 (snd_concat_of_skel s) d in 
        let c1,d2 = dl1.put a1 (fst_concat_of_skel s) d1 in 
          (c1 ^ c2, d2));
      create = lift_rd i (create_str n) at (fun a d -> 
        let a2,a1 = split dl2.atype dl1.atype a in          
        let c2,d1 = dl2.create a2 d in 
        let c1,d2 = dl1.create a1 d1 in 
          (c1 ^ c2, d2));
      parse = lift_r i (parse_str n) ct (fun c ->
        let c1,c2 = split dl1.ctype dl2.ctype c in 
        let s2,d2 = dl2.parse c2 in 
        let s1,d1 = dl1.parse c1 in 
          (S_concat (s1,s2), d2++d1)); 
      key = lift_r i n at (fun a ->
        let a2, a1 = split dl2.atype dl1.atype a in
	  (dl2.key a2) ^ (dl1.key a1));
      uid = next_uid ();
     }
        
  let compose i dl1 dl2 = 
    let n = sprintf "%s; %s" dl1.string dl2.string in 
    let ct = dl1.ctype in
    let at = dl2.atype in 
    let dt = safe_fusion_dict_type i dl1.dtype dl2.dtype in
    let st = function
      | S_comp (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
    (match dl1.arel,dl2.crel with
      | Identity,Identity -> ()
      | _ -> 
          let s = sprintf "the composition of %s and %s is ill-typed: %s"            
            dl1.string dl2.string 
            "the middle relations must both be the identity" in 
            Berror.static_error i n s);
    let equiv, f_suppl = R.check_equiv dl1.atype dl2.ctype in
      if not equiv then
        begin
	  let s =(sprintf "the composition of %s and %s is ill-typed:"
		     dl1.string dl2.string)in
	    Berror.static_error i n ~suppl:f_suppl s 
        end;
    { info = i; 
      string = n;
      ctype = ct;      
      atype = at;
      dtype = dt;
      stype = st;
      crel = dl1.crel;
      arel = dl2.arel;
      get = lift_r i (get_str n) ct (fun c -> dl2.get (dl1.get c));
      put = lift_rsd i n at st 
	(fun a s d  ->
	   let s1,s2 = comp_of_skel s in
	   let b, d1 = dl2.put a s2 d in
	   dl1.put b s1 d1);
      create = lift_rd i n at 
	(fun a d ->
	   let b, d1 = dl2.create a d in
	   dl1.create b d1);
      parse = lift_r i n ct
	(fun c -> 
	   let s1, d1 = dl1.parse c in
	   let s2,d2 = dl2.parse (dl1.get c) in
	     (S_comp (s1, s2), d2 ++ d1));
      key = dl2.key;
      uid = next_uid();
    }

  let default i def dl1 = 
    let n = sprintf "default %s %s" (RS.string_of_t def) dl1.string in 
    let at = dl1.atype in 
    let () = 
      if not (R.match_str dl1.ctype def) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" (RS.string_of_t def) (R.string_of_t dl1.ctype)) in 
    let s,d = dl1.parse def in
      { dl1 with
        create = lift_rd i (create_str n) at (fun a d' -> 
          (* changing of behavior for former D-lenses ! Information
	   * are added to the dictionnary only for the time of the
	   * create and are then removed. This modification was needed
	   * because of the streaming lenses and the two type of
	   * dictionnaries that could not be smached together anymore*)
          match dl1.put a s (DDict {provisory_dict = CDict d; main_dict = d'}) with
	    | res, DDict dd -> (res, dd.main_dict)
	    | _ -> assert false);
	  uid = next_uid ();
      }


  let smatch i tag dl1 = 
    let n = sprintf "<%s>" dl1.string in
    let ct = dl1.ctype in 
    let at = dl1.atype in 
    let st = function
      | S_box b -> (RS.compare tag b) = 0
      | _ -> false in
    let dt = TMap.add tag dl1.uid TMap.empty in
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        stype = st;
	dtype = dt;
        crel = dl1.crel;
        arel = dl1.arel;
        get = lift_r i (get_str n) ct (fun c -> dl1.get c);
        put = lift_rsd i (put_str n) at st (fun a _ d -> 
	   match lookup tag (dl1.key a) d with
	       Some((s',d'),d'') -> (fst (dl1.put a s' (CDict d')),d'')
	     | None       -> (fst (dl1.create a (CDict CD_empty)), d));
        create = lift_rx i (create_str n) at (fun a d -> 
	  match lookup tag (dl1.key a) d with
	      Some((s',d'),d'') -> (fst (dl1.put a s' (CDict d')),d'')
	    | None       -> (fst (dl1.create a (CDict CD_empty)), d));
        parse = lift_r i (parse_str n) ct (fun c -> 
	  let s,d = dl1.parse c in
	  let d' = TMap.add tag (KMap.add (dl1.key (dl1.get c)) [(s,d)] KMap.empty) TMap.empty in 
	    (S_box tag, CD d'));
	key = dl1.key;
	uid = next_uid ();
      }



  let filter i rd rk =
    let n = sprintf "filter %s %s" (R.string_of_t rd) (R.string_of_t rk) in 
    let ru = R.disjoint_alt i n rd rk in
    let ct = R.unambig_star i n ru in
    let at = R.unambig_star i n rk in
    let dt = TMap.empty in
    let st = function
      | S_string s -> R.match_str ct s
      | _ -> false in
    let get = lift_r i (get_str n) ct 
      (fun c ->
         let rec loop acc = function 
           | [] -> acc
           | h :: t -> 
               if R.match_str rd h then 
                 loop acc t
               else 
                 loop (acc ^ h) t in
         let lc = R.unambig_star_split ct c in
           loop RS.empty lc) in
    let put = lift_rsd i (put_str n) at st
      (fun a s d ->
         let c = string_of_skel s in
         let rec loop acc lc la = match lc,la with
           | [], [] -> acc
           | [], ha :: ta -> loop (acc ^ ha) [] ta
           | hc :: tc, [] -> 
               if R.match_str rd hc then
                 loop (acc ^ hc) tc []
               else
                 loop acc tc []
           | hc :: tc, ha :: ta -> 
               if R.match_str rd hc then
                 loop (acc ^ hc) tc la
               else
                 loop (acc ^ ha) tc ta in
         let lc = R.unambig_star_split ct c in
         let la = R.unambig_star_split at a in
           (loop RS.empty lc la, d)) in
    let create = lift_rd i n at (fun a d -> (a,d)) in
    let parse = lift_r i n ct (fun c -> (S_string c, CD_empty))in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;
        dtype = dt;
	stype = st;
	crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create;
	parse = parse;
	key = lift_r i n at (fun _ -> RS.empty);
	uid = next_uid ();
      }


  (* left quotient of a dlens by a canonizer. I don't know if we
     should check if the dlens is not already a q-lens. *)

  let left_quot i cn dl = 
    let n = sprintf "left quotient of %s by %s" dl.string (Canonizer.string cn) in
      (* the "class type" of the canonizer has to be equal to the
	 concrete type of the lens *)
    let equiv, f_suppl = R.check_equiv (Canonizer.ctype cn) dl.ctype in
      if not equiv then
        begin
	  let s =sprintf "the %s is ill-typed:" n in
	    Berror.static_error i n ~suppl:f_suppl s 
        end;
    let ct = Canonizer.rtype cn in
    let at = dl.atype in
    let dt = dl.dtype in
    let st = dl.stype in
    let cls = Canonizer.cls cn in
    let rep = Canonizer.rep cn in
    { info = i; 
      string = n;
      ctype = ct;
      atype = at;
      dtype = dt;
      stype = st;
      crel = Unknown;
      arel = dl.arel;
      get = lift_r i (get_str n) ct 
	(fun c -> dl.get(cls c));
      put = lift_rsd i n at st (* useless lift_rsd ... *)
	(fun a s d ->
	  let cc, d = dl.put a s d in
	  (rep cc, d));
      create = lift_rd i n at
	(fun a d ->
	  let cc, d = dl.create a d in
	  (rep cc, d));
      parse = lift_r i n ct
	(fun c ->
	  let cc = cls c in
	  dl.parse cc);
      key = dl.key;
      uid = next_uid();
    }

  (* right quotient of a dlens by a canonizer. I don't know if we
     should check if the dlens is not already a q-lens. *)
  let right_quot i dl cn= 
    let n = sprintf "right quotient of %s by %s" dl.string (Canonizer.string cn) in
      (* the "class type" of the canonizer has to be equal to the
	 abstract type of the lens *)
    let equiv, f_suppl = R.check_equiv (Canonizer.ctype cn) dl.atype in
      if not equiv then
        begin
	  let s =sprintf "the %s is ill-typed:" n in
	    Berror.static_error i n ~suppl:f_suppl s 
        end;
    let ct = dl.ctype in
    let at = Canonizer.rtype cn in
    let dt = dl.dtype in
    let st = dl.stype in
    let cls = Canonizer.cls cn in
    let rep = Canonizer.rep cn in
    { info = i; 
      string = n;
      ctype = ct;      
      atype = at;
      dtype = dt;
      stype = st;
      crel = dl.crel;
      arel = Unknown;
      get = lift_r i (get_str n) ct 
	(fun c -> rep (dl.get c));
      put = lift_rsd i n at st 
	(fun a s d ->
	  let ac = cls a in
	  dl.put ac s d);
      create = lift_rd i n at
	(fun a d ->
	  let ac = cls a in
	  dl.create ac d);
      parse = dl.parse;

      (* BIG QUESTION ABOUT LENS LAWS*)
      (* I'm not 100% convinced about this implementation and about
	 the fact it ensure the conservation of the laws *)
      key = (fun a -> dl.key (cls a));
      uid = next_uid();
    }
end
