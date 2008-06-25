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
(* $Id$ *)
(*******************************************************************************)

(* imports and abbreviations *)
let string_concat = (^)
let sprintf = Printf.sprintf
let msg = Util.format
let (@) = Safelist.append

(* whack: inserts escape codes in rendered strings *)
let whack = 
  let replacements = 
    [(Str.regexp "\n","\\n");
     (Str.regexp "\t","\\t");
     (Str.regexp "\"","\\\"");
     (Str.regexp "\\", "\\\\")] in 
  (fun s ->
     Safelist.fold_right 
       (fun (f,t) s -> Str.global_replace f t s)
       replacements s)
    
let get_str n = sprintf "%s get" n
let put_str n = sprintf "%s put" n
let create_str n = sprintf "%s create" n
let parse_str n = sprintf "%s parse" n
let canonize_str n = sprintf "%s canonize" n
let choose_str n = sprintf "%s choose" n
let key_str n = sprintf "%s key" n

let paranoid = Prefs.createBool "paranoid" false
  "do paranoid sanity checks in lens primitives"
  "do paranoid sanity checks in lens primitives"
  
let plift_r b i n t1 f =  
  if not (b || Prefs.read paranoid) then f else
    (fun x ->     
       if not (Brx.match_string t1 x) then 
         Berror.type_error i (Brx.split_bad_prefix t1 x)
       else (f x))

let plift_rr b i n t1 t2 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y ->     
     if not (Brx.match_string t1 x) then 
       Berror.type_error i (Brx.split_bad_prefix t1 x)
     else if not (Brx.match_string t2 y) then 
       Berror.type_error i (Brx.split_bad_prefix t2 y)
     else (f x y))

let plift_rsd b i n t1 st2 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y z ->     
     if not (Brx.match_string t1 x) then 
       Berror.type_error i (Brx.split_bad_prefix t1 x)
     else if not (st2 y) then 
       assert false
     else (f x y z))
    
let plift_rd b i n t1 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y ->     
     if not (Brx.match_string t1 x) then 
       Berror.type_error i (Brx.split_bad_prefix t1 x)
     else (f x y))

let plift_rrd b i n t1 t2 t3 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y z ->     
     if not (Brx.match_string t1 x) then 
       Berror.type_error i (Brx.split_bad_prefix t1 x)
     else if not (Brx.match_string t2 y) then 
       Berror.type_error i (Brx.split_bad_prefix t2 y)
     else (f x y z))

let plift_rx b i n t1 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y ->     
     if not (Brx.match_string t1 x) then 
       Berror.type_error i (Brx.split_bad_prefix t1 x)
     else (f x y))

let plift_rrx b i n t1 t2 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y z ->     
     if not (Brx.match_string t1 x) then 
       Berror.type_error i (Brx.split_bad_prefix t1 x)
     else if not (Brx.match_string t2 y) then 
       Berror.type_error i (Brx.split_bad_prefix t1 y)
     else (f x y z))

let lift_r i n r = plift_r false i n r
let lift_rr i n r r = plift_rr false i n r r 
let lift_rsd i n r s d = plift_rsd false i n r s d 
let lift_rd i n r d = plift_rd false i n r d 
let lift_rrd i n r r d = plift_rrd false i n r r d
let lift_rx i n r x = plift_rx false i n r x 
let lift_rrx i n r r x = plift_rrx false i n r r x
    
(* keys *)
type key = string
module KMap = Map.Make(
  struct 
    type t = string 
    let compare = compare 
  end)

(* tags *) 
type tag = string
module TMap = Map.Make(
  struct 
    type t = string 
    let compare = compare 
  end)

(* skeletons *)
type skeleton = 
  | S_string of string
  | S_concat of (skeleton * skeleton)
  | S_dup of (skeleton * skeleton)
  | S_star of skeleton list
  | S_box of tag
  | S_comp of (skeleton * skeleton)

(* helpers for unpacking skeletons *)
let string_of_skel i = function
  | S_string s -> s
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ string@ skeleton@]") 

let fst_concat_of_skel i = function
  | S_concat (s,_) -> s
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ concat@ skeleton@]") 

let snd_concat_of_skel i = function
  | S_concat (_, s) -> s
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ concat@ skeleton@]") 

let fst_dup_of_skel i = function
  | S_dup (s,_) -> s
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ dup@ skeleton@]") 

let snd_dup_of_skel i = function
  | S_dup (_, s) -> s
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ dup@ skeleton@]") 
      
let lst_of_skel i = function
  | S_star sl -> sl
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ lst@ skeleton@]") 

let comp_of_skel i = function
  | S_comp sc -> sc
  | _ -> Berror.run_error i (fun () -> msg "@[expected@ comp@ skeleton@]") 

(* helpers for iterating *)
let rec generic_iter i epsilon union concat star x min max = 
  let rec mk_cats x n = 
    if n=0 then epsilon
    else if n=1 then x
    else if n=2 then concat x x
    else 
      let half_x = mk_cats x (n/2) in 
      let twice_half_x = concat half_x half_x in 
      if n mod 2 = 0 then twice_half_x
      else concat x twice_half_x in 
   match min,max with
     | (0,-1) -> star x
     | (n,-1) -> concat (mk_cats x n) (star x)
     | (0,0)  -> epsilon
     | (0,1)  -> union epsilon x
     | (m,n)  -> 
         if m > n then 
           Berror.run_error i 
             (fun () -> Util.format "error in iteration: %d > %d" m n)
         else if m=n then mk_cats x n
         else (* n > m *)
           let rec aux (xi,us) j = 
             if j=0 then us
             else
               let xi1 = concat x xi in 
               aux (xi1, union us xi1) (pred j) in 
           let x1 = if m=0 then epsilon else mk_cats x m in 
           aux (x1,x1) (n-m) 

(* helpers for splitting *)    
let split_one choose t1 t2 s = 
  let ps = Brx.split_positions t1 t2 s in 
  let n = String.length s in 
  let j = choose ps in 
  (String.sub s 0 j, String.sub s j (n-j))

let split_both t1 t2 s = 
  let get_representative t = match Brx.representative t with 
    | None -> 
        Berror.run_error 
          (Info.M "split_both") 
          (fun () -> Util.format "%s was empty" (Brx.string_of_t t)) 
    | Some s -> s in 
  let s1 = get_representative t1 in 
  let s2 = get_representative t2 in 
  (s1^s,s2), (s1,s^s2)

(* helpers for concat-like operators *)
  let seq_split i t1 t2 w = match Brx.seq_split t1 t2 w with
    | None -> 
        Berror.run_error i 
          (fun () -> Util.format "the concatenation of %s and %s was ambiguous" 
             (Brx.string_of_t t1) (Brx.string_of_t t2)) 
    | Some p -> p 

let split2 t11 t12 t21 t22 (x1,x2) = 
  let i = Info.M "split2" in 
  let x11,x12 = seq_split i t11 t12 x1 in
  let x21,x22 = seq_split i t21 t22 x2 in 
  ((x11,x21),(x12,x22)) 

let do_split split f1 f2 combine = 
  (fun x -> 
     let x1,x2 = split x in 
     combine (f1 x1) (f2 x2))
    
let do_split_thread split f1 f2 combine = 
  (fun x y ->
     let x1,x2 = split x in 
     let x1,y1 = f1 x1 y in 
     let x2,y2 = f2 x2 y1 in 
       (combine x1 x2, y2))
    
(* helpers for iteration operators *)
let star_loop t f x = 
  Safelist.fold_left
    (fun acc xi -> acc ^ (f xi))
    "" (Brx.star_split t x) 

(* helpers for conditional operators *)
let branch t f1 f2 = 
  (fun x -> 
     if Brx.match_string t x then f1 x 
     else f2 x) 

let branch2 t f1 f2 = 
  (fun x y ->
     if Brx.match_string t x then f1 x y 
     else f2 x y)
    
(* uid *)
type uid = int
let current_uid = ref 0
let next_uid () = 
  incr current_uid;
  !current_uid

(* -----------------------------------------------------------------------------*)
(* CANONIZERS *)
module Canonizer = struct
  type t = 
      { (* --- meta data --- *)
        info: Info.t;
        string: string;
        (* --- types --- *)
        rtype : Brx.t;
        ctype : Brx.t;
        (* --- core functions --- *)
        canonize : string -> string;
        choose : string -> string;
      }

  (* accessors *)
  let info cn = cn.info
  let string cn = cn.string
  let rtype cn = cn.rtype
  let ctype cn = cn.ctype
  let canonize cn = cn.canonize
  let choose cn = cn.choose
  let mk_t i s rt ct cn ch = 
    { info = i;
      string = s;
      rtype = rt;
      ctype = ct;
      canonize = cn;
      choose = ch; 
    }

  let copy i r = 
    let n = sprintf "copy (%s)" (Brx.string_of_t r) in 
    let rt = r in 
    let ct = r in
      { info = i;
        string = n;
        rtype = rt;
        ctype = ct;
        canonize = (fun x -> x);
        choose = (fun x -> x);
      }

(*   (\* add primitives ... *\) *)
(*   let columnize i ks r s nl =      *)
(*     let n = sprintf "columnize %s (%s) (%s) (%s)" ks r s nl in *)
(*     let k =  *)
(*       try int_of_string ks with _ ->  *)
(*         Berror.static_error i n *)
(*           (sprintf "%s must be an integer@\n" ks) in *)
(*     let () =  *)
(*       let contains_nl = R.seq Brx.anything (R.seq (R.str false nl) any) in  *)
(*       let r_contains_nl = not (R.is_empty (R.inter r contains_nl)) in       *)
(*         if RS.length s != 1 then  *)
(*           Berror.static_error i n  *)
(*             (sprintf "%s must have length 1@\n" *)
(*                (RS.string_of_t s)); *)
(*         if r_contains_nl then  *)
(*           Berror.static_error i n  *)
(*             (sprintf "%s contains %s" *)
(*                (Brx.string_of_t r) *)
(*                (RS.string_of_t nl)) in        *)
(*     let s_len = RS.length s in  *)
(*     let nl_len = RS.length nl in  *)
(*     let r_expanded = R.expand r (RS.get s 0) nl in  *)
(*     let ct = r in  *)
(*     let rt = r_expanded in  *)
(*     let matches s c i =  *)
(*       let s_len = RS.length s in  *)
(*       let c_len = RS.length c in  *)
(*       let rec aux j =  *)
(*         if j=s_len then true *)
(*         else *)
(*           (i+j < c_len)  *)
(*           && ((RS.get c (i+j)) = (RS.get s j)) *)
(*           && (aux (succ j)) in  *)
(*         aux 0 in  *)
(*       { info = i; *)
(*         string = n; *)
(*         rtype = rt; *)
(*         ctype = ct; *)
(*         canonize =  *)
(*           (fun c ->  *)
(*              let c_len = RS.length c in  *)
(*              let buf = Buffer.create c_len in  *)
(*              let rec loop i =  *)
(*                if i = c_len then () *)
(*                else *)
(*                  if matches nl c i then  *)
(*                    (Buffer.add_string buf (RS.string_of_t s); *)
(*                     loop (i + nl_len)) *)
(*                  else  *)
(*                    (Buffer.add_string buf (RS.repr (RS.get c i)); *)
(*                     loop (succ i)) in  *)
(*                loop 0; *)
(*                RS.t_of_string (Buffer.contents buf)); *)

(*         choose =  *)
(*           (fun c ->  *)
(*              let c_len = RS.length c in  *)
(*              let buf = Buffer.create c_len in  *)
(*              let line_buf = Buffer.create k in  *)
(*              let aux_buf = Buffer.create k in  *)
(*              let do_line () =  *)
(*                if Buffer.length buf <> 0 && Buffer.length line_buf <> 0 then  *)
(*                  Buffer.add_string buf (RS.string_of_t nl); *)
(*                Buffer.add_buffer buf line_buf; *)
(*                Buffer.reset line_buf in  *)
(*              let do_space () =   *)
(*                if Buffer.length line_buf <> 0 then  *)
(*                  Buffer.add_string line_buf (RS.string_of_t s);             *)
(*                Buffer.add_buffer line_buf aux_buf; *)
(*                Buffer.reset aux_buf in  *)
(*              let rec loop i =                            *)
(*                let sum =                 *)
(*                  let nl_off = if Buffer.length buf=0 then 0 else pred nl_len in *)
(*                  let aux_len = Buffer.length aux_buf in  *)
(*                  let line_len = let n = Buffer.length line_buf in if n=0 then n else succ n in  *)
(*                    nl_off + aux_len + line_len in  *)
(*                  (\* Util.format "line_buf[%d] aux_buf[%d] Sum[%d] ~?~ k[%d] c[i]=%s@\n" *)
(*                     (Buffer.length line_buf) (Buffer.length aux_buf) sum k  *)
(*                     (if i=c_len then "NONE" else RS.chooser (RS.get c i)); *\) *)
(*                  if sum > k then  *)
(*                    do_line (); *)
(*                  if i = c_len then  *)
(*                    (do_space ();  *)
(*                     do_line ()) *)
(*                  else *)
(*                    let i' =  *)
(*                      if matches s c i then  *)
(*                        (do_space ();  *)
(*                         i + s_len) *)
(*                      else  *)
(*                        (Buffer.add_string aux_buf (RS.repr (RS.get c i)); *)
(*                         succ i) in  *)
(*                      loop i' in  *)
(*                loop 0; *)
(*                RS.t_of_string (Buffer.contents buf)) } *)
        
  let concat i cn1 cn2 = 
    let n = sprintf "concat (%s) (%s)" cn1.string cn2.string in 
    let () = match Brx.splittable_cex cn1.rtype cn2.rtype with 
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both cn1.rtype cn2.rtype over in 
          let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            cn1.string cn2.string s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let rt = Brx.mk_seq cn1.rtype cn2.rtype in 
    let ct = Brx.mk_seq cn1.ctype cn2.ctype in 
      { info = i;
        string = n;
        rtype = rt;
        ctype = ct;
        canonize = 
          (do_split (seq_split i cn1.rtype cn2.rtype)
             cn1.canonize cn2.canonize (^));
        choose = 
          (do_split ((split_one Int.Set.min_elt) cn1.ctype cn2.ctype)
             cn1.choose cn2.choose (^));
      }

  let union i cn1 cn2 = 
    let n = sprintf "union (%s) (%s)" cn1.string cn2.string in 
    let () = match Brx.disjoint_cex cn1.rtype cn2.rtype with
      | None -> ()
      | Some w -> 
          let s = sprintf "%s and %s are not disjoint: %s"
            (Brx.string_of_t cn1.rtype) (Brx.string_of_t cn2.rtype) w in
        Berror.static_error i n s in
    let rt = Brx.mk_alt cn1.rtype cn2.rtype in 
    let ct = Brx.mk_alt cn1.ctype cn2.ctype in 
      { info = i;
        string = n;
        rtype = rt;
        ctype = ct;
        canonize = (branch cn1.rtype cn1.canonize cn2.canonize);
        choose = (branch cn1.ctype cn1.choose cn2.choose);
      }

  let star i cn1 =
    let n = sprintf "(%s)*" cn1.string in
    let rt = Brx.mk_star cn1.rtype in
    let () = match Brx.iterable_cex cn1.rtype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both cn1.rtype rt over in 
          let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (Brx.string_of_t cn1.rtype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let ct = Brx.mk_star cn1.ctype in
      { info = i;
        string = n;
        rtype = rt;
        ctype = ct;
        canonize = (star_loop cn1.rtype cn1.canonize);      
        choose = 
          (fun cl -> 
             let rec loop cl acc = 
               if String.length cl = 0 then acc
               else
                 let cl1,clrest = (split_one Int.Set.min_elt) cn1.ctype ct cl in 
                   loop clrest (acc ^ (cn1.choose cl1)) in 
               loop cl "");
      }
        
  let iter i cn1 min maxo = 
    generic_iter i 
      (copy i Brx.epsilon) (union i) (concat i) (star i) 
      cn1 min maxo
end

(* simple sort checking for relations *)
type rel = Identity | Unknown
let combine_rel r1 r2 = match r1,r2 with 
  | Identity,Identity -> Identity 
  | _                 -> Unknown

(* -----------------------------------------------------------------------------*)
(* DICTIONARY LENSES *)
module DLens = struct    
  
  type dict = ((skeleton * dict) list KMap.t) TMap.t

  let empty_dict : dict = TMap.empty

  (* dictionariy types. At each level, we check that for each tag, the
     lens used to process matches tagged thus, has a unique uid *)
  type dict_type = uid TMap.t

  type t = 
      { (* --- meta data --- *)
        info: Info.t;                                       (* parsing info *)
        string: string;                                     (* pretty printer *)
        (* --- types --- *)                                 
        ctype: Brx.t;                                       (* concrete type *)
        atype: Brx.t;                                       (* abstract type *)
        xtype: Erx.t option;                                (* synchronization type *)
        dtype: dict_type;                                   (* dictionary type *)
        stype: skeleton -> bool;                            (* given a skeleton, returns if it is part
						               of the skeleton type of the lens*)
        crel: rel;                                          (* concrete equiv rel type *)
        arel: rel;                                          (* abstract equiv rel type *)
        (* --- core --- *)
        get: string -> string;                              (* get function *)
        put: string -> skeleton -> dict -> (string * dict); (* put function *)
        parse: string -> (skeleton * dict);                 (* parse function *)
        create: string -> dict -> (string * dict);          (* create function *)
        key: string -> string;                              (* key function *)
        (* --- hack --- *)
        uid: uid
      }

  (* lookup function for both types of dictionary types *)
  let rec std_lookup tag k d = 
    let km = try TMap.find tag d with Not_found -> KMap.empty in
      try match KMap.find k km with 
        | sd::l -> Some (sd, TMap.add tag (KMap.add k l km) d)
        | []    -> None
      with Not_found -> None

  let rec sim_lookup delta tag k d = 
    let distance s1 s2 = 
      let m = String.length s1 in 
      let n = String.length s2 in 
      let aodd = Array.make (succ n) 0 in 
      let aeven = Array.make (succ n) 0 in 
        for j=0 to n do aeven.(j) <- j done;
        for i=1 to m do
          let apredi = if i mod 2 = 0 then aodd else aeven in 
          let ai = if i mod 2 = 0 then aeven else aodd in 
            ai.(0) <- i;
            for j = 1 to n do      
              ai.(j) <-
                (min (apredi.(j) + 1)
                   (min (ai.(j-1) + 1)
                      (apredi.(j-1) + 
                       if String.get s1 (pred i) = String.get s2 (pred j) 
                       then 0 else 1)));
            done
        done;
        if m mod 2 = 0 then aeven.(n) else aodd.(n) in 
    let aux k1 k2 = 
      let di = distance k1 k2 in 
      let len = max (String.length k1) (String.length k2) in 
      let delta = float_of_int di /. float_of_int len in 
        (delta,di) in  
    let km = try TMap.find tag d with Not_found -> KMap.empty in
    let reso = try match KMap.find k km with 
      | sd::l -> Some (sd, TMap.add tag (KMap.add k l km) d)
      | []    -> None
    with Not_found -> None in 
      begin match reso with 
        | Some _ -> reso
        | None -> 
            begin
(*               Util.format "SEARCHING FOR %s @[" (RS.string_of_t tag); Berror.nlify (RS.string_of_t k); Util.format "@]@\n"; *)
              let reso = KMap.fold
                (fun ki li acco -> 
(*                    Util.format "TRYING @["; Berror.nlify (RS.string_of_t ki); Util.format "@]@\n"; *)
                   match li,acco with 
                     | [],_ | _,Some(0,_,_,_) -> acco
                     | sdi::li,Some(d',_,_,_) ->
                         let _,di = aux k ki in 
                           if di < d' then Some(di,ki,sdi,li)
                           else acco
                     | sdi::li,None -> 
                         let deltai,di = aux k ki in 
                           if deltai < delta then Some(di,ki,sdi,li) 
                           else None)
                km None in 
                Misc.map_option       
                  (fun (di,ki,sdi,li) -> 
                     (sdi,TMap.add tag (KMap.add ki li km) d))
                  reso
            end
      end  
  exception Incompatible_dict_type of tag
  let merge_dict_type dt1 dt2 = 
    TMap.fold 
      (fun t u acc ->
         if (not (TMap.mem t dt2)) || (TMap.find t dt2 = u) then
           TMap.add t u acc
         else raise (Incompatible_dict_type t)) dt1 dt2

  let safe_merge_dict_type i dt1 dt2 = 
    try merge_dict_type dt1 dt2 
    with
      | Incompatible_dict_type t -> 
	  raise (Error.Harmony_error 
		   (fun () -> 
		      Util.format "@[%s: type error in@\n" (Info.string_of_t i);
		      Util.format "The tag \"%s\" is used with different lenses@]@\n" t))

  (* helper: combine maps with merge. 
     Mapplus's combine is (combine fold find add (curry fst)) *)
  let combine fold find add merge m1 m2 = 
    fold (fun k v -> 
            add k (try merge v (find k m2) with Not_found -> v))
      m1 m2  

  (* smash two classic dictionaries *)
  let (++) d1 d2 =
    combine TMap.fold TMap.find TMap.add 
      (fun km1 km2 -> 
	 (combine KMap.fold KMap.find KMap.add 
	    (fun kl1 kl2 -> kl1 @ kl2)
	    km1 km2))
      d1 d2

  let split_dict d t = 
    try 
      let km = TMap.find t d in 
      (TMap.add t km empty_dict, TMap.remove t d)      
    with Not_found -> 
      (empty_dict,d)
          

  let info dl = dl.info
  let string dl = dl.string
  let ctype dl = dl.ctype
  let atype dl = dl.atype
  let xtype dl = dl.xtype
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
    
  let mk_t i s ct at xto dt st cr ar g put parse c k uid = 
    { info = i;
      string = s;
      ctype = ct;
      atype = at;
      xtype = xto;
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
  let rget dl = plift_r true dl.info dl.string dl.ctype 
    (fun c -> dl.get c)

  let rput dl = 
    plift_rr true dl.info dl.string dl.atype dl.ctype 
      (fun a c -> 
         let s,d = dl.parse c in
         fst (dl.put a s d))

  let rcreate dl = 
    plift_r true dl.info dl.string dl.atype
      (fun a -> fst (dl.create a empty_dict))
      
  let forgetkey dl = 
    { dl with
        key = (fun _ -> "");
        uid = next_uid();
    }
      
  let canonizer_of_t i dl = 
    Canonizer.mk_t 
      i
      (sprintf "canonizer_of_lens(%s)" dl.string)
      dl.ctype
      dl.atype
      dl.get
      (rcreate dl)
      
  (* invert -- only for bijective lenses! *)
  let invert i dl = 
    let n = sprintf "invert (%s)" dl.string in 
    let ct = dl.atype in
    let at = dl.ctype in 
      { dl with 
          info = i;
          string = n;
          ctype = ct;
          atype = at;
          get = rcreate dl;
          put = lift_rsd i (put_str n) at dl.stype (fun a _ d -> (dl.get a,d));
          parse = lift_r i (parse_str n) ct (fun c -> (S_string c, empty_dict));
          create = lift_rd i (create_str n) at (fun a d -> (dl.get a,d));
          key = lift_r i n at (fun a -> dl.key (dl.get a));
          uid = next_uid ();
      }
        
  (* ---------- copy ---------- *)
  let copy i r = 
    let n = sprintf "cp (%s)" (Brx.string_of_t r) in 
    let ct = r in 
    let at = r in
    let xto = Some (Erx.mk_leaf at) in 
    let dt = TMap.empty in
    let st = function
      | S_string s -> Brx.match_string r s
      | _ -> false in
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        xtype = xto;
        dtype = dt;
        stype = st;
        crel = Identity;
        arel = Identity;
        get = lift_r i (get_str n) ct (fun c -> c);
        put = lift_rsd i (put_str n) at st (fun a _ d -> (a,d));
        parse = lift_r i (parse_str n) ct (fun c -> (S_string c, empty_dict)); 
        create = lift_rd i (create_str n) at (fun a d -> a,d);
        key = lift_r i n at (fun _ -> "");
        uid = next_uid ();
      }

  let key i r = 
    let c = copy i r in
      { c with 
        xtype = Some (Erx.mk_key r);
        key = lift_r i c.string c.atype (fun a -> a) }

  (* ---------- const ---------- *)
  let const i r u def = 
    let n = sprintf "const (%s) \"%s\" \"%s\"" (Brx.string_of_t r) (whack u) (whack def) in 
    let ct = r in 
    let at = (Brx.mk_string u) in 
    let xto = Some (Erx.mk_leaf at) in 
    let dt = TMap.empty in
    let st = function
      | S_string s -> Brx.match_string r s
      | _ -> false in
    let () = 
      if not (Brx.match_string r def) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def (Brx.string_of_t r)) in 
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        xtype = xto;
	dtype = dt;
	stype = st;
        crel = Identity;
        arel = Identity;
        get = lift_r i (get_str n) ct (fun c -> u);
        put = lift_rsd i (put_str n) at st (fun _ s d -> (string_of_skel i s, d) );
	parse = lift_r i (parse_str n) ct (fun c -> (S_string c, empty_dict));  
        create = lift_rd i (create_str n) at (fun a d -> (def,d));
	key = lift_r i n at (fun _ -> "");
	uid = next_uid ();
      }

  (* ---------- concat ---------- *)
  let concat i dl1 dl2 = 
    let n = sprintf "%s . %s" dl1.string dl2.string in 
    let () = match Brx.splittable_cex dl1.ctype dl2.ctype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both dl1.ctype dl2.ctype over in 
          let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (Brx.string_of_t dl1.ctype) (Brx.string_of_t dl2.ctype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let () = match Brx.splittable_cex dl1.atype dl2.atype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both dl1.atype dl2.atype over in 
          let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (Brx.string_of_t dl1.atype) (Brx.string_of_t dl2.atype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let ct = Brx.mk_seq dl1.ctype dl2.ctype in 
    let at = Brx.mk_seq dl1.atype dl2.atype in 
    let xto = Misc.map2_option (fun x1 x2 -> Erx.mk_seq x1 x2) dl1.xtype dl2.xtype in
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in
    let st = function
      | S_concat (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
      (* lens *) 
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;
        xtype = xto;
	dtype = dt;
	stype = st;
        crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = lift_r i (get_str n) ct 
          (do_split (seq_split i dl1.ctype dl2.ctype)
             dl1.get dl2.get
             (^));
        put = lift_rsd i (put_str n) at st 
          (fun a s d -> 
             let a1,a2 = seq_split i dl1.atype dl2.atype a in 
	     let c1,d1 = dl1.put a1 (fst_concat_of_skel i s) d in
	     let c2,d2 = dl2.put a2 (snd_concat_of_skel i s) d1 in
	       (c1 ^ c2, d2));
	parse = lift_r i (parse_str n) ct 
          (fun c->
             let c1, c2 = seq_split i dl1.ctype dl2.ctype c in
	     let s1, d1 = dl1.parse c1 in
	     let s2, d2 = dl2.parse c2 in
	       (S_concat (s1, s2), d1 ++ d2));
	create = lift_rd i n at 
          (fun a d ->
             let a1, a2 = seq_split i dl1.atype dl2.atype a in
	     let c1, d1 = dl1.create a1 d in
	     let c2, d2 = dl2.create a2 d1 in
	       (c1 ^ c2, d2));
	key = lift_r i n at 
          (fun a ->
             let a1,a2 = seq_split i dl1.atype dl2.atype a in
	       (dl1.key a1) ^ (dl2.key a2));
	uid = next_uid ();}          

  let union i dl1 dl2 = 
    (* utilities *)
    let bare_get = branch dl1.ctype dl1.get dl2.get in
    let n = sprintf "(%s | %s)" dl1.string dl2.string in 
    let at = Brx.mk_alt dl1.atype dl2.atype in 
    let () = match Brx.disjoint_cex dl1.ctype dl2.ctype with 
      | None -> ()
      | Some w -> 
          let s = sprintf "%s and %s are not disjoint: %s"
            (Brx.string_of_t dl1.ctype) (Brx.string_of_t dl2.ctype) w in
        Berror.static_error i n s in
    let ct = Brx.mk_alt dl1.ctype dl2.ctype in 
    let xto = Misc.map2_option (fun x1 x2 -> Erx.mk_alt x1 x2) dl1.xtype dl2.xtype in
    (**** We still need to check equality of keys ***)
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in
    let st s = dl1.stype s || dl2.stype s in
      { info = i;
        string = n;
        ctype = ct; 
        atype = at;
        xtype = xto;
	dtype = dt;
	stype = st;
	crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = lift_r i (get_str n) ct bare_get;
        put = lift_rsd i (put_str n) at st 
          (fun a s d -> 
             match Brx.match_string dl1.atype a, 
               Brx.match_string dl2.atype a,
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

  let disjoint_union i dl1 dl2 = union i dl1 dl2

  let star i dl1 = 
    (* body *)
    let n = sprintf "(%s)*" dl1.string in
    let ct = Brx.mk_star dl1.ctype in
    let () = match Brx.iterable_cex dl1.ctype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both dl1.ctype ct over in 
          let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (Brx.string_of_t dl1.ctype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let at = Brx.mk_star dl1.atype in 
    let () = match Brx.iterable_cex dl1.atype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both dl1.atype at over in 
          let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
          (Brx.string_of_t dl1.atype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let xto = match dl1.xtype with 
      | None -> None 
      | Some xt -> 
          if Erx.boxes xt <> 0 && not (Erx.iterable xt) then None
          else Some (Erx.mk_star xt) in
    let dt = dl1.dtype in
    let st = function
      | S_star sl -> Safelist.fold_left (fun b s -> b && dl1.stype s) true sl
      | _ -> false in
      { info = i;
	string = n;
	ctype = ct;
	atype = at;
        xtype = xto;
	dtype = dt;
	stype = st;
        crel = dl1.crel;
        arel = dl1.arel;
        get = lift_r i (get_str n) ct (star_loop dl1.ctype dl1.get);
        put = lift_rsd i (put_str n) at st 
          (fun a s d -> 
	     let rec loop al sl buf d = match al,sl with
                 [],_ -> (buf, d)
               | a1::at,[] ->
		   let c1,d1 = dl1.create a1 d in
		     loop at [] (buf ^ c1) d1
               | a1::at,s1::st ->
		   let c1, d1 = dl1.put a1 s1 d in 
		     loop at st (buf ^ c1) d1 in 
               loop 
                 (Brx.star_split dl1.atype a)
                 (lst_of_skel i s)
	         ""
	         d);
	create = lift_rd i (create_str n) at 
          (fun a d -> 
             Safelist.fold_left 
               (fun (buf, d) a1 -> 
                  let c1,d' = dl1.create a1 d in 
                  let buf' = buf ^ c1 in
                  (buf', d'))
               ("", d) (Brx.star_split dl1.atype a));
	parse = lift_r i (parse_str n) ct 
          (fun c ->
             let (sl, d) = Safelist.fold_left 
               (fun (buf, d) c1 -> 
                  let s1,d1 = dl1.parse c1 in
	          let buf' = s1::buf  in
	          let d' = d ++ d1 in
	            (buf', d'))
               ([], empty_dict)
               (Brx.star_split dl1.ctype c) in
	       (S_star (Safelist.rev sl), d));
	key = lift_r i (key_str n) at (star_loop dl1.atype dl1.key);
	uid = next_uid ();
      }

  let iter i l1 min maxo = 
    generic_iter i 
      (copy i Brx.epsilon) (union i) (concat i) (star i) 
      l1 min maxo

  (* non-standard lenses *)
  let swap i dl1 dl2 = 
    let n = sprintf "swap (%s) (%s)" dl1.string dl2.string in 
    let () = match Brx.splittable_cex dl1.ctype dl2.ctype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both dl1.ctype dl2.ctype over in 
          let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (Brx.string_of_t dl1.ctype) (Brx.string_of_t dl2.ctype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let () = match Brx.splittable_cex dl2.atype dl1.atype with
      | None -> ()
      | Some over -> 
          let (s1,s2),(s3,s4) = split_both dl2.atype dl1.atype over in 
          let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (Brx.string_of_t dl2.atype) (Brx.string_of_t dl1.atype) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let ct = Brx.mk_seq dl1.ctype dl2.ctype in 
    let at = Brx.mk_seq dl2.atype dl1.atype in 
    let xto = Misc.map2_option (fun x2 x1 -> Erx.mk_seq x2 x1) dl2.xtype dl1.xtype in
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in
    let st = function
      | S_concat (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
      { info = i;
        string = n;
        ctype = ct; 
        atype = at;
        xtype = xto;
        dtype = dt;
        stype = st;
        crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = lift_r i n ct (fun c -> 
                               let c1,c2 = seq_split i dl1.ctype dl2.ctype c in 
                                 (dl2.get c2) ^ (dl1.get c1));
        put = lift_rsd i (put_str n) at st (fun a s d -> 
                                              let a2,a1 = seq_split i dl2.atype dl1.atype a in 
                                              let c2,d1 = dl2.put a2 (snd_concat_of_skel i s) d in 
                                              let c1,d2 = dl1.put a1 (fst_concat_of_skel i s) d1 in 
                                                (c1 ^ c2, d2));
        create = lift_rd i (create_str n) at (fun a d -> 
                                                let a2,a1 = seq_split i dl2.atype dl1.atype a in          
                                                let c2,d1 = dl2.create a2 d in 
                                                let c1,d2 = dl1.create a1 d1 in 
                                                  (c1 ^ c2, d2));
        parse = lift_r i (parse_str n) ct (fun c ->
                                             let c1,c2 = seq_split i dl1.ctype dl2.ctype c in 
                                             let s2,d2 = dl2.parse c2 in 
                                             let s1,d1 = dl1.parse c1 in 
                                               (S_concat (s1,s2), d2++d1)); 
        key = lift_r i n at (fun a ->
                               let a2, a1 = seq_split i dl2.atype dl1.atype a in
	                         (dl2.key a2) ^ (dl1.key a1));
        uid = next_uid ();
      }
        
  let compose i dl1 dl2 = 
    let n = sprintf "%s; %s" dl1.string dl2.string in 
    let ct = dl1.ctype in
    let at = dl2.atype in 
    let xto = dl2.xtype in 
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in
    let st = function
      | S_comp (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
      (match dl1.arel,dl2.crel with
         | Unknown,Unknown -> 
             let s = sprintf "the composition of %s and %s is ill-typed: %s"            
               dl1.string dl2.string 
               "the middle relations must both be the identity" in 
               Berror.static_error i n s
         | _ -> ());
      
      if not (Brx.equiv dl1.atype dl2.ctype) then
        begin
	  let s = sprintf "the composition of %s and %s is ill-typed"
	    dl1.string dl2.string in 
	  Berror.static_error i n s
        end;
        { info = i; 
          string = n;
          ctype = ct;      
          atype = at;
          xtype = xto;
          dtype = dt;
          stype = st;
          crel = dl1.crel;
          arel = dl2.arel;
          get = lift_r i (get_str n) ct (fun c -> dl2.get (dl1.get c));
          put = lift_rsd i n at st 
	    (fun a s d  ->
	       let s1,s2 = comp_of_skel i s in
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
    let n = sprintf "default %s %s" def dl1.string in 
    let at = dl1.atype in 
    let () = 
      if not (Brx.match_string dl1.ctype def) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def
             (Brx.string_of_t dl1.ctype)) in 
    let s,d = dl1.parse def in
      { dl1 with
          create = lift_rd i (create_str n) at (fun a d' -> dl1.put a s (d' ++ d)); 
          (* FINISH: think carefully about order of dictionary smashing here *)
	  uid = next_uid ();
      }

  let dmatch i lookup_fun tag dl1 = 
    let n = sprintf "<%s>" dl1.string in
    let ct = dl1.ctype in 
    let at = dl1.atype in 
    let st = function
      | S_box b -> tag = b
      | _ -> false in
    let xto = Misc.map_option (fun x1 -> Erx.mk_box tag x1) dl1.xtype in 
    let dt = TMap.add tag dl1.uid TMap.empty in
    let put_create a d = 
      let k = dl1.key a in 
      let d1,d2 = split_dict d tag in 
	match lookup_fun tag k d1 with
	  | Some((s',d1'),d1'') -> 
              let c',d' = dl1.put a s' (d1' ++ d2) in                      
              let _,d2' = split_dict d' tag in 
                (c',d1'' ++ d2')
	  | None       -> 
              let c',d' = dl1.create a d2 in 
              let _,d2' = split_dict d' tag in 
                (c',d1++d2') in 
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        xtype = xto;
        stype = st;
	dtype = dt;
        crel = dl1.crel;
        arel = dl1.arel;
        get = lift_r i (get_str n) ct (fun c -> dl1.get c);
        put = lift_rsd i (put_str n) at st 
          (fun a _ d -> put_create a d);
        create = lift_rx i (create_str n) at 
          (fun a d -> put_create a d);
        parse = lift_r i (parse_str n) ct 
          (fun c -> 
	     let s,d = dl1.parse c in
             let d1,d2 = split_dict d tag in 
             let k = dl1.key (dl1.get c) in
               (*           Util.format "PARSED [@["; Berror.nlify (RS.string_of_t c); Util.format "@]]"; *)
               (*           Util.format " = %s@\n" (RS.string_of_t k); *)
             let km = KMap.add k [(s,d1)] KMap.empty in 
	     let d' = TMap.add tag km d2 in 
	     (S_box tag,d'));
	key = dl1.key;
	uid = next_uid ();
      }

  let filter i rd rk =
    let n = sprintf "filter %s %s" (Brx.string_of_t rd) (Brx.string_of_t rk) in 
    let () = match Brx.disjoint_cex rd rk with
      | None -> ()
      | Some w -> 
          let s = sprintf "%s and %s are not disjoint: %s"
            (Brx.string_of_t rd) (Brx.string_of_t rk) w in
        Berror.static_error i n s in
    let ru = Brx.mk_alt rd rk in
    let ct = Brx.mk_star ru in 
    let () = 
      match Brx.iterable_cex ru with
        | None -> ()
        | Some over -> 
            let (s1,s2),(s3,s4) = split_both ru ct over in 
            let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (Brx.string_of_t ru) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let at = Brx.mk_star rk in 
    let xto = Some (Erx.mk_leaf at) in 
    let () = 
      match Brx.iterable_cex rk with
        | None -> ()
        | Some over -> 
            let (s1,s2),(s3,s4) = split_both rk at over in 
            let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (Brx.string_of_t rk) s1 s2 s3 s4 in 
          Berror.static_error i n s in 
    let dt = TMap.empty in
    let st = function
      | S_string s -> Brx.match_string ct s
      | _ -> false in
    let get = lift_r i (get_str n) ct 
      (fun c ->
         let rec loop acc = function 
           | [] -> acc
           | h :: t -> 
               if Brx.match_string rd h then 
                 loop acc t
               else 
                 loop (acc ^ h) t in
         let lc = Brx.star_split ct c in
           loop "" lc) in
    let put = lift_rsd i (put_str n) at st
      (fun a s d ->
         let c = string_of_skel i s in
         let rec loop acc lc la = match lc,la with
           | [], [] -> acc
           | [], ha :: ta -> loop (acc ^ ha) [] ta
           | hc :: tc, [] -> 
               if Brx.match_string rd hc then
                 loop (acc ^ hc) tc []
               else
                 loop acc tc []
           | hc :: tc, ha :: ta -> 
               if Brx.match_string rd hc then
                 loop (acc ^ hc) tc la
               else
                 loop (acc ^ ha) tc ta in
         let lc = Brx.star_split ct c in
         let la = Brx.star_split at a in
           (loop "" lc la, d)) in
    let create = lift_rd i n at (fun a d -> (a,d)) in
    let parse = lift_r i n ct (fun c -> (S_string c, empty_dict))in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;
        xtype = xto;
        dtype = dt;
	stype = st;
	crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create;
	parse = parse;
	key = lift_r i n at (fun _ -> "");
	uid = next_uid ();
      }


  (* left quotient of a dlens by a canonizer. I don't know if we
     should check if the dlens is not already a q-lens. *)

  let left_quot i cn dl = 
    let n = sprintf "left quotient of %s by %s" dl.string (Canonizer.string cn) in
      (* the "class type" of the canonizer has to be equal to the
	 concrete type of the lens *)
    if not (Brx.equiv (Canonizer.ctype cn) dl.ctype) then
      begin
	let s = sprintf "%s is ill-typed" n in
	  Berror.static_error i n s 
      end;
      let ct = Canonizer.rtype cn in
      let at = dl.atype in
      let xto = dl.xtype in 
      let dt = dl.dtype in
      let st = dl.stype in
      let canonize = Canonizer.canonize cn in
      let choose = Canonizer.choose cn in
        { info = i; 
          string = n;
          ctype = ct;
          atype = at;
          xtype = xto;
          dtype = dt;
          stype = st;
          crel = Unknown;
          arel = dl.arel;
          get = lift_r i (get_str n) ct 
	    (fun c -> dl.get(canonize c));
          put = lift_rsd i n at st 
	    (fun a s d ->
	       let cc, d = dl.put a s d in
	         (choose cc, d));
          create = lift_rd i n at
	    (fun a d ->
	       let cc, d = dl.create a d in
	         (choose cc, d));
          parse = lift_r i n ct
	    (fun c ->
	       let cc = canonize c in
	         dl.parse cc);
          key = dl.key;
          uid = next_uid();
        }

  let right_quot i dl cn = 
    let n = sprintf "right quotient of %s by %s" dl.string (Canonizer.string cn) in
    if not (Brx.equiv (Canonizer.ctype cn) dl.atype) then
      begin
	let s = sprintf "%s is ill-typed" n in
	  Berror.static_error i n s 
      end;
    let ct = dl.ctype in
    let at = Canonizer.rtype cn in
    let xto = dl.xtype in 
    let dt = dl.dtype in
    let st = dl.stype in
    let canonize = Canonizer.canonize cn in
    let choose = Canonizer.choose cn in
      { info = i; 
        string = n;
        ctype = ct;      
        atype = at;
        xtype = xto;
        dtype = dt;
        stype = st;
        crel = dl.crel;
        arel = Unknown;
        get = lift_r i (get_str n) ct 
	  (fun c -> choose (dl.get c));
        put = lift_rsd i n at st 
	  (fun a s d ->
	     let ac = canonize a in
	       dl.put ac s d);
        create = lift_rd i n at
	  (fun a d ->
	     let ac = canonize a in
	       dl.create ac d);
        parse = dl.parse;
        key = (fun a -> dl.key (canonize a));
        uid = next_uid ();
      }
end
