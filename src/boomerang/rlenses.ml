(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* lenses.ml : resourceful lenses                            *)
(*************************************************************)
(* $Id$ *)

(* abbreviations from other modules *)
module RS = Rstring
(* input and output*)
module I = Wic
module O = Woc
let string_concat = (^)
let (^) = RS.append
let (@) = Safelist.append
let sprintf = Printf.sprintf

let no_check = Prefs.createBool "no-check" false 
  "don't type check lens arguments" 
  "don't type check lens arguments"

let no_type_check = Prefs.createBool "no-type-check" false
  "don't type check concatenation, alternative and repetition of lenses. To be used with precaution !"
  "don't type check concatenation, alternative and repetition of lenses. To be used with precaution !"

(* -------------------- utilities -------------------- *)
let lst_replace = [("\n","\\n");("\t","\\t");("\"","\\\"");("\\", "\\\\")]
let lst_regexp_replace = Safelist.map (fun (p,r) -> (Str.regexp p, r)) lst_replace
let whack s =  
  Safelist.fold_right
    (fun (p, r) s -> Str.global_replace p r s)
    lst_regexp_replace s
    
(* format a string, converting newlines to @\n *)
let nlify s = Misc.format_list "@\n" 
  (Util.format "%s") 
  (Misc.split_nonescape '\n' s)

let nlify_str s = nlify (RS.to_string s)

(* split a string according to a regexp *)

let string_of_reps = function
  | (0,None) -> "*" 
  | (0,Some 1) -> "?"
  | (1,None) -> "+"
  | (i,None) -> sprintf "{%d,}" i
  | (i,Some j) -> if i=j then sprintf "{%d}" i else sprintf "{%d,%d}" i j



(* definitions of different ranks. 
 *)
type type_exp = 
  | Bexp (* bar *)
  | Mexp (* minus *)
  | Iexp (* inter *)
  | Sexp (* swap *)
  | Cexp (* concat *)
  | Rexp (* rep *)
  | Uexp (* unique. A string or an ident *)
      
(* need_par_left e1 e2 determines if you need to put parenthesis around exp of type e1 when you
 * create an expression of type e2 with e1 on the left
 *)
 
let need_par_left e1 e2 = match e1,e2 with
  | Uexp, _ -> false
  | _, Uexp -> assert false
  | Rexp, _ -> false
  | _, Rexp -> true
  | Cexp, _ -> false
  | _, Cexp -> true
  | Sexp, _ -> false
  | _, Sexp -> true
  | Iexp, _ -> false
  | _, Iexp -> true
  | Bexp, Mexp
  | Mexp, Bexp -> true
  | Mexp, Mexp -> false
  | Bexp, Bexp -> false

(* need_par_right e1 e2 determines if you need to put parenthesis around exp of type e1 when you
 * create an expression of type e2 with e1 on the right
 *)
 
let need_par_right e1 e2 = match e1,e2 with
  | Uexp, _ -> false
  | _, Uexp -> assert false
  | _, Rexp -> true
  | Rexp, _ -> false
  | _, Cexp -> true
  | Cexp, _ -> false
  | _, Sexp -> true
  | Sexp, _ -> false
  | _, Iexp -> true
  | Iexp, _ -> false
  | Bexp, Mexp
  | Mexp, Bexp -> true
  | Mexp, Mexp -> true
  | Bexp, Bexp -> true


(* -------------------- Erxs -------------------- *)
(* Erx.ts, extended with strings for printing, representatives *)
(* The "rank" field is for pretty printing purposes *)
type r = { str : string; rx : Erx.t; rank : type_exp }

let string_of_r r = r.str

let rx_equiv r1 r2 = 
  Erx.equiv r1.rx r2.rx

let rep r = Erx.representative r.rx 

let determinize r = {r with rx = Erx.determinize r.rx}

let id = fun x -> x

let check_rx_equiv r1 r2 =
  if rx_equiv r1 r2 then (true,id)
  else (false,
        fun() ->
          let printrep s1 s2 n1 n2 =
            try
              let v = Erx.representative (Erx.mk_diff s1.rx s2.rx) in
              Util.format "value \"%s\"@\nis in the %s but not the %s@ "
                (RS.to_string v) n1 n2
            with Not_found -> () in
          Util.format "%s <> %s@\n" r1.str r2.str;
          printrep r1 r2 "first" "second";
          printrep r2 r1 "second" "first"
       )
        
let concat_repr r1 r2 concat_symb rc =
  let s1' = if need_par_left r1.rank rc then sprintf "(%s)" r1.str else r1.str in
  let s2' = if need_par_right r2.rank rc then sprintf "(%s)" r2.str else r2.str in
  sprintf "%s %s %s" s1' concat_symb s2'
    

let rx_epsilon = 
  { str = "epsilon"; 
    rx = Erx.epsilon;
    rank = Uexp}

let rx_empty = 
  { str = "empty";
    rx = Erx.empty;
    rank = Uexp}

(* two functions for a quick test of equality.
 * to be used only for pretty printing *)
let is_rx_epsilon r =
  r.rx == Erx.epsilon

let is_rx_empty r = 
  r.rx == Erx.empty

let rx_box e = 
  let st = RS.repr e in
    { str = st;
      rx = Erx.mk_str false (RS.make 1 e);
      rank = Uexp
    }

let str_of_cl cl = 
  let buf = Buffer.create 17 in 
  let to_str fs = Char.escaped (Char.chr (RS.to_int fs)) in
    Safelist.iter 
      (fun (c1,c2) -> 
        if c1=c2 then 
          Buffer.add_string buf (to_str c1)
        else 
          (Buffer.add_string buf (to_str c1);
           Buffer.add_char buf '-';
           Buffer.add_string buf (to_str c2)))
      cl;
    Buffer.contents buf

let rx_set cl = 
  { str = sprintf "[%s]" (str_of_cl cl);
    rx = Erx.mk_cset true cl;
    rank = Uexp
  }

let rx_negset cl = 
  { str = sprintf "[^%s]" (str_of_cl cl);
    rx = Erx.mk_cset false cl;
    rank = Uexp
  }
  
let rx_str ignore_case s = 
  let s_string = String.escaped (RS.to_string s) in
    { str = sprintf "%s\"%s\"%s" 
        (if ignore_case then "ignore_case(" else "")
        (whack s_string)
        (if ignore_case then ")" else "");      
      rx = Erx.mk_str ignore_case s;
      rank = Uexp}

let rx_seq r1 r2 = 
  if is_rx_empty r1 || is_rx_empty r2 then rx_empty
  else if is_rx_epsilon r1 then r2 
  else if is_rx_epsilon r2 then r1
  else let str = concat_repr r1 r2 "." Cexp in
    { str = str; 
      rx = Erx.mk_seq r1.rx r2.rx;
      rank = Cexp}

let rx_alt r1 r2 = 
  if is_rx_empty r1 then r2
  else if is_rx_empty r2 then r1
  else 
  let add_qmark s r = if need_par_left r Rexp then sprintf "(%s)?" s else sprintf "%s?" s in
  let str = 
    if is_rx_epsilon r1 then add_qmark r2.str r2.rank 
    else if is_rx_epsilon r2 then add_qmark r1.str r1.rank
    else concat_repr r1 r2 "|" Bexp in
  { str = str; 
    rx = Erx.mk_alt r1.rx r2.rx;
    rank = Bexp
  }

let rx_set_str r1 s = 
  { r1 with str = s; rank = Uexp; }
      
(* XXX: if max < min, then Erx will raise an Invalid_argument exception *)
let rx_star r1 =
  let s1 = if need_par_left r1.rank Rexp then sprintf "(%s)*" r1.str else string_concat r1.str "*" in
  { str = s1;
    rx = Erx.mk_star r1.rx;
    rank = Rexp}

let rx_diff r1 r2 =
  if is_rx_empty r1 then rx_empty 
  else if is_rx_empty r2 then r1
  else let str = concat_repr r1 r2 "-" Mexp in
  let r = Erx.mk_diff r1.rx r2.rx in 
    { str = str;
      rx = r; 
      rank = Mexp}
      
let rx_inter r1 r2 =
  if is_rx_empty r1 || is_rx_empty r2 then rx_empty
  else let r = Erx.mk_inter r1.rx r2.rx in 
  let str = concat_repr r1 r2 "&" Iexp in
    { str = str;
      rx = r; 
      rank = Iexp} 
      
let rx_lowercase r1 = 
  { str = sprintf "lowercase(%s)" r1.str;
    rx = Erx.mk_lowercase r1.rx;
    rank = Uexp }

let rx_uppercase r1 = 
  { str = sprintf "uppercase(%s)" r1.str;
    rx = Erx.mk_uppercase r1.rx;
    rank = Uexp }
    
let get_str n = sprintf "%s get" n
let put_str n = sprintf "%s put" n
let create_str n = sprintf "%s create" n
let parse_str n = sprintf "%s parse" n
let cls_str n = sprintf "%s cls" n
let rep_str n = sprintf "%s rep" n
let key_str n = sprintf "%s key" n

let split_bad_prefix t s = 
  try 
    let is, approx = Erx.find_exit_automaton t.rx s in
    let i = Rint.Set.max_elt is in 
    let s1 = RS.sub s 0 i in 
    let s2 = RS.sub s i ((RS.length s) - i) in 
      (s1,s2,approx)
  with
    | Not_found -> (RS.empty, s,false)

(* type checking wrappers *)
let type_error i t s1 (s3l,s3r,approx) =
  raise (Error.Harmony_error (fun () -> 
    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
    Util.format "  T=@[%s@]@\n@\n" t.str;    
    Util.format "  @["; 
    nlify s1;
    Util.format "@]@\n@\n";
    Util.format "  [@["; 
    nlify_str s3l; 
    if approx then
      Util.format "@]]@\n<<AROUND HERE>>@\n  [@["
    else
      Util.format "@]]@\n<<HERE>>@\n  [@[";
    nlify_str s3r; 
    Util.format "@]]@]@\n"))

let split_error i t pos nf =
  raise (Error.Harmony_error (fun () -> 
    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
    Util.format "  Cannot find any string in @\nT=@[%s@]@\n@\n" t.str;    
    Util.format "  in the %s file at posistion %d@]" nf pos;))


let static_error i n ?(suppl =  id) msg = 
  raise (Error.Harmony_error(fun () -> 
    Util.format "@[%s: static error in@\n" (Info.string_of_t i);
    Util.format "  @["; 
    nlify n;
    Util.format "@]@\n@\n";
    Util.format "  [@["; 
    nlify msg; 
    suppl ();
    Util.format "@]]@\n"))
    
let lift_r i n t1 f =  
  (fun x ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else (f x))
    
let lift_rr i n t1 t2 f = (fun x y ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else if not (Erx.match_str t2.rx y) then 
      type_error i t2 n (split_bad_prefix t2 y)
    else (f x y))

let lift_rsd i n t1 st2 f = (fun x y z ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else if not (st2 y) then 
      assert false
    else (f x y z))


let lift_rd i n t1 f = 
  (fun x y ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else (f x y))

let lift_rrd i n t1 t2 t3 f = 
  (fun x y z ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else if not (Erx.match_str t2.rx y) then 
      type_error i t2 n (split_bad_prefix t2 y)
    else (f x y z))

let lift_rx i n t1 f = 
  (fun x y ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else (f x y))

let lift_rrx i n t1 t2 f = 
  (fun x y z ->     
    if not (Erx.match_str t1.rx x) then 
      type_error i t1 n (split_bad_prefix t1 x)
    else if not (Erx.match_str t2.rx y) then 
      type_error i t2 n (split_bad_prefix t1 y)
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
    Var(_,x) -> RS.to_string x
  | Const(_,s) -> sprintf "\"%s\"" (whack (RS.to_string s)) 

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
  match Erx.unambig_split t1.rx t2.rx s with
      None -> assert false
    | Some s1s2 -> s1s2 

let split_one choose t1 t2 s = 
  let n = RS.length s in 
  let i = choose (Erx.split_positions t1.rx t2.rx s) in 
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
    RS.empty (Erx.unambig_star_split t.rx x) 

let branch t f1 f2 = 
  (fun x -> 
    if Erx.match_str t.rx x then f1 x 
    else f2 x) 

let branch2 t f1 f2 = 
  (fun x y ->
     if Erx.match_str t.rx x then f1 x y 
     else f2 x y)

(* common stuff *)
let rx_unambig_seq i n r1 r2 = 
  if not (Prefs.read no_type_check) then( 
    match Erx.unambig_seq r1.rx r2.rx with
      |	Erx.NA_true rxseq -> 
	  let str = concat_repr r1 r2 "." Cexp in
	    { str = str; 
	      rx = rxseq;
	      rank = Cexp}
      | Erx.NA_false dss ->
          let (s1,s2),(s1',s2') = Erx.example_of_dss dss in 
            static_error i n 
              (sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" \"%s\" \nand\n\"%s\" \"%s\""
                 r1.str r2.str 
                 (RS.to_string s1) (RS.to_string s2) 
                 (RS.to_string s1') (RS.to_string s2')))
  else rx_seq r1 r2


(* to be modified. Need to add counter example *)
let rx_easy_seq i n r1 r2 = 
  if (not (Prefs.read no_type_check) && not (Erx.easy_seq r1.rx r2.rx)) then
    static_error i n 
      (sprintf "the concatenation of %s and %s is not easy.\n"
         r1.str r2.str);
  rx_seq r1 r2
    
let rx_unambig_star i n r1 = 
  if not (Prefs.read no_type_check) then( 
    match Erx.unambig_star r1.rx with
	Erx.NSA_true rxstar ->
	  let s1 = if need_par_left r1.rank Rexp then sprintf "(%s)*" r1.str else string_concat r1.str "*" in
	    { str = s1;
	      rx = rxstar;
	      rank = Rexp}
      | Erx.NSA_empty_word -> 
          static_error i n 
            (sprintf "the iteration of %s is ambiguous: \"\""
               r1.str)
      | Erx.NSA_false -> 
          static_error i n 
            (sprintf "the iteration of %s is ambiguous"
               r1.str)
      | Erx.NSA_false_ce dms -> 
          static_error i n 
            (sprintf "the iteration of %s is ambiguous: \n\"%s\""
               r1.str
               (RS.to_string (Erx.example_of_dms dms))))
  else rx_star r1

let rx_easy_star i n r1 = 
  if (not (Prefs.read no_type_check) && not (Erx.easy_star r1.rx)) then
    static_error i n 
      (sprintf "the iteration of %s is not easy."
         r1.str);
  rx_star r1



let rx_disjoint_alt i n t1 t2 = 
  if not (Prefs.read no_type_check) then( 
  let t1ut2 = Erx.mk_inter t1.rx t2.rx in 
    if not (Erx.is_empty t1ut2) then
      static_error i n 
        (sprintf "the intersection of %s and %s is non-empty: \"%s\""
            t1.str t2.str
            (RS.to_string (Erx.representative t1ut2)))
    else rx_alt t1 t2)
  else rx_alt t1 t2

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
        rtype : r;
        ctype : r;
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
    let rt = rx_unambig_seq i n cn1.rtype cn2.rtype in 
    let ct = rx_seq cn1.ctype cn2.ctype in 
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
    let rt = rx_disjoint_alt i n cn1.rtype cn2.rtype in 
    let ct = rx_alt cn1.ctype cn2.ctype in 
    { info = i;
      string = n;
      rtype = rt;
      ctype = ct;
      cls = lift_r i (cls_str n) rt (branch cn1.rtype cn1.cls cn2.cls);
      rep = lift_r i (rep_str n) ct (branch cn1.ctype cn1.rep cn2.rep);
    }

  let star i cn1 = 
    let n = sprintf "(%s)*" cn1.string in 
    let rt = rx_unambig_star i n cn1.rtype in 
    let ct = rx_star cn1.ctype in 
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

(* streaming dictionary *)
and sdict_entry = 
    { lens: t;
      pos_in_file: int;
      nbr_to_remove: int;
      nbr_left: int}

and sdict = 
  | SD_empty
  | SD of ((sdict_entry list) KMap.t) TMap.t
   
(* ddict stands for double dict. Used for default *)
and ddict = {provisory_dict:dict;
	     main_dict:dict;}

and dict = 
  | CDict of cdict
  | SDict of (Wic.t * sdict)
  | DDict of ddict

(* dictionaries type type. At each level, we need to check that for
   one tag, there is only one unique id for this tag *)

and dict_type = 
    uid TMap.t


and t = 
    { (* --- meta data --- *)
      info: Info.t;                           (* parsing info *)
      string: string;                         (* pretty printer *)
      (* --- types --- *)
      ctype: r;                               (* concrete type *)
      atype: r;                               (* abstract type *)
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


(* lookup function in dictionnaries of both type *)
let rec lookup tag k = function 
  | CDict CD_empty | SDict (_, SD_empty) -> None
  | CDict (CD d) ->
      (let km = try TMap.find tag d with Not_found -> KMap.empty in
       try 
       (match KMap.find k km with 
	  | c::kl -> Some (c, CDict (CD (TMap.add tag (KMap.add k kl km) d)))
	  | [] -> None)
       with Not_found -> None)
  | SDict (wic, (SD d)) ->
      (let km = try TMap.find tag d with Not_found -> KMap.empty in
       try 
       (match KMap.find k km with 
	  | c::kl -> 
	      (let wic' = Wic.seek_in wic c.pos_in_file in
	      match Erx.easy_split c.lens.ctype.rx wic' with
		| None, _ -> assert false
		| Some str, wic'' -> 
		    (match c.lens.parse str with
		       | _, CD_empty -> assert false
		       | _, CD cd ->
			   (try 
			      let l = KMap.find k (TMap.find tag cd) in
			      let res = Safelist.nth l c.nbr_to_remove in
			      let kl' = 
				if c.nbr_left = 1 
				then kl 
				else {c with nbr_to_remove = c.nbr_to_remove + 1; nbr_left = c.nbr_left + 1} :: kl in
				Some (res, SDict (wic', SD (TMap.add tag (KMap.add k kl' km) d)))
			    with
			      | Not_found -> assert false
			      | Failure "nth" -> assert false)))
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
		    Util.format "The tag \"%s\" is used twice with different lenses@]@\n" (RS.to_string t);))


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

(* smash two streaming dictionaries *)
let (+++) sd1 sd2 = match (sd1,sd2) with
  | SD_empty, SD_empty -> SD_empty
  | SD_empty, sd
  | sd, SD_empty -> sd
  | SD d1', SD d2' ->
      SD (combine TMap.fold TMap.find TMap.add 
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
       ctype = determinize dl.ctype; 
       atype = determinize dl.atype}

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
    let n = sprintf "cp (%s)" (r.str) in 
    let ct = r in 
    let at = r in
    let dt = TMap.empty in
    let st = function
      | S_string s -> Erx.match_str r.rx s
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
    let u = RS.to_string u_str in
    let def = RS.to_string def_str in
    let n = sprintf "const (%s) \"%s\" \"%s\"" (r.str) (whack u) (whack def) in 
    let ct = r in 
    let at = rx_str false u_str in
    let dt = TMap.empty in
    let st = function
      | S_string s -> Erx.match_str r.rx s
      | _ -> false in
    let () = 
      if not (Erx.match_str r.rx def_str) then 
        static_error i n 
          (sprintf "%s does not belong to %s" def r.str) in 
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


  (* ---------- qconst ---------- *)
  let qconst i rc ra u_str def_str =
    let u = RS.to_string u_str in
    let def = RS.to_string def_str in
    let n = sprintf "qconst (%s) (%s) \"%s\" \"%s\"" (rc.str) (ra.str) (whack u) (whack def) in 
    let ct = rc in 
    let at = ra in
    let dt = TMap.empty in
    let st = function
      | S_string s -> Erx.match_str rc.rx s
      | _ -> false in
    let () = 
      if not (Erx.match_str ra.rx u_str) then 
        static_error i n 
          (sprintf "%s does not belong to %s" def ra.str) in 
    let () = 
      if not (Erx.match_str rc.rx def_str) then 
        static_error i n 
          (sprintf "%s does not belong to %s" def rc.str) in 
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
    let ct = rx_unambig_seq i n dl1.ctype dl2.ctype in 
    let at = rx_unambig_seq i n dl1.atype dl2.atype in 
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
    let at = rx_alt dl1.atype dl2.atype in 
    let ct = rx_disjoint_alt i n  dl1.ctype dl2.ctype in 
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
          match Erx.match_str dl1.atype.rx a, 
            Erx.match_str dl2.atype.rx a,
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
    let ct = rx_unambig_star i n dl1.ctype in 
    let at = rx_unambig_star i n dl1.atype in 
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
              (Erx.unambig_star_split dl1.atype.rx a)
              (lst_of_skel s)
	      RS.empty
	      d);
	create = lift_rd i (create_str n) at (fun a d -> 
          Safelist.fold_left (fun (buf, d) a1 -> 
            let c1,d' = dl1.create a1 d in 
            let buf' = RS.append buf c1 in
              (buf', d'))
            (RS.empty, d) (Erx.unambig_star_split dl1.atype.rx a));
	parse = lift_r i (parse_str n) ct (fun c ->
          let (sl, d) = Safelist.fold_left (fun (buf, d) c1 -> 
            let s1,d1 = dl1.parse c1 in
	    let buf' = s1::buf  in
	    let d' = d ++ d1 in
	      (buf', d'))
            ([], CD_empty)
            (Erx.unambig_star_split dl1.ctype.rx c) in
	  (S_star (Safelist.rev sl), d));
	key = lift_r i (key_str n) at 
          (star_loop dl1.atype dl1.key);
	uid = next_uid ();

      }

  (* non-standard lenses *)
  let swap i dl1 dl2 = 
    let n = sprintf "swap (%s) (%s)" dl1.string dl2.string in 
    let at = rx_unambig_seq i n dl2.atype dl1.atype in 
    let ct = rx_unambig_seq i n dl1.ctype dl2.ctype in 
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
            static_error i n s);
    let equiv, f_suppl = check_rx_equiv dl1.atype dl2.ctype in
      if not equiv then
        begin
	  let s =(sprintf "the composition of %s and %s is ill-typed:"
		     dl1.string dl2.string)in
	    static_error i n ~suppl:f_suppl s 
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
    let n = sprintf "default %s %s" (RS.to_string def) dl1.string in 
    let at = dl1.atype in 
    let () = 
      if not (Erx.match_str dl1.ctype.rx def) then 
        static_error i n 
          (sprintf "%s does not belong to %s" (RS.to_string def) dl1.ctype.str) in 
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
    let n = sprintf "filter %s %s" rd.str rk.str in
    let ru = rx_disjoint_alt i n rd rk in
    let ct = rx_unambig_star i n ru in
    let at = rx_unambig_star i n rk in
    let dt = TMap.empty in
    let st = function
      | S_string s -> Erx.match_str ct.rx s
      | _ -> false in
    let get = lift_r i (get_str n) ct 
      (fun c ->
         let rec loop acc = function 
           | [] -> acc
           | h :: t -> 
               if Erx.match_str rd.rx h then 
                 loop acc t
               else 
                 loop (acc ^ h) t in
         let lc = Erx.unambig_star_split ct.rx c in
           loop RS.empty lc) in
    let put = lift_rsd i (put_str n) at st
      (fun a s d ->
         let c = string_of_skel s in
         let rec loop acc lc la = match lc,la with
           | [], [] -> acc
           | [], ha :: ta -> loop (acc ^ ha) [] ta
           | hc :: tc, [] -> 
               if Erx.match_str rd.rx hc then
                 loop (acc ^ hc) tc []
               else
                 loop acc tc []
           | hc :: tc, ha :: ta -> 
               if Erx.match_str rd.rx hc then
                 loop (acc ^ hc) tc la
               else
                 loop (acc ^ ha) tc ta in
         let lc = Erx.unambig_star_split ct.rx c in
         let la = Erx.unambig_star_split at.rx a in
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




end



(* -------------------- STREAMING LENSES -------------------- *)
module StLens = struct    

  module D = DLens
  open D
  let dcrx dl = dl.ctype.rx
  let darx dl = dl.atype.rx


  type slens_elt = 
      S_dl of DLens.t (* a dlens, as is *)
    | S_sdl of DLens.t (* a dlens, stared *)

  type t = 
      { (* --- meta data --- *)
        info : Info.t;                          (* parsing info *)
        string: string;                         (* pretty printer *)
	(* --- type --- *)
	ctype: r;
	atype: r;
	(* core *)
	list: slens_elt list;
	(* --- hack --- *)
	uid: uid
      }
  let of_list l i =
    let rec aux ct at dt l = 
      match (ct, at, dt, l) with
	| None, None, _,  [] -> (* the list was empty *)
	    static_error i "" "The list to build a streaming-lens should not be empty"
	| None, None, _, (i, S_dl dl)::t ->
	    let (ct', at', l') = aux (Some dl.D.ctype) (Some dl.D.atype) (dl.D.dtype) t in
	    (ct', at', (S_dl dl)::l')
	| None, None, _, (i, S_sdl dl)::t ->
	    let cts = rx_easy_star i "easy star of concrete" dl.D.ctype in
	    let ats = rx_easy_star i "easy star of abstract" dl.D.atype in
	    let (ct', at', l') = aux (Some cts) (Some ats) dl.D.dtype t in
	    (ct', at', (S_sdl dl)::l')
	| None, _, _, _
	| _, None, _, _ -> assert false
	| Some ct, Some at, _, [] -> (ct, at, [])
	| Some ct, Some at, dt, (i, S_dl dl)::t ->
	    let ctc = rx_easy_seq i "easy concat of concrete" ct dl.D.ctype in
	    let atc = rx_easy_seq i "easy concat of abstract"at dl.D.atype in
	    let (ct', at', l') = aux (Some ctc) (Some atc) (fusion_dict_type dt dl.dtype) t in
	    (ct', at', (S_dl dl)::l')
	| Some ct, Some at, dt, (i, S_sdl dl)::t ->
	    let cts = rx_easy_star i "easy star of concrete" dl.D.ctype in
	    let ats = rx_easy_star i "easy star of abstract" dl.D.atype in
	    let ctc = rx_easy_seq i "easy concat of concrete" ct cts in
	    let atc = rx_easy_seq i "easy concat of abstract"at ats in
	    let (ct', at', l') = aux (Some ctc) (Some atc) (fusion_dict_type dt dl.dtype) t in
	      (ct', at', (S_sdl dl)::l') in
    let (ct, at, list) = aux None None TMap.empty l in
    { info = i;
      string = "";
      ctype = ct;
      atype = at;
      list = list;
      uid = next_uid ()}


  let info stl = stl.info
  let string stl = stl.string
  let ctype stl = stl.ctype
  let atype stl = stl.atype
  let list stl = stl.list
  let uid stl = stl.uid

      
  let get wic woc stl =
    let rec aux wic woc = function
      | [] -> ()
      | S_dl dl :: t ->
	  (let pos = I.pos_file wic in
	     match Erx.easy_split (dcrx dl) wic with 
	       | None, _ -> split_error dl.D.info dl.D.ctype pos "concrete"
	       | Some c, wic' ->
		   let a = dl.get c in
		   let woc' = O.write_string woc (RS.to_string a) in
		     aux wic' woc' t)
      | S_sdl dl :: t as sl->
	  (match Erx.easy_split (dcrx dl) wic with 
	     | None, wic' -> aux wic' woc t
	     | Some c, wic' ->
		 let a = dl.get c in
		 let woc' = O.write_string woc (RS.to_string a) in
		   aux wic' woc' sl) in
    aux wic woc stl.list
	    

  let create wic woc stl = 
    let rec aux wic woc = function
      | [] -> ()
      | S_dl dl :: t ->
	  (let pos = I.pos_file wic in
	     match Erx.easy_split (darx dl) wic with 
	       | None, _ -> split_error dl.D.info dl.D.atype pos "abstract"
	       | Some a, wic' ->
		   let c, _ = dl.create a (CDict CD_empty) in
		   let woc' = O.write_string woc (RS.to_string c) in
		     aux wic' woc' t)
      | S_sdl dl :: t as sl->
	  (match Erx.easy_split (darx dl) wic with 
	     | None, wic' -> aux wic' woc t
	     | Some a, wic' ->
		 let c, _ = dl.create a (CDict CD_empty) in
		 let woc' = O.write_string woc (RS.to_string c) in
		   aux wic' woc' sl) in
    aux wic woc stl.list


  let parse wic stl =
    let sd_of_cd dl pos = function 
      | CD_empty -> SD_empty
      | CD cd ->
	  SD (TMap.fold (fun tag km acc ->
            let km' = KMap.fold (fun k l acc' ->
			let entry = {lens = dl;
				     pos_in_file = pos;
				     nbr_to_remove = 0;
				     nbr_left = Safelist.length l}in
			KMap.add k [entry] acc') km KMap.empty in
	    TMap.add tag km' acc) cd TMap.empty) in
    let rec aux dict wic = function
      | [] -> dict
      | S_dl dl :: t ->
	  (let pos = I.pos_file wic in
	   match Erx.easy_split (dcrx dl) wic with 
	     | None, _ -> split_error dl.D.info dl.D.atype pos "concrete"
	     | Some c, wic' ->
		 let _, cd = dl.parse c in
		 let sd = sd_of_cd dl pos cd in
		 aux (dict +++ sd) wic' t)
      | S_sdl dl :: t as sl ->
	  (let pos = I.pos_file wic in
	   match Erx.easy_split (dcrx dl) wic with 
	     | None, wic' -> aux dict wic' t
	     | Some c, wic' ->
		 let _, cd = dl.parse c in
		 let sd = sd_of_cd dl pos cd in
		 aux (dict +++ sd) wic' sl) in
    let sd = aux SD_empty wic stl.list in
    SDict (wic, sd)

  let put wicc wica woc dict stl = 
    (* function to create the rest of a star lens *)
    let rec end_star_a wica woc dl dict = 
      match Erx.easy_split (darx dl) wica with 
	| None, wica' -> (wica', woc, dict) 
	| Some a, wica' ->
	    let c, dict' = dl.create a dict in
	    let woc' = O.write_string woc (RS.to_string c) in
	      end_star_a wica' woc' dl dict' in
    let rec end_star_c wicc dl = (* to be improved *)
      match Erx.easy_split (dcrx dl) wicc with 
	| None, wicc' -> wicc' 
	| Some _, wicc' -> end_star_c wicc' dl in
    let rec aux wicc wica woc dict = function
      | [] -> ()
      | S_dl dl :: t ->
	  (let posc = I.pos_file wicc in
	   let posa = I.pos_file wica in
	     match Erx.easy_split (dcrx dl) wicc, Erx.easy_split (darx dl) wica with 
	       | (None,_), _ -> split_error dl.D.info dl.D.ctype posc "concrete"
	       | _, (None,_) -> split_error dl.D.info dl.D.atype posa "abstract"
	       | (Some c, wicc'),(Some a, wica') ->
		   let s, _ = dl.parse c in
		   let c', dict'  = dl.put a s dict in
		   let woc' = O.write_string woc (RS.to_string c') in
		     aux wicc' wica' woc' dict' t)
      | S_sdl dl :: t as sl->
	  (match Erx.easy_split (dcrx dl) wicc, Erx.easy_split (darx dl) wica with 
	     | (None, wicc'), (None, wica') -> aux wicc' wica' woc dict t (*nothing left*)
	     | (None, wicc'), (Some a, wica') -> (* no more concrete, go for creation *)
		 let c, dict' = dl.create a dict in
		 let woc' = O.write_string woc (RS.to_string c) in
		 let (wica'', woc'', dict'') = end_star_a wica' woc' dl dict' in
		   aux wicc' wica'' woc'' dict'' t
	     | (Some _, wicc'), (None, wica') -> (* no more abstract, go for the next lens *)
		 let wicc'' = end_star_c wicc' dl in
		   aux wicc'' wica' woc dict t 
	     | (Some c, wicc'), (Some a, wica') ->
		 let s, _ = dl.parse c in
		 let c', dict' = dl.put a s dict  in
		 let woc' = O.write_string woc (RS.to_string c') in
		   aux wicc' wica' woc' dict' sl) in
      aux wicc wica woc dict stl.list

   
end
