(*************************************************************)
(* The Harmony Project                                       *)
(* harmony@lists.seas.upenn.edu                              *)
(*                                                           *)
(* lenses.ml : resourceful lenses                            *)
(*************************************************************)
(* $Id$ *)

(* abbreviations from other modules *)
module RS = Rstring
let (^) = RS.append
let (@) = Safelist.append
let sprintf = Printf.sprintf

let no_check = Prefs.createBool "no-check" false 
  "don't type check lens arguments" 
  "don't type check lens arguments"

let no_type_check = Prefs.createBool "no-type-check" false
  "don't type check concatenation, alternative and repetition of lenses. To be used with precotion !"
  "don't type check concatenation, alternative and repetition of lenses. To be used with precotion !"

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
let rx_rep r1 (min,maxo) =
  let s1 = if need_par_left r1.rank Rexp then sprintf "(%s)" r1.str else r1.str in
  { str = sprintf "%s%s" s1 (string_of_reps (min,maxo));
    rx = Erx.mk_rep min maxo r1.rx;
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
  | S_string of string
  | S_concat of (skeleton * skeleton)
  | S_star of skeleton list
  | S_box of tag
  | S_compose of (skeleton * skeleton)

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

(* dictionaries *)
type dict = 
  | D_empty
  | D of (((skeleton * dict) list) KMap.t) TMap.t

(* dictionaries type type. At each level, we need to check that for
   one tag, there is only one unique id for this tag *)
type uid = int
let current_uid = ref 0
let next_uid () = 
  incr current_uid;
  !current_uid

type dict_type = 
    uid TMap.t

exception Incompatilbe_dict_type of tag
let fusion_dict_type dt1 dt2 = 
  TMap.fold 
    (fun t u acc ->
       if !(TMap.mem t dt2) || (TMap.find t dt2 = u) then
	 TMap.add t u acc
       else raise Incompatilbe_dict_type t) dt1 dt2

let safe_fusion_dict_type dt1 dt2 i = 
  try 
    fusion_dict_type dt1 dt2 
  with
    | Incompatilbe_dict_type t -> 
	raise (Error.Harmony_error 
		 (fun () -> 
		    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
		    Util.format "The taf %s is used twice with different lenses@]@\n" RS.to_string t;))


(*
let format_dict d = 
  Util.format "{";
  ignore (SMap.fold (fun t km is_fst -> 
    Util.format "%s%s={" (if is_fst then "" else ", ") (whack (RS.to_string t));
    ignore (KMap.fold (fun ki cl is_fst -> 
      Util.format "%s%s=[" (if is_fst then "" else ", ") (whack (RS.to_string ki));
      ignore (Safelist.fold_left (fun b cj -> Util.format "%s%s" (if b then "" else ",") (RS.to_string cj); false) true cl);
      Util.format "]";
      false) 
      km true);
    Util.format "}";
    false)
    d true);
  Util.format "}"
*)



(* helper: combine maps with merge. 
   Mapplus's combine is (combine fold find add (curry fst)) *)
let combine fold find add merge m1 m2 = 
  fold (fun k v -> 
    add k (try merge v (find k m2) with Not_found -> v))
    m1 m2  

(* smash two dictionaries *)
let (++) d1 d2 = 
  combine TMap.fold TMap.find TMap.add 
    (fun km1 km2 -> 
      (combine KMap.fold KMap.find KMap.add 
          (fun kl1 kl2 -> kl1 @ kl2)
          km1 km2))
    d1 d2

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
let chk_concat i n t1 t2 = 
  if not (Prefs.read no_type_check) then( 
  match Erx.unambig_seq t1.rx t2.rx with
      Erx.NA_true -> ()
    | Erx.NA_false dss ->
        let (s1,s2),(s1',s2') = Erx.example_of_dss dss in 
          static_error i n 
            (sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" \"%s\" \nand\n\"%s\" \"%s\""
                t1.str t2.str 
                (RS.to_string s1) (RS.to_string s2) 
                (RS.to_string s1') (RS.to_string s2')))

let chk_iterate i n t1 min maxo = 
  if not (Prefs.read no_type_check) then( 
  match Erx.unambig_rep t1.rx min maxo with
      Erx.NSA_true -> ()
    | Erx.NSA_empty_word -> 
        static_error i n 
          (sprintf "the iteration of %s is ambiguous: \"\""
              t1.str)
    | Erx.NSA_false -> 
        static_error i n 
          (sprintf "the iteration of %s is ambiguous"
              t1.str)
    | Erx.NSA_false_ce dms -> 
        static_error i n 
          (sprintf "the iteration of %s is ambiguous: \n\"%s\""
              t1.str
              (RS.to_string (Erx.example_of_dms dms))))
        
let chk_disjoint i n t1 t2 = 
  if not (Prefs.read no_type_check) then( 
  let t1ut2 = Erx.mk_inter t1.rx t2.rx in 
    if not (Erx.is_empty t1ut2) then
      static_error i n 
        (sprintf "the intersection of %s and %s is non-empty: \"%s\""
            t1.str t2.str
            (RS.to_string (Erx.representative t1ut2))))


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
    let rt = rx_seq cn1.rtype cn2.rtype in 
    let ct = rx_seq cn1.ctype cn2.ctype in 
    chk_concat i n cn1.rtype cn2.rtype;
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
    let rt = rx_alt cn1.rtype cn2.rtype in 
    let ct = rx_alt cn1.ctype cn2.ctype in 
    chk_disjoint i n cn1.rtype cn2.rtype;
    { info = i;
      string = n;
      rtype = rt;
      ctype = ct;
      cls = lift_r i (cls_str n) rt (branch cn1.rtype cn1.cls cn2.cls);
      rep = lift_r i (rep_str n) ct (branch cn1.ctype cn1.rep cn2.rep);
    }

  let star i cn1 = 
    let n = sprintf "(%s)*" cn1.string in 
    let min = 0 in
    let maxo = None in
    let reps = (min,maxo) in 
    let rt = rx_rep cn1.rtype reps in 
    let ct = rx_rep cn1.ctype reps in 
      chk_iterate i n cn1.rtype min maxo;      
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
  type t = 
      { (* --- meta data --- *)
        info : Info.t;                          (* parsing info *)
        string: string;                         (* pretty printer *)
        (* --- types --- *)
        ctype : r;                              (* concrete type *)
        atype : r;                              (* abstract type *)
        dtype : dict_type;                      (* dictionary type *)
	stype : skeleton -> bool                (* given a skeleton, returns if it is part
						   of the skeleton type of the lens*)
        crel: rel;                              (* concrete equiv rel type *)
        arel: rel;                              (* abstract equiv rel type *)
        (* --- core --- *)
        get: RS.t -> RS.t;                      (* get function *)
        put: RS.t -> skeleton -> dict -> (RS.t, dict);  (* put function *)
	parse: RS.t -> (skeleton * dict)        (* parse function *)
        create: RS.t -> dict -> (RS.t, dict)    (* create function *)
	(* --- hack --- *)
        uid: uid
      }

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
  let uid dl = dl.uid


  let mk_t i s ct at dt st cr ar g put parse c uid = 
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
      uid = uid;
    }

  let determinize_clens cl =
    {cl with 
       ctype = determinize cl.ctype; 
       atype = determinize cl.atype}

  (* ---------- copy ---------- *)
  let copy i r = 
    let n = sprintf "cp (%s)" (r.str) in 
    let ct = r in 
    let at = r in
    let dt = TMap.empty in
    let st = function
      | S_string s -> Erx.match_str r.rx
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
        parse = lift_r i (parse_str n) ct (fun c -> (S_string c, Empty_Dict)); 
	create = lift_rd i (create_str n) at (fun a d -> a,d);
	uid = next_uid ();
      }

  (* ---------- const ---------- *)
  let const i r u_str def_str =
    let u = RS.to_string u_str in
    let def = RS.to_string def_str in
    let n = sprintf "const (%s) \"%s\" \"%s\"" (r.str) (whack u) (whack def) in 
    let ct = r in 
    let at = rx_str false u_str in
    let dt = TMap.empty in
    let st = function
      | S_string s -> Erx.match_str r.rx
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
	parse = lift_r i (parse_str n) ct (fun c -> (S_string c, Empty_Dict));  
        create = lift_rd i (create_str n) at (fun a d -> (def_str,d));
	uid = next_uid ();
      }

  (* ---------- concat ---------- *)
  let concat i dl1 dl2 = 
    let n = sprintf "%s . %s" dl1.string dl2.string in 
    let ct = rx_seq dl1.ctype dl2.ctype in 
    let at = rx_seq dl1.atype dl2.atype in 
    (* type checking *)
    chk_concat i n dl1.ctype dl2.ctype;
    chk_concat i n dl1.atype dl2.atype;
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
	uid = next_uid ();}          

  let union i dl1 dl2 = 
    (* utilities *)
    let bare_get = branch dl1.ctype dl1.get dl2.get in
    let n = sprintf "(%s|%s)" dl1.string dl2.string in 
    let at = rx_alt dl1.atype dl2.atype in 
    let ct = rx_alt dl1.ctype dl2.ctype in 
    chk_disjoint i n dl1.ctype dl2.ctype;
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
	uid = next_uid ();
      }

  let star i dl1 = 
    (* body *)
    let min = 0 in
    let maxo = None in
    let reps = (min,maxo) in 
    let ct = rx_rep dl1.ctype reps in 
    let at = rx_rep dl1.atype reps in 
    let dt = dl1.dtype in
    let st = function
      | S_star sl -> Safelist.fold_left (fun b s -> b && dl1.stype s) true sl
      | _ -> false in
    let n = sprintf "(%s)%s" dl1.string (string_of_reps reps) in
      chk_iterate i n dl1.ctype min maxo;
      chk_iterate i n dl1.atype min maxo;
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
	dtype = dt;
	stype = st;
        crel = dl1.crel;
        arel = dl1.arel;
        get = lift_r i (get_str n) ct (star_loop dl1.ctype dl1.get);
        put = lift_rsd i (put_str n) at ct (fun a s d -> 
	  let rec loop al kl buf = match al,kl with
              [],_ -> buf 
            | a1::at,[] -> 
	        let buf' = buf ^ (dl1.create a1) in
		  loop at [] buf'
            | a1::at,c1::ct -> 
                let buf' = buf ^ (dl1.put a1 c1) in
		  loop at ct buf' in 
            loop 
              (Erx.unambig_star_split dl1.atype.rx a)
              (Erx.unambig_star_split dl1.ctype.rx c)
	      RS.empty);
        create = lift_r i (create_str n) at 
          (star_loop dl1.atype dl1.create); 
      }

  let probe i tag dl1 = 
    let n = sprintf "probe \"%s\" (%s)" (whack tag) dl1.string in 
      { dl1 with 
        info = i;
        string = n;
        get = (fun c -> 
          Util.format "%s GET:@\n  [@[" tag;
          nlify_str c;
          Util.format "@]]@\n --> ";
          let a = dl1.get c in 
            Util.format "[@[";
            nlify_str a;
            Util.format "@]]@\n";
            a);
        put = (fun a c ->
          Util.format "%s PUT:@\n  [@[" tag;
          nlify_str a;
          Util.format "@]]@\n  [@[";
          nlify_str c;
          Util.format "@]]@\n --> ";
          let c' = dl1.put a c in             
            Util.format "[@[";
            nlify_str c';
            Util.format "@]]@\n";
            c');
        create = (fun a -> 
          Util.format "%s CREATE:@\n  [@[" tag;
          nlify_str a;
          Util.format "@]]@\n --> ";
          let c' = dl1.create a in             
            Util.format "[@[";
            nlify_str c';
            Util.format "@]]@\n";
            c'); }
        
  (* non-standard lenses *)
  let swap i dl1 dl2 = 
    let at = rx_seq dl2.atype dl1.atype in 
    let ct = rx_seq dl1.ctype dl2.ctype in  
    let n = sprintf "swap (%s) (%s)" dl1.string dl2.string in 
      (* type check *)
      chk_concat i n dl1.ctype dl2.ctype;
      chk_concat i n dl2.atype dl1.atype;
      { info = i;
        string = n;
        ctype = ct; 
        atype = at;
        crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = lift_r i n ct (fun c -> 
            let c1,c2 = split dl1.ctype dl2.ctype c in 
              (dl2.get c2) ^ (dl1.get c1));
        put = lift_rr i (put_str n) at ct (fun a c -> 
          let a2,a1 = split dl2.atype dl1.atype a in 
          let c1,c2 = split dl1.ctype dl2.ctype c in 
          let c2' = dl2.put a2 c2 in 
          let c1' = dl1.put a1 c1 in 
            (c1' ^ c2'));
        create = lift_r i (create_str n) at (fun a -> 
          let a2,a1 = split dl2.atype dl1.atype a in          
          let c2' = dl2.create a2 in 
          let c1' = dl1.create a1 in 
            (c1' ^ c2')); }
        
  let compose i dl1 dl2 = 
    let n = sprintf "%s; %s" dl1.string dl2.string in 
    let ct = dl1.ctype in
    let at = dl2.atype in 
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
      crel = dl1.crel;
      arel = dl2.arel;
      get = lift_r i (get_str n) ct (fun c -> dl2.get (dl1.get c));
      put = lift_rr i n at ct (fun a c -> dl1.put (dl2.put a (dl1.get c)) c);
      create = lift_r i n at (fun a -> dl1.create (dl2.create a));
    }

  let default i def dl1 = 
    let n = sprintf "default %s %s" (RS.to_string def) dl1.string in 
    let at = dl1.atype in 
      { dl1 with
        create = lift_r i (create_str n) at (fun a -> 
          dl1.put a def);
      }

  let filter i rd rk =
    let n = sprintf "filter %s %s" rd.str rk.str in
    chk_disjoint i n rd rk;
    let ru = rx_alt rd rk in
    chk_iterate i n ru 0 None;
    chk_iterate i n rk 0 None;
    let ct = rx_rep ru (0,None) in
    let at = rx_rep rk (0,None) in
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
    let put = lift_rr i (put_str n) at ct
      (fun a c ->
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
	   loop RS.empty lc la) in
    let create = lift_r i n at (fun a -> a) in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;
        crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create
      }

  (* move_end i ra rb defines a lense from A*BA* to A*B that moves the element in B at the end *)
  let move_end i ra rb =  
    let n = sprintf "move_end %s %s" ra.str rb.str in
    chk_iterate i n ra 0 None;
    let ras = rx_rep ra (0, None) in
    chk_concat i n rb ras;
    let rb_ras = rx_seq rb ras in
    chk_concat i n ras rb_ras;
    let ras_rb_ras = rx_seq ras rb_ras in
    let ct = ras_rb_ras in
    let ras_rb = rx_seq ras rb in
    let at = ras_rb in
    let get = lift_r i (get_str n) ct
      (fun c -> match Erx.unambig_split ras.rx rb_ras.rx c with
       | None -> assert false
       | Some (c1, c23) ->
	   (match Erx.unambig_split rb.rx ras.rx c23 with
	    | None -> assert false
	    | Some (c2, c3) -> c1 ^ c3 ^ c2)) in
    let put = lift_rr i (put_str n) at ct
      (fun a c ->  
	 let n_before = 
	  match Erx.unambig_split ras.rx rb_ras.rx c with
	  | None -> assert false
	  | Some (c1, _) -> Erx.count_unambig_star_split ra.rx c1 in
	 let a1,a2 = match Erx.unambig_split ras.rx rb.rx a with None -> assert false | Some r -> r in
	 (* the result of the putback has the size of the abstract *)
	 let n = RS.length a in 
	 let res = RS.make n (RS.of_int 0) in
	 let rec loop_concat dstoff =  function
	   | [] -> ()
	   | h::t -> 
	       let nh = RS.length h in
		 RS.blit h 0 res dstoff nh;
		 loop_concat (dstoff + nh) t in
	 let rec loop dstoff count l = match (count, l) with 
	   | 0, l -> 
	       let n2 = RS.length a2 in
		 RS.blit a2 0 res dstoff n2;
		 loop_concat (dstoff + n2) l
	   | _, [] ->
	       let n2 = RS.length a2 in
		 RS.blit a2 0 res dstoff n2
	   | count, h :: t ->
	       let nh = RS.length h in
		 RS.blit h 0 res dstoff nh;
		 loop (dstoff + nh) (pred count) t in
	 loop 0 n_before (Erx.unambig_star_split ra.rx a1);
	 res) in
    let create = lift_r i n at (fun a -> a) in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;                 
        crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create
      }
	   
  (* order i ra rb defines a lense from (A|B)* to A*B* that moves the elements in B at the end *)
  let order i ra rb =  
    let n = sprintf "order %s %s" ra.str rb.str in
    chk_disjoint i n ra rb;
    let raob = rx_alt ra rb in
    chk_iterate i n raob 0 None;
    let raobs = rx_rep raob (0, None) in
    let ct = raobs in
      (* repetitions of ra and rb are already checked by repetition of ra|rb*)
    let ras = rx_rep ra (0, None) in
    let rbs = rx_rep rb (0, None) in
    let at = rx_seq ras rbs in
    let get = lift_r i (get_str n) ct
      (fun c -> 
	 let rec loop acca accb = function
	   | [] -> acca ^accb
	   | h :: t -> 
	       if Erx.match_str ra.rx h 
	       then loop (acca ^ h) accb t 
	       else loop acca (accb ^ h) t in
	   loop RS.empty RS.empty (Erx.unambig_star_split raob.rx c))in
    let put = lift_rr i (put_str n) at ct
      (fun a c -> 
	 let concat_list l = 
	   (Safelist.fold_left (^) RS.empty l) in
	 let rec loop acc dl aasl absl = match (dl, aasl, absl) with
	   | [], aasl, absl -> acc ^ (concat_list aasl) ^ (concat_list absl)
	   | _, [], absl -> acc ^ (concat_list absl)
	   | _ , aasl, [] -> acc ^ (concat_list aasl)
	   | hc :: tc, haa :: taa, hab :: tab ->
	       if Erx.match_str ra.rx hc
	       then loop (acc ^ haa) tc taa absl
	       else loop (acc ^ hab) tc aasl tab in
	 let aas, abs = match Erx.unambig_split ras.rx rbs.rx a with Some r -> r | None -> assert false in
	 let aasl = Erx.unambig_star_split ra.rx aas in
	 let absl = Erx.unambig_star_split rb.rx abs in
	 let dl = Erx.unambig_star_split raob.rx c in
	   loop RS.empty dl aasl absl) in
    let create = lift_r i n at (fun a -> a) in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;                 
        crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create
      }

  let lowercase i r =
    let n = sprintf "lowercase(%s)" r.str in
    let ct = r in 
    let at = rx_lowercase r in 
    let () = 
      if not (Erx.is_empty (Erx.mk_diff at.rx ct.rx)) then 
        static_error i n 
          (sprintf "%s does not contain %s" ct.str at.str) in       
    let get = lift_r i (get_str n) ct 
      (fun c -> 
          for i=0 to pred (RS.length c) do
            let ci' = RS.lowercase (RS.get c i) in 
              RS.set c i ci'
          done;
          c) in 
    let put = lift_rr i (put_str n) at ct
      (fun a c -> 
        let m = RS.length c in 
        let n = RS.length a in 
          (* construct list of booleans from c; ith element shows
             capitalization of ith alphabetic char *)
        let rec loop1 i acc = 
          if i=m then Safelist.rev acc
          else
            let ci = RS.get c i in 
            loop1 (succ i) (if RS.is_alpha ci then RS.is_upper ci::acc else acc) in 
        let rec loop2 i c_uppers =           
          if i < n then 
            let ai = RS.get a i in               
              if not (RS.is_alpha ai) then 
                loop2 (succ i) c_uppers
              else match c_uppers with                  
                | b::t -> 
                    if b then RS.set a i (RS.uppercase ai);
                    loop2 (succ i) (if t=[] then c_uppers else t)
                | [] -> loop2 (succ i) [] in 
          loop2 0 (loop1 0 []);
          a) in 
    let create = lift_r i n at (fun a -> a) in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;                 
        crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create
      }	

  let uppercase i r =
    let n = sprintf "uppercase(%s)" r.str in
    let ct = r in 
    let at = rx_uppercase r in 
    let () = 
      if not (Erx.is_empty (Erx.mk_diff at.rx ct.rx)) then 
        static_error i n 
          (sprintf "%s does not contain %s" ct.str at.str) in       
    let get = lift_r i (get_str n) ct 
      (fun c -> 
          for i=0 to pred (RS.length c) do
            let ci' = RS.uppercase (RS.get c i) in 
              RS.set c i ci'
          done;
          c) in 
    let put = lift_rr i (put_str n) at ct
      (fun a c -> 
        let m = RS.length c in 
        let n = RS.length a in 
          (* construct list of booleans from c; ith element shows
             capitalization of ith alphabetic char *)
        let rec loop1 i acc = 
          if i=m then Safelist.rev acc
          else
            let ci = RS.get c i in 
            loop1 (succ i) (if RS.is_alpha ci then RS.is_lower ci::acc else acc) in 
        let rec loop2 i c_lowers =           
          if i < n then 
            let ai = RS.get a i in               
              if not (RS.is_alpha ai) then 
                loop2 (succ i) c_lowers
              else match c_lowers with                  
                | b::t -> 
                    if b then RS.set a i (RS.lowercase ai);
                    loop2 (succ i) (if t=[] then c_lowers else t)
                | [] -> loop2 (succ i) [] in 
          loop2 0 (loop1 0 []);
          a) in 
    let create = lift_r i n at (fun a -> a) in
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;                 
        crel = Identity;
        arel = Identity;
        get = get;
        put = put;
        create = create
      }	

  (* Q-lens stuff *)
  let canonizer_of_t dl1 = 
    Canonizer.mk_t 
      dl1.info
      (sprintf "canonizer_of_lens(%s)" dl1.string)
      dl1.ctype
      dl1.atype
      dl1.get
      dl1.create

  let quotient_c i cn1 dl1 = 
    let n = sprintf "qc (%s) (%s)" (Canonizer.string cn1) dl1.string in 
    let ct = Canonizer.rtype cn1 in 
    let at = dl1.atype in 
    let cn_ctype = Canonizer.ctype cn1 in 
    let dls = Canonizer.dls cn1 in 
    let rep = Canonizer.rep cn1 in 
    let equiv, f_suppl = check_rx_equiv cn_ctype dl1.ctype in
      if not equiv then
        begin
	  let s =sprintf "%s is BOGUS!-typed:" n in 
	  static_error i n ~suppl:f_suppl s
        end; 
    { info = i;
      string = n;
      ctype = ct;
      atype = at;
      crel = Unknown;
      arel = dl1.arel;
      get = lift_r i (get_str n) ct (fun c -> 
        dl1.get (dls c));
      put = lift_rr i (put_str n) at ct (fun a c -> 
        rep (dl1.put a (dls c)));
      create = lift_r i (create_str n) at (fun a -> 
        rep (dl1.create a));
    }

  let quotient_a i cn1 dl1 = 
    let n = sprintf "qa (%s) (%s)" (Canonizer.string cn1) dl1.string in 
    let ct = dl1.ctype in 
    let at = Canonizer.rtype cn1 in 
    let cn_ct = Canonizer.ctype cn1 in 
    let dls = Canonizer.dls cn1 in 
    let rep = Canonizer.rep cn1 in 
    let equiv, f_suppl = check_rx_equiv dl1.atype cn_ct in
      if not equiv then
        begin
	  let s =sprintf "%s is ill-typed:@," n in 
	  static_error i n ~suppl:f_suppl s
        end;    
    { info = i;
      string = n;
      ctype = ct;
      atype = at;
      crel = dl1.crel;
      arel = Unknown;
      get = lift_r i (get_str n) ct (fun c -> 
        rep (dl1.get c));
      put = lift_rr i (put_str n) at ct (fun a c -> 
        dl1.put (dls a) c);
      create = lift_r i (create_str n) at (fun a -> 
        dl1.create (dls a));
    }
end

(* -------------------- KEY LENSES -------------------- *)
module KLens = struct
  type t = { 
      (* --- meta data --- *)
      info : Info.t;                       (* parsing info *)
      string: string;                      (* pretty printer *)
      (* --- types --- *)
      ctype : r;                           (* concrete type *)
      atype : r;                           (* abstract type *)
      crel: rel;                           (* concrete equiv rel type *)
      arel: rel;                           (* abstract equiv rel type *)      
      (* --- core --- *)
      get: RS.t -> RS.t;                   (* get function *)
      put: RS.t -> RS.t -> RS.t;           (* put function *)
      create: RS.t -> RS.t;                (* create function *)
      (* ---- keys ---- *)
      key:RS.t -> RS.t;
    }

  let info l = l.info
  let string l = l.string
  let get l = l.get
  let put l a c = l.put a c
  let create l a = l.create a
  let ctype l = l.ctype
  let atype l = l.atype
  let crel l = l.crel
  let arel l = l.arel
  let string_t l = l.string
  let key l = l.key 

  let mk_t i s ct at cr ar g p c k = 
    { info = i;
      string = s;
      ctype = ct;
      atype = at;
      crel = cr;
      arel = ar;
      get = g;
      put = p;
      create = c;
      key = k;
    }

  let t_of_dlens dl1 = 
    let i = DLens.info dl1 in 
    let n = DLens.string dl1 in 
    let at = DLens.atype dl1 in
      { info = i;
        string = n;
        ctype = DLens.ctype dl1;
        atype = at;
        crel = DLens.crel dl1;
        arel = DLens.arel dl1;
        get = DLens.get dl1;
        put = DLens.put dl1;
        create = DLens.create dl1;
        key = lift_r i (key_str n) at (fun a ->
          RS.empty);
      }

  let dlens_of_t kl1 = 
    DLens.mk_t 
      kl1.info
      kl1.string
      kl1.ctype
      kl1.atype
      kl1.crel
      kl1.arel
      kl1.get
      kl1.put
      kl1.create

  let addkey i kl1 = 
    let n = string kl1 in 
    let at = atype kl1 in 
    { kl1 with
      key = lift_r i (key_str n) at (fun a -> a); }
              
  let concat i kl1 kl2 = 
    let kl12 = t_of_dlens (DLens.concat i (dlens_of_t kl1) (dlens_of_t kl2)) in 
      { kl12 with
        key = lift_r i (key_str kl12.string) kl12.atype
          (do_split (split kl1.atype kl2.atype)
              kl1.key kl2.key
              (^));
      }

  let swap i kl1 kl2 = 
    let kl21 = t_of_dlens (DLens.swap i (dlens_of_t kl1) (dlens_of_t kl2)) in 
      { kl21 with
        key = lift_r i (key_str kl21.string) kl21.atype
          (do_split (split kl2.atype kl1.atype)
              kl2.key kl1.key
              (^));
      }

  let union i kl1 kl2 = 
    let kl1u2 = t_of_dlens (DLens.union i (dlens_of_t kl1) (dlens_of_t kl2)) in 
      { kl1u2 with
        key = lift_r i (key_str kl1u2.string) kl1u2.atype (fun a -> 
          match 
            Erx.match_str kl1.atype.rx a,
            Erx.match_str kl2.atype.rx a
          with
            | true,false -> kl1.key a
            | false,true -> kl2.key a
            | _ -> 
                let k1 = kl1.key a in 
                let k2 = kl2.key a in 
                  if RS.compare k1 k2 = 0 then k1 else RS.empty); }
        
  let star i kl1 = 
    let kl1s = t_of_dlens (DLens.star i (dlens_of_t kl1)) in 
      { kl1s with 
        key = lift_r i (key_str kl1s.string) kl1s.atype 
          (star_loop kl1.atype kl1.key); }

  let compose_kc i kl1 dl2 = 
    let kl1c2 = t_of_dlens (DLens.compose i (dlens_of_t kl1) dl2) in 
      { kl1c2 with
        key = lift_r i kl1c2.string kl1c2.atype (fun a -> 
          kl1.key ((DLens.create dl2) a)); }
        
  let compose_ck i dl1 kl2 = 
    let kl1c2 = t_of_dlens (DLens.compose i dl1 (dlens_of_t kl2)) in
      { kl1c2 with
        key = lift_r i kl1c2.string kl1c2.atype (fun a -> kl2.key a); }

  let probe i tag kl1 = 
    let kl1p = t_of_dlens (DLens.probe i tag (dlens_of_t kl1)) in 
      { kl1p with 
        key = (fun a -> 
          Util.format "%s KEY:@\n  [@[" tag;
          nlify_str a;
          Util.format "@]]@\n --> ";
          let k = kl1.key a in             
            Util.format "[@[";
            nlify_str k;
            Util.format "@]]@\n";
            k); }

  let default i def kl1 = 
    let n = sprintf "default %s %s" (RS.to_string def) kl1.string in 
    let at = kl1.atype in 
      { kl1 with
        create = lift_r i (create_str n) at (fun a -> 
          kl1.put a def);
      }

  let quotient_c i cn1 kl1 = 
    let kl1q = t_of_dlens (DLens.quotient_c i cn1 (dlens_of_t kl1)) in 
      { kl1q with
        key = kl1.key }

  let quotient_a i cn1 kl1 = 
    let kl1q = t_of_dlens (DLens.quotient_a i cn1 (dlens_of_t kl1)) in 
    let dls = Canonizer.dls cn1 in 
    let at = kl1q.atype in 
      { kl1q with
        key = lift_r i (key_str kl1q.string) at (fun a -> 
          kl1.key (dls a)); }      
end

(* -------------------- SKELETON LENSES -------------------- *)
module SLens = struct     
  let chk_chunks i n u1or u2or = 
    match !u1or,!u2or with 
      | None,_ -> u2or
      | _,None -> u1or
      | Some u1,Some u2 -> 
          if not (u1 = u2) then 
            static_error i n 
              (sprintf "cannot concatenate S-lenses operating on different chunks")
          else u1or

  type t = { 
      (* --- meta data --- *)
      info : Info.t;                       (* parsing info *)
      string: string;                      (* pretty printer *)
      (* --- types --- *)
      ctype : r;                           (* concrete type *)
      atype : r;                           (* abstract type *)
      stype : r;                           (* skeleton type *)
      crel: rel;                           (* concrete equiv rel type *)
      arel: rel;                           (* abstract equiv rel type *)      
      (* --- core --- *)
      chunk : (int option) ref;
      get: RS.t -> RS.t;                           (* get function *)
      put: RS.t -> RS.t -> dict -> RS.t * dict;    (* put function *)
      create:RS.t -> dict -> RS.t * dict;          (* create function *)
      parse: RS.t -> RS.t * dict;                  (* parser *)
    }

  let info sl = sl.info
  let string sl = sl.string
  let ctype sl = sl.ctype
  let stype sl = sl.stype
  let atype sl = sl.atype
  let crel sl = sl.crel
  let arel sl = sl.arel
  let chunk sl = sl.chunk
  let get sl = sl.get
  let put sl = sl.put
  let create sl = sl.create
  let parse sl = sl.parse

  let t_of_dlens dl1 = 
    let i = DLens.info dl1 in 
    let n = DLens.string dl1 in 
    let ct = DLens.ctype dl1 in
    let at = DLens.atype dl1 in
    let st = DLens.ctype dl1 in
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        stype = st;
        crel = DLens.crel dl1;
        arel = DLens.arel dl1;
        chunk = ref None;
        get = DLens.get dl1;
        put = lift_rrx i (put_str n) at ct (fun a s d -> 
          ((DLens.put dl1 a s),d));
        create = lift_rx i (create_str n) at (fun a d -> 
          ((DLens.create dl1) a, d));
        parse = lift_r i (parse_str n) ct (fun c -> (c,empty_dict));
      }

  let dlens_of_t sl1 = 
    let i = sl1.info in 
    let n = sl1.string in 
    let ct = sl1.ctype in 
    let at = sl1.atype in 
      DLens.mk_t 
        i n ct at 
        sl1.crel sl1.arel
        sl1.get
        (fun a c -> 
          let s,d = sl1.parse c in 
            (fst (sl1.put a s d)))
        (fun a -> fst (sl1.create a empty_dict))

  let smatch i e x kl1 = 
    let e_ns = RS.new_special e in
    let e_str = RS.make 1 e_ns in
    let n = sprintf "<%s>" (KLens.string kl1) in
    let ct = KLens.ctype kl1 in 
    let st = rx_box e_ns in 
    let at = KLens.atype kl1 in 
    let lookup k d = 
      let km = try SMap.find e_str d with Not_found -> KMap.empty in
        try 
          (match KMap.find k km with 
            | c::kl -> Some (c,SMap.add e_str (KMap.add k kl km) d)
            | [] -> None)
        with Not_found -> None in 
      { info = i;
        chunk = ref (Some x);
        string = n;
        ctype = ct;
        atype = at;
        stype = st;
        crel = KLens.crel kl1;
        arel = KLens.arel kl1;
        get = lift_r i (get_str n) ct (fun c -> KLens.get kl1 c);
        put = lift_rrx i (put_str n) at st (fun a s d -> 
          match lookup (KLens.key kl1 a) d with
              Some(c,d') -> (KLens.put kl1 a c,d')
            | None       -> (KLens.create kl1 a, d));
        create = lift_rx i (create_str n) at (fun a d -> 
          match lookup (KLens.key kl1 a) d with
              Some(c,d') -> (KLens.put kl1 a c,d')
            | None       -> (KLens.create kl1 a, d));
        parse = lift_r i (parse_str n) ct (fun c -> 
          let d = SMap.add e_str (KMap.add (KLens.key kl1 (KLens.get kl1 c)) [c] KMap.empty) empty_dict in 
            (e_str,d));
      }

  let concat i sl1 sl2 = 
    let n = sprintf "%s . %s" sl1.string sl2.string in 
    let ct = rx_seq sl1.ctype sl2.ctype in 
    let at = rx_seq sl1.atype sl2.atype in 
    let st = rx_seq sl1.stype sl2.stype in
      (* type checking *)
      chk_concat i n sl1.ctype sl2.ctype;
      chk_concat i n sl1.atype sl2.atype;
      let kl1or = chk_chunks i n sl1.chunk sl2.chunk in 
      (* lens *) 
      { info = i; 
        chunk = kl1or;
        string = n;
        ctype = ct;
        stype = st;
        atype = at;
        crel = combine_rel sl1.crel sl2.crel;
        arel = combine_rel sl1.arel sl2.arel;
        get = lift_r i (get_str n) ct
          (do_split (split sl1.ctype sl2.ctype)
              sl1.get sl2.get
              (^));
        put = lift_rrx i n at st (fun a s d -> 
            do_split_thread 
              (split2 sl1.atype sl2.atype sl1.stype sl2.stype) 
              (fun (a,s) -> sl1.put a s) 
              (fun (a,s) -> sl2.put a s)
              (^) (a,s) d);
        create = lift_r i n at (fun a d -> 
          (do_split_thread 
              (split sl1.atype sl2.atype)
              sl1.create sl2.create
              (^)) a d);
        parse = lift_r i n ct
          (do_split (split sl1.ctype sl2.ctype) 
              sl1.parse sl2.parse
              (fun (s1,d1) (s2,d2) -> (RS.append s1 s2,d1++d2))); 
      } 

  let union i sl1 sl2 = 
    (* utilities *)
    let bare_get = branch sl1.ctype sl1.get sl2.get in
    let n = sprintf "(%s|%s)" sl1.string sl2.string in 
    let ct = rx_alt sl1.ctype sl2.ctype in 
    let at = rx_alt sl1.atype sl2.atype in 
    let st = rx_alt sl1.stype sl2.stype in 
      chk_disjoint i n sl1.ctype sl2.ctype;
      let kl1or = chk_chunks i n sl1.chunk sl2.chunk in 
      { info = i;
        chunk = kl1or;
        string = n;
        ctype = ct; 
        atype = at;
        stype = st;
        crel = combine_rel sl1.crel sl2.crel;
        arel = combine_rel sl1.arel sl2.arel;
        get = lift_r i n ct bare_get;
        put = lift_rrx i n at st (fun a s d -> 
          match Erx.match_str sl1.atype.rx a, 
            Erx.match_str sl2.atype.rx a,
            Erx.match_str sl1.stype.rx s with
              | true,_,true  -> sl1.put a s d
              | _,true,false -> sl2.put a s d
              | true,false,false -> sl1.create a d 
              | false,true,true  -> sl2.create a d
              | false,false,_    -> assert false);
        create = lift_rx i (create_str n) at (fun a d -> 
          if Erx.match_str sl1.atype.rx a then sl1.create a d
          else sl2.create a d);
        parse = lift_r i n ct (branch sl1.ctype sl1.parse sl2.parse);
      }
        
  let star i sl1 = 
    (* body *)
    let min = 0 in
    let maxo = None in
    let reps = (min,maxo) in 
    let ct = rx_rep sl1.ctype reps in 
    let at = rx_rep sl1.atype reps in 
    let st = rx_rep sl1.stype reps in 
    let n = sprintf "(%s)%s" sl1.string (string_of_reps reps) in
      chk_iterate i n sl1.ctype min maxo;
      chk_iterate i n sl1.atype min maxo;
      { info = i;
        chunk = sl1.chunk;
        string = n;
        ctype = ct;
        atype = at;
        stype = st;
        crel = sl1.crel;
        arel = sl1.arel;
        get = lift_r i (get_str n) ct (star_loop sl1.ctype sl1.get);
        put = lift_rrx i (put_str n) at st (fun a s d -> 
	  let rec loop al kl d buf = match al,kl with
              [],_ -> (buf,d)
            | a1::at,[] -> 
                let c1',d' = sl1.create a1 d in
                let buf' = RS.append buf c1' in
                  loop at [] d' buf'
            | a1::at,s1::st -> 
                let c1',d' = sl1.put a1 s1 d in 
                let buf' = RS.append buf c1' in
                  loop at st d' buf' in 
            loop 
              (Erx.unambig_star_split sl1.atype.rx a)
              (Erx.unambig_star_split sl1.stype.rx s)
              d RS.empty);
        create = lift_rx i (create_str n) at (fun a d -> 
          Safelist.fold_left (fun (buf, d) a1 -> 
            let c1,d' = sl1.create a1 d in 
            let buf' = RS.append buf c1 in
              (buf', d'))
            (RS.empty, d) (Erx.unambig_star_split sl1.atype.rx a));
	parse = lift_r i (parse_str n) ct (fun c ->            
          Safelist.fold_left (fun (buf, d) c1 -> 
            let c1,d1 = sl1.parse c1 in
	    let buf' = RS.append buf c1 in
	    let d' = d ++ d1 in
	      (buf', d'))
            (RS.empty, empty_dict)
            (Erx.unambig_star_split sl1.ctype.rx c)); 
      }

  let swap i sl1 sl2 = 
    let n = sprintf "swap (%s) (%s)" sl1.string sl2.string in 
    let ct = rx_seq sl1.ctype sl2.ctype in  
    let at = rx_seq sl2.atype sl1.atype in 
    let st = rx_seq sl2.stype sl1.stype in 
      (* type check *)
      chk_concat i n sl1.ctype sl2.ctype;
      chk_concat i n sl2.atype sl1.atype;
      let kl1or = chk_chunks i n sl1.chunk sl2.chunk in 
      { info = i;
        chunk = kl1or;
        string = n;
        ctype = ct; 
        atype = at;
        stype = st;
        crel = combine_rel sl1.crel sl2.crel;
        arel = combine_rel sl1.arel sl2.arel;
        get = lift_r i n ct 
          (fun c -> 
            let c1,c2 = split sl1.ctype sl2.ctype c in 
              (sl2.get c2) ^ (sl1.get c1));
        put = lift_rrx i (put_str n) at st (fun a s d -> 
          let a2,a1 = split sl2.atype sl1.atype a in 
          let s1,s2 = split sl1.stype sl2.stype s in 
          let c2',d1 = sl2.put a2 s2 d in 
          let c1',d2 = sl1.put a1 s1 d1 in 
            (c1' ^ c2', d2));
        create = lift_rx i (create_str n) at (fun a d -> 
          let a2,a1 = split sl2.atype sl1.atype a in          
          let c2',d1 = sl2.create a2 d in 
          let c1',d2 = sl1.create a1 d1 in 
            (c1' ^ c2', d2));
        parse = lift_r i (parse_str n) ct (fun c ->
          let c1,c2 = split sl1.ctype sl2.ctype c in 
          let s2,d2 = sl2.parse c2 in 
          let s1,d1 = sl1.parse c1 in 
            (s1 ^ s2, d2++d1)); }


  let probe i tag sl1 = 
    let n = sprintf "probe \"%s\" (%s)" (whack tag) sl1.string in 
      { sl1 with 
        info = i;
        string = n;
        get = (fun c -> 
          Util.format "%s GET:@\n  [@[" tag;
          nlify_str c;
          Util.format "@]]@\n --> ";
          let a = sl1.get c in 
            Util.format "[@[";
            nlify_str a;
            Util.format "@]]@\n";
            a);
        put = (fun a s d ->
          Util.format "%s PUT:@\n  [@[" tag;
          nlify_str a;
          Util.format "@]]@\n  [@[";
          nlify_str s;
          Util.format "@]]@\n  [@[";
          format_dict d;
          Util.format "@]]@\n --> ";
          let c',d' = sl1.put a s d in             
            Util.format "[@[";
            nlify_str c';
            Util.format "@]]@\n";
            format_dict d';
            Util.format "@]]@\n  [@[";
            c',d');
        create = (fun a d -> 
          Util.format "%s CREATE:@\n  [@[" tag;
          nlify_str a;
          Util.format "@]]@\n  [@[";
          format_dict d;
          Util.format "@]]@\n --> ";
          let c',d' = sl1.create a d in             
            Util.format "[@[";
            nlify_str c';
            Util.format "@]]@\n  [@[";
            format_dict d';
            Util.format "@]]@\n";
            c',d'); 
        parse = (fun c -> 
          Util.format "%s PARSE:@\n  [@[" tag;
          nlify_str c;
          Util.format "@]]@\n --> ";
          let s,d = sl1.parse c in 
            Util.format "[@[";
            nlify_str s;
            Util.format "@]]@\n  [@[";
            format_dict d;
            Util.format "@]]@\n";
            s,d);
      }

  let default i def sl1 = 
    let n = sprintf "default %s %s" (RS.to_string def) sl1.string in 
    let at = sl1.atype in 
      { sl1 with
        create = lift_rx i (create_str n) at (fun a d -> 
          let s,d2 = sl1.parse def in 
          let d' = d2 ++ d in 
            sl1.put a s d');
      }

  let quotient_c i cn1 sl1 = 
    let n = sprintf "qc_slens (%s) (%s)" (Canonizer.string cn1) sl1.string in 
    let ct = Canonizer.rtype cn1 in 
    let at = sl1.atype in 
    let st = sl1.stype in 
    let cn_ctype = Canonizer.ctype cn1 in 
    let dls = Canonizer.dls cn1 in 
    let rep = Canonizer.rep cn1 in 
    let equiv, f_suppl = check_rx_equiv cn_ctype sl1.ctype in
      if not equiv then
        begin
	  let s =sprintf "%s is ill-typed:@," n in 
	  static_error i n ~suppl:f_suppl s
        end;    
    { info = i;
      string = n;
      ctype = ct;
      atype = at;
      stype = st;
      crel = Unknown;
      arel = sl1.arel;
      chunk = sl1.chunk;
      get = lift_r i (get_str n) ct (fun c -> 
        sl1.get (dls c));
      put = lift_rrx i (put_str n) at st (fun a s d -> 
        let c',d = sl1.put a s d in 
        (rep c', d));
      create = lift_r i (create_str n) at (fun a d -> 
        let c,d = sl1.create a d in 
        (rep c, d));
      parse = lift_r i (parse_str n) ct (fun c -> 
        sl1.parse (dls c));
    }
                                        

  let quotient_a i cn1 sl1 = 
    let n = sprintf "qa_slens (%s) (%s)" (Canonizer.string cn1) sl1.string in 
    let ct = sl1.ctype in 
    let at = Canonizer.rtype cn1 in 
    let st = sl1.stype in 
    let cn_ct = Canonizer.ctype cn1 in 
    let dls = Canonizer.dls cn1 in 
    let rep = Canonizer.rep cn1 in 
    let equiv, f_suppl = check_rx_equiv sl1.atype cn_ct in
      if not equiv then
        begin
	  let s =sprintf "%s is ill-typed:@," n in 
	  static_error i n ~suppl:f_suppl s
        end;    
    { info = i;
      string = n;
      ctype = ct;
      atype = at;
      stype = st;
      crel = sl1.crel;
      arel = Unknown;
      chunk = sl1.chunk;
      get = lift_r i (get_str n) ct (fun c -> 
        rep (sl1.get c));
      put = lift_rrx i (put_str n) at st (fun a s d -> 
        sl1.put (dls a) s d);
      create = lift_r i (create_str n) at (fun a d -> 
        sl1.create (dls a) d);
      parse = lift_r i (parse_str n) ct (fun c -> 
        sl1.parse c);
    }
end

(* -------------------- RESOURCEFUL LENSES -------------------- *)
module RLens = struct
  type t = { 
      (* --- meta data --- *)
      info : Info.t;                   (* parsing info *)
      string: string;                  (* pretty printer *)
      (* --- types --- *)
      ctype : r;                       (* concrete type *)
      atype : r;                       (* abstract type *)
      crel: rel;                       (* concrete equiv rel type *)
      arel: rel;                       (* abstract equiv rel type *)  
      (* --- core --- *)
      get: RS.t -> RS.t;               (* get function *)
      put: RS.t -> RS.t -> RS.t;       (* put function *)
      create: RS.t -> RS.t;            (* create function *)
      equiv: RS.t -> RS.t -> bool;     (* equiv relation *)
    }
  let info rl = rl.info
  let string rl = rl.string
  let get rl = rl.get
  let put rl = rl.put
  let create rl = rl.create
  let equiv rl = rl.equiv
  let ctype rl = rl.ctype
  let atype rl = rl.atype
  let crel rl = rl.crel
  let arel rl = rl.arel

  let t_of_slens sl1 = 
    let i = SLens.info sl1 in 
    let n = SLens.string sl1 in 
    let ct = SLens.ctype sl1 in 
    let at = SLens.atype sl1 in 
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        crel = SLens.crel sl1;
        arel = SLens.arel sl1;
        get = SLens.get sl1;
        put = lift_rr i (put_str n) at ct (fun a c -> 
          let s,d = SLens.parse sl1 c in 
            fst (SLens.put sl1 a s d));
        create = lift_r i (create_str n) at (fun a -> 
          fst (SLens.create sl1 a empty_dict));
        equiv = (fun _ _ -> assert false); }
        
  let t_of_dlens dl1 = 
    let i = DLens.info dl1 in 
    let n = DLens.string dl1 in 
    let ct = DLens.ctype dl1 in 
    let at = DLens.atype dl1 in 
      { info = i;
        string = n;
        ctype = ct;
        atype = at;
        crel = DLens.crel dl1;
        arel = DLens.arel dl1;
        get = DLens.get dl1;
        put = DLens.put dl1;
        create = DLens.create dl1;
        equiv = (fun c c' -> RS.compare c c' = 0);
      }

  let dlens_of_t rl1 = 
    DLens.mk_t 
      rl1.info
      rl1.string
      rl1.ctype
      rl1.atype
      rl1.crel
      rl1.arel
      rl1.get
      rl1.put
      rl1.create

  let concat i rl1 rl2 = 
    t_of_dlens (DLens.concat i (dlens_of_t rl1) (dlens_of_t rl2)) 

  let swap i pl1 pl2 = 
    t_of_dlens (DLens.swap i (dlens_of_t pl1) (dlens_of_t pl2))

  let union i pl1 pl2 = 
    t_of_dlens (DLens.union i (dlens_of_t pl1) (dlens_of_t pl2)) 
        
  let star i pl1 = 
    t_of_dlens (DLens.star i (dlens_of_t pl1)) 

  let compose i pl1 pl2 = 
    (* body *)
    let n = sprintf "%s; %s" pl1.string pl2.string in 
    let ct = pl1.ctype in 
    let at = pl2.atype in 
    (match pl1.arel,pl2.crel with
      | Identity,Identity -> ()
      | _ -> 
          let s = sprintf "the composition of %s and %s is ill-typed: %s"            
            pl1.string pl2.string 
            "the middle relations must both be the identity" in 
            static_error i n s);
    let equiv, f_suppl = check_rx_equiv pl1.atype pl2.ctype in
    if not equiv then
      begin
	let s =(sprintf "the composition of %s and %s is ill-typed: "
		  pl1.string pl2.string)in
	static_error i n ~suppl:f_suppl s 
      end;
      { info = i; 
        string = n;
        ctype = ct;
        atype = at;                 
        crel = pl1.crel;
        arel = pl2.arel;
        get = lift_r i (get_str n) ct (fun c -> 
          pl2.get (pl1.get c));
        put = lift_rr i n at ct 
          (fun a c -> 
            let b = pl1.get c in 
              pl1.put (pl2.put a b) c);
        create = lift_r i n at (fun a -> 
          pl1.create (pl2.create a));
        equiv = (fun _ _ -> assert false);
      }

  let probe i tag pl1 = 
    t_of_dlens (DLens.probe i tag (dlens_of_t pl1))

  let default i def pl1 = 
    t_of_dlens (DLens.default i def (dlens_of_t pl1))

  let quotient_c i cn1 rl1 =
    t_of_dlens (DLens.quotient_c i cn1 (dlens_of_t rl1))

  let quotient_a i cn1 rl1 =
    t_of_dlens (DLens.quotient_a i cn1 (dlens_of_t rl1))

end
