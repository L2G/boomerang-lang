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
(* /boomerang/src/bregexp.ml                                                   *)
(* Boomerang regular expressions                                               *)
(* $Id$                                                                        *)
(*******************************************************************************)

let (@) = Safelist.append
let sprintf = Printf.sprintf
module S = Bstring
let string_concat = (^)

(* dead? *)
let string_of_reps = function
  | (0,None) -> "*" 
  | (0,Some 1) -> "?"
  | (1,None) -> "+"
  | (i,None) -> sprintf "{%d,}" i
  | (i,Some j) -> if i=j then sprintf "{%d}" i else sprintf "{%d,%d}" i j

(* definitions of different ranks. *)
type type_exp = 
  | Bexp           (* bar *)
  | Mexp           (* minus *)
  | Iexp           (* inter *)
  | Sexp           (* swap *)
  | Cexp           (* concat *)
  | Rexp           (* rep *)
  | Aexp           (* atomic *)
      
(* need_par_left e1 e2 determines if you need to put parenthesis
 * around exp of type e1 when you create an expression of type e2 with
 * e1 on the left *)
      
let need_par_left e1 e2 = match e1,e2 with
  | Aexp, _ -> false
  | _, Aexp -> assert false
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
  | Aexp, _ -> false
  | _, Aexp -> assert false
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

(* Erx.ts, extended with strings for printing, representatives *)
(* The "rank" field is for pretty printing purposes *)
type t = { str : string;
           mutable rep: S.t option;
           rx : Erx.t; 
           rank : type_exp }
    
let string_of_t r = r.str

let equiv r1 r2 = 
  Erx.equiv r1.rx r2.rx

let rep r = match r.rep with 
  | None -> 
      let s = Erx.representative r.rx in 
        r.rep <- Some s;
        s
  | Some s -> s

let determinize r = {r with rx = Erx.determinize r.rx}

let id = fun x -> x

let check_equiv r1 r2 =
  if equiv r1 r2 then (true,id)
  else (false,
        fun() ->
          let printrep s1 s2 n1 n2 =
            try
              let v = Erx.representative (Erx.mk_diff s1.rx s2.rx) in
              Util.format "value \"%s\"@\nis in the %s but not the %s@ "
                (S.string_of_t v) n1 n2
            with Not_found -> () in
          Util.format "%s <> %s@\n" r1.str r2.str;
          printrep r1 r2 "first" "second";
          printrep r2 r1 "second" "first"
       )
        
let concat_repr r1 r2 concat_symb rc =
  let s1' = if need_par_left r1.rank rc then sprintf "(%s)" r1.str else r1.str in
  let s2' = if need_par_right r2.rank rc then sprintf "(%s)" r2.str else r2.str in
  sprintf "%s %s %s" s1' concat_symb s2'    

let epsilon = 
  { str = "epsilon"; 
    rep = Some (S.t_of_string "");
    rx = Erx.epsilon;
    rank = Aexp}

let empty = 
  { str = "empty";
    rep = None;
    rx = Erx.empty;
    rank = Aexp}

(* two functions for a quick test of equality.
 * to be used only for pretty printing *)
let is_epsilon r =
  r.rx == Erx.epsilon

let is_empty r = 
  r.rx == Erx.empty

let box e = 
  let st = S.repr e in
    { str = st;
      rep = None;
      rx = Erx.mk_str false (S.make 1 e);
      rank = Aexp
    }

let str_of_cl cl = 
  let buf = Buffer.create 17 in 
  let to_str fs = Char.escaped (Char.chr (S.int_of_sym fs)) in
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

let set cl = 
  { str = sprintf "[%s]" (str_of_cl cl);
    rep = 
      (match cl with 
         | (h,_)::_ -> Some (S.make 1 h)
         | _ -> None);
    rx = Erx.mk_cset true cl;
    rank = Aexp
  }

let negset cl = 
  { str = sprintf "[^%s]" (str_of_cl cl);
    rep = None;
    rx = Erx.mk_cset false cl;
    rank = Aexp
  }
  
let str ignore_case s = 
  let s_string = String.escaped (S.string_of_t s) in
    { str = sprintf "%s\"%s\"%s" 
        (if ignore_case then "ignore_case(" else "")
        s_string
        (if ignore_case then ")" else "");      
      rep = Some s;
      rx = Erx.mk_str ignore_case s;
      rank = Aexp}

let seq r1 r2 = 
  if is_empty r1 || is_empty r2 then empty
  else if is_epsilon r1 then r2 
  else if is_epsilon r2 then r1
  else let str = concat_repr r1 r2 "." Cexp in
    { str = str; 
      rep = None;
      rx = Erx.mk_seq r1.rx r2.rx;
      rank = Cexp }

let alt r1 r2 = 
  if is_empty r1 then r2
  else if is_empty r2 then r1
  else 
  let add_qmark s r = if need_par_left r Rexp then sprintf "(%s)?" s else sprintf "%s?" s in
  let str = 
    if is_epsilon r1 then add_qmark r2.str r2.rank 
    else if is_epsilon r2 then add_qmark r1.str r1.rank
    else concat_repr r1 r2 "|" Bexp in
  { str = str; 
    rep = r1.rep;
    rx = Erx.mk_alt r1.rx r2.rx;
    rank = Bexp
  }

let set_str r1 s = 
  { r1 with str = s; rank = Aexp; }
      
(* XXX: if max < min, then Erx will raise an Invalid_argument exception *)
let star r1 =
  let s1 = if need_par_left r1.rank Rexp then sprintf "(%s)*" r1.str else string_concat r1.str "*" in
  { str = s1;
    rep = Some (S.t_of_string "");
    rx = Erx.mk_star r1.rx;
    rank = Rexp}

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

let iter r1 min maxo = 
  let i = Info.M ("Bregexp.iter") in 
  generic_iter i epsilon alt seq star r1 min maxo

let diff r1 r2 =
  if is_empty r1 then empty 
  else if is_empty r2 then r1
  else let str = concat_repr r1 r2 "-" Mexp in
  let r = Erx.mk_diff r1.rx r2.rx in 
    { str = str;
      rep = None;
      rx = r; 
      rank = Mexp}
      
let inter r1 r2 =
  if is_empty r1 || is_empty r2 then empty
  else let r = Erx.mk_inter r1.rx r2.rx in 
  let str = concat_repr r1 r2 "&" Iexp in
    { str = str;
      rep = None;
      rx = r; 
      rank = Iexp} 
      
let lowercase r1 = 
  { str = sprintf "lowercase(%s)" r1.str;
    rep = None;
    rx = Erx.mk_lowercase r1.rx;
    rank = Aexp }

let uppercase r1 = 
  { str = sprintf "uppercase(%s)" r1.str;
    rep = None;
    rx = Erx.mk_uppercase r1.rx;
    rank = Aexp }

let expand r1 s1 s2 = 
  { str = sprintf "extend(%s,%s,%s)" r1.str (S.repr s1) (S.string_of_t s2);
    rep = r1.rep;
    rx = Erx.expand r1.rx s1 s2;
    rank = Aexp }

let match_str r s = Erx.match_str r.rx s

let unambig_split r1 r2 s = Erx.unambig_split r1.rx r2.rx s

let unambig_star_split r1 s = Erx.unambig_star_split r1.rx s

let split_positions r1 r2 s = Erx.split_positions r1.rx r2.rx s

let string_of_t r = r.str

let split_bad_prefix t s = 
  try 
    let is, approx = Erx.find_exit_automaton t.rx s in
    let i = Rint.Set.max_elt is in 
    let s1 = S.sub s 0 i in 
    let s2 = S.sub s i ((S.length s) - i) in 
      (s1,s2,approx)
  with
    | Not_found -> (S.empty, s,false)

let no_type_check = Prefs.createBool "no-type-check" false
  "don't type check concatenation, alternative and repetition of lenses. To be used with precaution !"
  "don't type check concatenation, alternative and repetition of lenses. To be used with precaution !"

(* common stuff *)
let unambig_seq i n r1 r2 = 
  if not (Prefs.read no_type_check) then( 
    match Erx.unambig_seq r1.rx r2.rx with
      |	Erx.NA_true rxseq -> 
	  let str = concat_repr r1 r2 "." Cexp in
	    { str = str; 
	      rep = None;
              rx = rxseq;
	      rank = Cexp}
      | Erx.NA_false dss ->
          let (s1,s2),(s1',s2') = Erx.example_of_dss dss in 
            Berror.static_error i n 
              (sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" \"%s\" \nand\n\"%s\" \"%s\""
                 r1.str r2.str 
                 (S.string_of_t s1) (S.string_of_t s2) 
                 (S.string_of_t s1') (S.string_of_t s2')))
  else seq r1 r2


(* to be modified. Need to add counter example *)
let easy_seq i n r1 r2 = 
  if (not (Prefs.read no_type_check) && not (Erx.easy_seq r1.rx r2.rx)) then
    Berror.static_error i n 
      (sprintf "the concatenation of %s and %s is not easy.\n"
         r1.str r2.str);
  seq r1 r2
    
let unambig_star i n r1 = 
  if not (Prefs.read no_type_check) then( 
    match Erx.unambig_star r1.rx with
	Erx.NSA_true rxstar ->
	  let s1 = if need_par_left r1.rank Rexp then sprintf "(%s)*" r1.str else string_concat r1.str "*" in
	    { str = s1;
	      rep = None;
              rx = rxstar;
	      rank = Rexp}
      | Erx.NSA_empty_word -> 
          Berror.static_error i n 
            (sprintf "the iteration of %s is ambiguous: \"\""
               r1.str)
      | Erx.NSA_false -> 
          Berror.static_error i n 
            (sprintf "the iteration of %s is ambiguous"
               r1.str)
      | Erx.NSA_false_ce dms -> 
          Berror.static_error i n 
            (sprintf "the iteration of %s is ambiguous: \n\"%s\""
               r1.str
               (S.string_of_t (Erx.example_of_dms dms))))
  else star r1

let easy_star i n r1 = 
  if (not (Prefs.read no_type_check) && not (Erx.easy_star r1.rx)) then
    Berror.static_error i n 
      (sprintf "the iteration of %s is not easy."
         r1.str);
  star r1

let disjoint_alt i n t1 t2 = 
  if not (Prefs.read no_type_check) then( 
  let t1ut2 = Erx.mk_inter t1.rx t2.rx in 
    if not (Erx.is_empty t1ut2) then
      Berror.static_error i n 
        (sprintf "the intersection of %s and %s is non-empty: \"%s\""
            t1.str t2.str
            (S.string_of_t (Erx.representative t1ut2)))
    else alt t1 t2)
  else alt t1 t2

let split_positions t1 t2 = Erx.split_positions t1.rx t2.rx

