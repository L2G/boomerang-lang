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
module S = String
let string_concat = (^)

(* dead? *)
let string_of_reps = function
  | (0,None) -> "*" 
  | (0,Some 1) -> "?"
  | (1,None) -> "+"
  | (i,None) -> sprintf "{%d,}" i
  | (i,Some j) -> if i=j then sprintf "{%d}" i else sprintf "{%d,%d}" i j

(* definitions of different ranks. *)
type r = 
  | Urnk (* union *)
  | Drnk (* diff *)
  | Irnk (* inter *)
  | Crnk (* concat *)
  | Srnk (* star *)
  | Arnk (* atomic *)
      
(* need_par_left e1 e2 determines if you need to put parenthesis
 * around exp of type e1 when you create an expression of type e2 with
 * e1 on the left *)
      
let need_par_left e1 e2 = match e1,e2 with
  | Arnk, _ -> false
  | _, Arnk -> assert false
  | Crnk, _ -> false
  | _, Crnk -> true
  | Srnk, _ -> false
  | _, Srnk -> true
  | Irnk, _ -> false
  | _, Irnk -> true
  | Urnk, Drnk
  | Drnk, Urnk -> true
  | Drnk, Drnk -> false
  | Urnk, Urnk -> false

let lpar = need_par_left

(* need_par_right e1 e2 determines if you need to put parenthesis around rnk of type e1 when you
 * create an rnkression of type e2 with e1 on the right
 *)
 
let need_par_right e1 e2 = match e1,e2 with
  | Arnk, _ -> false
  | _, Arnk -> assert false
  | _, Crnk -> true
  | Crnk, _ -> false
  | _, Srnk -> true
  | Srnk, _ -> false
  | _, Irnk -> true
  | Irnk, _ -> false
  | Urnk, Drnk
  | Drnk, Urnk -> true
  | Drnk, Drnk -> true
  | Urnk, Urnk -> true

let rpar = need_par_right


(* Bnfa.ts, extended with strings for printing, representatives *)
(* The "rank" field is for pretty printing purposes *)
type t = { str : string;
           mutable rep: string option;
           rx : Bnfa.t; 
           rank : r }
    
let string_of_t r = r.str
let format_t r = Util.format "%s" r.str

let rank r = r.rank

let equiv r1 r2 = 
  Bnfa.equiv r1.rx r2.rx

let representative r = 
  try 
    match r.rep with 
      | None -> 
          let s = Bnfa.representative r.rx in 
            r.rep <- Some s;
            Some s
      | Some s -> Some s           
  with Not_found -> None

let determinize r = {r with rx = Bnfa.determinize r.rx}

let id = fun x -> x

let check_equiv r1 r2 =
  if equiv r1 r2 then (true,id)
  else (false,
        fun() ->
          let printrep s1 s2 n1 n2 =
            try
              let v = Bnfa.representative (Bnfa.mk_diff s1.rx s2.rx) in
              Util.format "value \"%s\"@\nis in the %s but not the %s@ "
                v n1 n2
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
    rep = Some "";
    rx = Bnfa.epsilon;
    rank = Arnk}

let empty = 
  { str = "empty";
    rep = None;
    rx = Bnfa.empty;
    rank = Arnk}

let anything = 
  { str = "anything";
    rep = Some "";
    rx = Bnfa.mk_star (Bnfa.mk_cset false []);
    rank = Arnk }

(* two functions for a quick test of equality.
 * to be used only for pretty printing *)
let is_epsilon r =
  r.rx == Bnfa.epsilon

let is_empty r = 
  r.rx == Bnfa.empty

let is_singleton r = 
  Bnfa.is_singleton r.rx

let is_empty_or_singleton r = 
  is_empty r || Bnfa.is_singleton r.rx

let str_of_cl cl = 
  let buf = Buffer.create 17 in 
  let to_str fs = Char.escaped fs in
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

let mk_cset cl = 
  { str = sprintf "[%s]" (str_of_cl cl);
    rep = 
      (match cl with 
         | (h,_)::_ -> Some (S.make 1 h)
         | _ -> None);
    rx = Bnfa.mk_cset true cl;
    rank = Arnk
  }

let mk_neg_cset cl = 
  { str = sprintf "[^%s]" (str_of_cl cl);
    rep = None;
    rx = Bnfa.mk_cset false cl;
    rank = Arnk
  }
  
let mk_string s = 
  let s_string = String.escaped s in
    { str = sprintf "\"%s\"" s_string;
      rep = Some s;
      rx = Bnfa.mk_str s;
      rank = Arnk}

let mk_seq r1 r2 = 
  if is_empty r1 || is_empty r2 then empty
  else if is_epsilon r1 then r2 
  else if is_epsilon r2 then r1
  else let str = concat_repr r1 r2 "." Crnk in
    { str = str; 
      rep = None;
      rx = Bnfa.mk_seq r1.rx r2.rx;
      rank = Crnk }

let mk_alt r1 r2 = 
  if is_empty r1 then r2
  else if is_empty r2 then r1
  else 
  let add_qmark s r = if need_par_left r Srnk then sprintf "(%s)?" s else sprintf "%s?" s in
  let str = 
    if is_epsilon r1 then add_qmark r2.str r2.rank 
    else if is_epsilon r2 then add_qmark r1.str r1.rank
    else concat_repr r1 r2 "|" Urnk in
  { str = str; 
    rep = r1.rep;
    rx = Bnfa.mk_alt r1.rx r2.rx;
    rank = Urnk
  }

let set_str r1 s = 
  { r1 with str = s; rank = Arnk; }
      
(* XXX: if max < min, then Bnfa will raise an Invalid_argument exception *)
let mk_star r1 =
  let s1 = if need_par_left r1.rank Srnk then sprintf "(%s)*" r1.str else string_concat r1.str "*" in
  { str = s1;
    rep = Some "";
    rx = Bnfa.mk_star r1.rx;
    rank = Srnk}

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

let mk_iter r1 min max = 
  let i = Info.M ("Bregrnk.iter") in 
  generic_iter i epsilon mk_alt mk_seq mk_star r1 min max

let mk_diff r1 r2 =
  if is_empty r1 then empty 
  else if is_empty r2 then r1
  else let str = concat_repr r1 r2 "-" Drnk in
  let r = Bnfa.mk_diff r1.rx r2.rx in 
    { str = str;
      rep = None;
      rx = r; 
      rank = Drnk}
      
let mk_complement r1 = mk_diff (mk_star (mk_neg_cset [])) r1

let mk_inter r1 r2 =
  if is_empty r1 || is_empty r2 then empty
  else let r = Bnfa.mk_inter r1.rx r2.rx in 
  let str = concat_repr r1 r2 "&" Irnk in
    { str = str;
      rep = None;
      rx = r; 
      rank = Irnk} 
      
let lowercase r1 = 
  { str = sprintf "lowercase(%s)" r1.str;
    rep = None;
    rx = Bnfa.mk_lowercase r1.rx;
    rank = Arnk }

let uppercase r1 = 
  { str = sprintf "uppercase(%s)" r1.str;
    rep = None;
    rx = Bnfa.mk_uppercase r1.rx;
    rank = Arnk }

let expand r1 s1 s2 = 
  { str = sprintf "extend(%s,%c,%s)" r1.str s1 s2;
    rep = r1.rep;
    rx = Bnfa.expand r1.rx s1 s2;
    rank = Arnk }

let match_string r s = Bnfa.match_str r.rx s

let match_string_positions r s = Bnfa.match_prefix r.rx s 

let unambig_split r1 r2 s = Bnfa.unambig_split r1.rx r2.rx s

let unambig_star_split r1 s = Bnfa.unambig_star_split r1.rx s

let split_positions r1 r2 s = Bnfa.split_positions r1.rx r2.rx s

let string_of_t r = r.str

let split_bad_prefix t s = 
  try 
    let is, approx = Bnfa.find_exit_automaton t.rx s in
    let i = Int.Set.max_elt is in 
    let s1 = S.sub s 0 i in 
    let s2 = S.sub s i ((S.length s) - i) in 
      (s1,s2)
  with
    | Not_found -> ("", s)

let no_type_check = Prefs.createBool "no-type-check" false
  "don't type check concatenation, alternative and repetition of lenses. To be used with precaution !"
  "don't type check concatenation, alternative and repetition of lenses. To be used with precaution !"

let splittable r1 r2 = 
  match Bnfa.unambig_seq r1.rx r2.rx with
    | Bnfa.NA_true _ -> true
    | _ -> false

let splittable_cex r1 r2 = 
  match Bnfa.unambig_seq r1.rx r2.rx with
    | Bnfa.NA_false dss -> 
        let w = dss.Bnfa.dss_example in
        let n = String.length w in 
        let i1 = dss.Bnfa.dss_cut1 in 
        let i2 = dss.Bnfa.dss_cut2 in 
        Misc.Left(String.sub w 0 i1, String.sub w i1 (i2-i1), String.sub w i2 (n-i2))
    | Bnfa.NA_true r12 -> 
        let str = concat_repr r1 r2 "." Crnk in
	Misc.Right 
          { str = str; 
	    rep = None;
            rx = r12;
	    rank = Crnk }

let iterable r1 = 
  match Bnfa.unambig_star r1.rx with
    | Bnfa.NSA_true _ -> true
    | _ -> false

let iterable_cex r1 = 
  let r1s = mk_star r1 in 
  match splittable_cex r1 (mk_star r1) with
    | Misc.Left _ as res -> res
    | Misc.Right _ -> Misc.Right(r1s)

let split_positions t1 t2 = Bnfa.split_positions t1.rx t2.rx

let seq_split s1 s2 w =
  let ps = split_positions s1 s2 w in 
    if not (Int.Set.cardinal ps = 1) then 
      None
    else
      let n = String.length w in 
      let j = Int.Set.choose ps in 
      let s1,s2 = (String.sub w 0 j, String.sub w j (n-j)) in 
	Some (s1,s2)

let star_split s1 w = 
  let s1_star = mk_star s1 in 
  let ps = Int.Set.remove 0 (split_positions s1_star s1_star w) in 
  let _,rev = 
    Int.Set.fold 
      (fun j (i,acc) -> (j,(String.sub w i (j-i))::acc)) 
      ps (0,[]) in 
    Safelist.rev rev 


(* common stuff *)
let unambig_seq i n r1 r2 = 
  if not (Prefs.read no_type_check) then( 
    match Bnfa.unambig_seq r1.rx r2.rx with
      |	Bnfa.NA_true rxseq -> 
	  let str = concat_repr r1 r2 "." Crnk in
	    { str = str; 
	      rep = None;
              rx = rxseq;
	      rank = Crnk}
      | Bnfa.NA_false dss ->
          let (s1,s2),(s1',s2') = Bnfa.example_of_dss dss in 
            Berror.static_error i n 
              (sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" \"%s\" \nand\n\"%s\" \"%s\""
                 r1.str r2.str 
                 (s1) (s2) 
                 (s1') (s2')))
  else mk_seq r1 r2


(* to be modified. Need to add counter example *)
let easy_seq i n r1 r2 = 
  if (not (Prefs.read no_type_check) && not (Bnfa.easy_seq r1.rx r2.rx)) then
    Berror.static_error i n 
      (sprintf "the concatenation of %s and %s is not easy.\n"
         r1.str r2.str);
  mk_seq r1 r2
    
let unambig_star i n r1 = 
  if not (Prefs.read no_type_check) then( 
    match Bnfa.unambig_star r1.rx with
	Bnfa.NSA_true rxstar ->
	  let s1 = if need_par_left r1.rank Srnk then sprintf "(%s)*" r1.str else string_concat r1.str "*" in
	    { str = s1;
	      rep = None;
              rx = rxstar;
	      rank = Srnk}
      | Bnfa.NSA_empty_word -> 
          Berror.static_error i n 
            (sprintf "the iteration of %s is ambiguous: \"\""
               r1.str)
      | Bnfa.NSA_false -> 
          Berror.static_error i n 
            (sprintf "the iteration of %s is ambiguous"
               r1.str)
      | Bnfa.NSA_false_ce dms -> 
          Berror.static_error i n 
            (sprintf "the iteration of %s is ambiguous: \n\"%s\""
               r1.str
               ((Bnfa.example_of_dms dms))))
  else mk_star r1

let easy_star i n r1 = 
  if (not (Prefs.read no_type_check) && not (Bnfa.easy_star r1.rx)) then
    Berror.static_error i n 
      (sprintf "the iteration of %s is not easy."
         r1.str);
  mk_star r1

let disjoint_cex t1 t2 = 
  let t1ut2 = Bnfa.mk_inter t1.rx t2.rx in 
  if not (Bnfa.is_empty t1ut2) then
    Some (Bnfa.representative t1ut2)
  else None

let disjoint t1 t2 = match disjoint_cex t1 t2 with
  | None -> true
  | _ -> false