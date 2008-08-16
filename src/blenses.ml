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

module RxImpl = Bregexp

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

let concat_array a = 
  let buf = Buffer.create 17 in
    Array.iter (Buffer.add_string buf) a;
    Buffer.contents buf 
    
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

let notypecheck = Prefs.createBool "no-type-check" false
  "skip type checking of lens primitives"
  "skip type checking of lens primitives"

let plift_r b i n t1 f =  
  if not (b || Prefs.read paranoid) then f else
    (fun x ->     
       if not (RxImpl.match_string t1 x) then 
         begin 
           Berror.type_error i (RxImpl.split_bad_prefix t1 x)
         end
       else (f x))

let plift_rr b i n t1 t2 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y ->     
     if not (RxImpl.match_string t1 x) then 
       Berror.type_error i (RxImpl.split_bad_prefix t1 x)
     else if not (RxImpl.match_string t2 y) then 
       Berror.type_error i (RxImpl.split_bad_prefix t2 y)
     else (f x y))

let plift_rsd b i n t1 st2 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y z ->     
     if not (RxImpl.match_string t1 x) then 
       Berror.type_error i (RxImpl.split_bad_prefix t1 x)
     else if not (st2 y) then 
       assert false
     else (f x y z))
    
let plift_rd b i n t1 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y ->     
     if not (RxImpl.match_string t1 x) then 
       begin 
         Util.format "T1=";
         Bregexp.format_t t1;
         Util.format"@\n";
         Berror.type_error i (RxImpl.split_bad_prefix t1 x)
       end
     else (f x y))

let plift_rrd b i n t1 t2 t3 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y z ->     
     if not (RxImpl.match_string t1 x) then 
       Berror.type_error i (RxImpl.split_bad_prefix t1 x)
     else if not (RxImpl.match_string t2 y) then 
       Berror.type_error i (RxImpl.split_bad_prefix t2 y)
     else (f x y z))

let plift_rx b i n t1 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y ->     
     if not (RxImpl.match_string t1 x) then 
       Berror.type_error i (RxImpl.split_bad_prefix t1 x)
     else (f x y))

let plift_rrx b i n t1 t2 f = 
  if not (b || Prefs.read paranoid) then f else
  (fun x y z ->     
     if not (RxImpl.match_string t1 x) then 
       Berror.type_error i (RxImpl.split_bad_prefix t1 x)
     else if not (RxImpl.match_string t2 y) then 
       Berror.type_error i (RxImpl.split_bad_prefix t1 y)
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
  | S_dup of skeleton
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

let dup_of_skel i = function
  | S_dup s -> s
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
  let ps = RxImpl.split_positions t1 t2 s in 
  let n = String.length s in 
  let j = choose ps in 
  (String.sub s 0 j, String.sub s j (n-j))

(* helpers for concat-like operators *)
  let seq_split i t1 t2 w = match RxImpl.seq_split t1 t2 w with
    | None -> 
        Berror.run_error i 
          (fun () -> Util.format "the concatenation of@\nT1=%s@\nand@\nT2=%s@\nwas ambiguous on@\nW=%s@\n" 
             (RxImpl.string_of_t t1) (RxImpl.string_of_t t2) w;
          Util.format "[%s]@\n" (Misc.concat_list "," 
                                   (Safelist.map string_of_int (Int.Set.elements (RxImpl.split_positions t1 t2 w)))))
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
    "" (RxImpl.star_split t x) 

(* helpers for conditional operators *)
let branch t f1 f2 = 
  (fun x -> 
     if RxImpl.match_string t x then f1 x 
     else f2 x) 

let branch2 t f1 f2 = 
  (fun x y ->
     if RxImpl.match_string t x then f1 x y 
     else f2 x y)

(* helpers for permutations *)
let rec build_list first last = 
  if first = last
  then []
  else first::(build_list (first + 1) last)

let valid_permutation sigma ls =
  let k = Safelist.length sigma in
  Safelist.length ls = k &&
  Safelist.sort compare sigma = build_list 0 k

let rec permutations_aux k =
  let rec insertions n ls =
    let (is,_) = Safelist.fold_left 
      (fun (ls_n_acc,ls_acc) i ->
	 let ls_acc' = i::ls_acc in
	 let ls_n_acc' = Safelist.map (fun ls_n -> i::ls_n) ls_n_acc in
	 ((n::ls_acc')::ls_n_acc',ls_acc'))
      ([[n]],[]) ls in
    is in
  if k = 0
  then [[]]
  else Safelist.concat (Safelist.map (insertions (k-1)) (permutations_aux (k-1)))

let permutations k = 
  (* N.B. we make sure the identity is first -- this will give us a nicer 
     default for the create case of lenses that use permutations to build 
     sorting *)
  let identity = build_list 0 k in
  identity::(Safelist.remove identity (permutations_aux k))

let permutation i n sigma k = 
  let err () = 
    Berror.static_error i n 
      (sprintf "[%s] is not a permutation on {0,..,%d}@\n" 
         (Misc.concat_list "," (Safelist.map string_of_int sigma)) 
         (pred k)) in 
  let sigma_arr = Array.create k (-1) in 
  let sigma_inv_arr = Array.create k (-1) in       
  let () = 
    let k' = Safelist.fold_left 
      (fun i j -> 
         sigma_arr.(i) <- j;
         if sigma_inv_arr.(j) <> (-1) then err ();
         sigma_inv_arr.(j) <- i;
         succ i)
      0 sigma in
    if k' <> k then err () in
  (sigma_arr,sigma_inv_arr) 

let invert_permutation i sigma = 
  let n = sprintf "invert_permutation [%s]" 
    (Misc.concat_list "," (Safelist.map string_of_int sigma)) in  
  let err () = 
    Berror.static_error i n
      (sprintf "[%s] is not a valid permutation\n" 
         (Misc.concat_list "," (Safelist.map string_of_int sigma))) in
  let sigma_arr = Array.of_list sigma in
  let sigma_inv_arr = Array.create (Array.length sigma_arr) (-1) in
  Array.iteri 
    (fun i j -> 
       if sigma_inv_arr.(j) <> -1 then err ();
       sigma_inv_arr.(j) <- i)
    sigma_arr;
  Array.to_list sigma_inv_arr

let permute_list i sigma ls =
  let n = sprintf "permute_list [%s] [...]" 
    (Misc.concat_list "," (Safelist.map string_of_int sigma)) in  
  let ls_arr = Array.of_list ls in
  let k = Array.length ls_arr in
  let _,sigma_inv_arr = permutation i n sigma k in
    Array.fold_right 
      (fun j ls' -> ls_arr.(j)::ls')
      sigma_inv_arr []
    
(* uid *)
type uid = int
let current_uid = ref 0
let next_uid () = 
  incr current_uid;
  !current_uid


(* simple sort checking for relations *)
type rel = Identity | Unknown
let combine_rel r1 r2 = match r1,r2 with 
  | Identity,Identity -> Identity 
  | _                 -> Unknown

(* -----------------------------------------------------------------------------*)
(* CANONIZERS *)
module Canonizer = struct
  type t = 
      { (* --- meta data --- *)
        info: Info.t;
        string: string;
        (* --- types --- *)
        uncanonized_type : RxImpl.t;
        canonized_type : RxImpl.t;
        crel  : rel;
        (* --- core functions --- *)
        canonize : string -> string;
        choose : string -> string;
      }

  (* accessors *)
  let info cn = cn.info
  let string cn = cn.string
  let uncanonized_type cn = cn.uncanonized_type
  let canonized_type cn = cn.canonized_type
  let cnrel_identity cn = cn.crel = Identity
  let canonize cn = cn.canonize
  let choose cn = cn.choose
  let mk_t i s rt ct r cn ch = 
    { info = i;
      string = s;
      uncanonized_type = rt;
      canonized_type = ct;
      crel = r;
      canonize = cn;
      choose = ch; 
    }

  let copy i r = 
    let n = sprintf "copy (%s)" (RxImpl.string_of_t r) in 
    let rt = r in 
    let ct = r in
      { info = i;
        string = n;
        uncanonized_type = rt;
        canonized_type = ct;
        crel = Identity;
        canonize = (fun x -> x);
        choose = (fun x -> x);
      }

  (* add primitives ... *)
  let columnize i k r ch nl =
    let n = sprintf "columnize %d (%s) (%c) (%s)" k (RxImpl.string_of_t r) ch nl in
    let () =
      let contains_nl = Bregexp.mk_seq Bregexp.anything (Bregexp.mk_seq (Bregexp.mk_string nl) Bregexp.anything) in
      let r_contains_nl = not (Bregexp.is_empty (Bregexp.mk_inter r contains_nl)) in
      if r_contains_nl then Berror.static_error i n (sprintf "%s contains %s" (Bregexp.string_of_t r) nl) in 
    let nl_len = String.length nl in
    let ct = r in
    let rt = Bregexp.mk_expand r (Char.code ch) (Bregexp.mk_string nl) in
    let matches s c i =
      let s_len = String.length s in
      let c_len = String.length c in
      let rec aux j =
        if j=s_len then true
        else
          (i+j < c_len)
          && ((String.get c (i+j)) = (String.get s j))
          && (aux (succ j)) in
        aux 0 in
      { info = i;
        string = n;
        uncanonized_type = rt;
        canonized_type = ct;
        crel = Identity;
        canonize =
          
          (fun c ->
             let c_len = String.length c in
             let buf = Buffer.create c_len in
             let rec loop i =
               if i = c_len then ()
               else
                 if matches nl c i then
                   (Buffer.add_char buf ch;
                    loop (i + nl_len))
                 else
                   (Buffer.add_char buf (String.get c i);
                    loop (succ i)) in
               loop 0;
               Buffer.contents buf);

        choose =
          (fun c ->
             let c_len = String.length c in
             let buf = Buffer.create c_len in
             let line_buf = Buffer.create k in
             let aux_buf = Buffer.create k in
             let do_line () =
               if Buffer.length buf <> 0 && Buffer.length line_buf <> 0 then Buffer.add_string buf nl;
               Buffer.add_buffer buf line_buf;
               Buffer.reset line_buf in
             let do_space () =
               if Buffer.length line_buf <> 0 then Buffer.add_char line_buf ch;
               Buffer.add_buffer line_buf aux_buf;
               Buffer.reset aux_buf in
             let rec loop i =
               let sum =
                 let nl_off = if Buffer.length buf=0 then 0 else pred nl_len in
                 let aux_len = Buffer.length aux_buf in
                 let line_len = let n = Buffer.length line_buf in if n=0 then n else succ n in
                   nl_off + aux_len + line_len in
                 if sum > k then do_line ();
                 if i = c_len then (do_space (); do_line ())
                 else
                   let i' =
                     if ch = c.[i] then
                       (do_space (); i + 1)
                     else (Buffer.add_char aux_buf (String.get c i); succ i) in
                     loop i' in
               loop 0;
              Buffer.contents buf) }
        
  let concat i cn1 cn2 = 
    let n = sprintf "concat (%s) (%s)" cn1.string cn2.string in 
    let rt = 
      if Prefs.read notypecheck then 
        RxImpl.mk_seq cn1.uncanonized_type cn2.uncanonized_type 
      else
        match RxImpl.splittable_cex cn1.uncanonized_type cn2.uncanonized_type with 
          | Misc.Right rt12 -> rt12 
          | Misc.Left(s1,s2,s1',s2') -> 
              let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
                cn1.string cn2.string s1 s2 s1' s2' in 
                Berror.static_error i n s in 
    let ct = match cn1.crel,cn2.crel with 
      | Identity,Identity -> RxImpl.mk_seq cn1.canonized_type cn2.canonized_type 
      | _ -> match RxImpl.splittable_cex cn1.canonized_type cn2.canonized_type with 
          | Misc.Right ct12 -> ct12 
          | Misc.Left(s1,s2,s1',s2') -> 
              let s = sprintf "the concatenation of %s and %s is ambiguous (and so the relation may not be an equivalence):\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
                cn1.string cn2.string s1 s2 s1' s2' in 
                Berror.static_error i n s in   
      { info = i;
        string = n;
        uncanonized_type = rt;
        canonized_type = ct;
        crel = combine_rel cn1.crel cn2.crel;
        canonize = 
          (do_split (seq_split i cn1.uncanonized_type cn2.uncanonized_type)
             cn1.canonize cn2.canonize (^));
        choose = 
          (do_split ((split_one Int.Set.min_elt) cn1.canonized_type cn2.canonized_type)
             cn1.choose cn2.choose (^));
      }

  let union i cn1 cn2 = 
    let n = sprintf "union (%s) (%s)" cn1.string cn2.string in 
    let () = 
      if not (Prefs.read notypecheck) then  
        match RxImpl.disjoint_cex cn1.uncanonized_type cn2.uncanonized_type with
          | None -> ()
          | Some w -> 
              let s = sprintf "%s and %s are not disjoint: %s"
                (RxImpl.string_of_t cn1.uncanonized_type) (RxImpl.string_of_t cn2.uncanonized_type) w in
                Berror.static_error i n s in
      (*     let () = match cn1.crel,cn2.crel with  *)
      (*       | Identity,Identity -> () *)
      (*       | _ -> match RxImpl.disjoint_cex cn1.canonized_type cn2.canonized_type with *)
      (*       | None -> () *)
      (*       | Some w ->  *)
      (*           let s = sprintf "%s and %s are not disjoint (and so the relation may not be the identity): %s" *)
      (*             (RxImpl.string_of_t cn1.uncanonized_type) (RxImpl.string_of_t cn2.uncanonized_type) w in *)
      (*           Berror.static_error i n s in *)
    let rt = RxImpl.mk_alt cn1.uncanonized_type cn2.uncanonized_type in 
    let ct = RxImpl.mk_alt cn1.canonized_type cn2.canonized_type in 
    let cr = 
      if RxImpl.is_empty (RxImpl.mk_inter cn2.canonized_type (RxImpl.mk_complement cn1.canonized_type)) then 
        cn1.crel
      else combine_rel cn1.crel cn2.crel in  
    { info = i;
      string = n;
      uncanonized_type = rt;
      canonized_type = ct;
      crel = cr;
      canonize = (branch cn1.uncanonized_type cn1.canonize cn2.canonize);
      choose = (branch cn1.canonized_type cn1.choose cn2.choose);
    }

  let star i cn1 =
    let n = sprintf "(%s)*" cn1.string in
    let rt = 
      if Prefs.read notypecheck then 
        RxImpl.mk_star cn1.uncanonized_type 
      else match RxImpl.iterable_cex cn1.uncanonized_type with
        | Misc.Right r -> r
        | Misc.Left (s1,s2,s1',s2') -> 
            let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t cn1.uncanonized_type) s1 s2 s1' s2' in 
              Berror.static_error i n s in 
    let ct = match Prefs.read notypecheck, cn1.crel with 
      | true,_ | _,Identity -> RxImpl.mk_star cn1.canonized_type 
      | _ -> 
          match RxImpl.iterable_cex cn1.canonized_type with 
            | Misc.Right cts -> cts 
            | Misc.Left(s1,s2,s1',s2') -> 
                let s = sprintf "the iteration of %s is ambiguous (and so the relation may not be an equivalence):\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
                  cn1.string s1 s2 s1' s2' in 
                  Berror.static_error i n s in   
      { info = i;
        string = n;
        uncanonized_type = rt;
        canonized_type = ct;
        crel = cn1.crel;
        canonize = (star_loop cn1.uncanonized_type cn1.canonize);      
        choose = 
          (fun cl -> 
             let rec loop cl acc = 
               if String.length cl = 0 then acc
               else
                 let cl1,clrest = (split_one Int.Set.min_elt) cn1.canonized_type ct cl in 
                   loop clrest (acc ^ (cn1.choose cl1)) in 
               loop cl "");
      }
        
  let normalize i f fc fc0 = 
    (* UNCHECKED: f's type, that f is the identity on fc0 :-/ *)
    let n = sprintf "normalize(<function>: %s -> %s)" 
      (RxImpl.string_of_t fc) (RxImpl.string_of_t fc0) in
    let rt = fc in 
    let ct = fc0 in 
      { info = i;
        string = n;
        uncanonized_type = rt;
        crel = Identity;
        canonized_type = ct;
        canonize = (fun c -> f c);
        choose = (fun c -> c);
      }

  let sort i rl = 
    let n = sprintf "sort(%s)" (Misc.concat_list "," (Safelist.map (fun ri -> RxImpl.string_of_t ri) rl)) in 
    let k,rl_idx_rev = Safelist.fold_left (fun (i,acc) ri -> (succ i,(i,ri)::acc)) (0,[]) rl in
    let rt_alts = 
      Safelist.fold_left 
        (fun acc ri -> 
           if Prefs.read notypecheck then RxImpl.mk_alt acc ri 
           else match RxImpl.disjoint_cex acc ri with
             | None -> RxImpl.mk_alt acc ri 
             | Some w -> 
                 Berror.static_error i n 
                   (sprintf "types %s and %s are not disjoint: %s"
                      (RxImpl.string_of_t acc) (RxImpl.string_of_t ri) w))
        Bregexp.empty rl in      
    let rt = 
      if Prefs.read notypecheck then 
        RxImpl.mk_star rt_alts
      else match RxImpl.iterable_cex rt_alts with 
        | Misc.Right r -> r
        | Misc.Left(s1,s2,s1',s2') ->             
            Berror.static_error i n 
              (sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
                 (RxImpl.string_of_t rt_alts) s1 s2 s1' s2') in
    let ct = Safelist.fold_left RxImpl.mk_seq RxImpl.epsilon rl in
      { info = i;
        string = n;
        uncanonized_type = rt;
        canonized_type = ct;
        crel = Identity;
        canonize = 
          (fun c -> 
             let cl = RxImpl.star_split rt_alts c in
             let c_arr = Array.create k "" in 
             if Safelist.length cl <> k then 
               Berror.static_error i n 
                 (sprintf "string %s did not split into exactly %d pieces" c k);
             let _ = Safelist.fold_left 
               (fun rs ci -> 
                  (* INEFFICIENT! *)
                  match Safelist.partition (fun (_,ri) -> RxImpl.match_string ri ci) rs with
                    | [i,_],rs' -> 
                        c_arr.(i) <- ci; 
                        rs'
                    | _,_ -> 
                        Berror.static_error i n 
                          (sprintf "%s did not match exactly one regexp" ci))
               rl_idx_rev cl in 
             concat_array c_arr);
        choose = (fun c -> c);
      }
      

  let iter i cn1 min maxo = 
    generic_iter i 
      (copy i RxImpl.epsilon) (union i) (concat i) (star i) 
      cn1 min maxo
end

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
        bij:bool;                                           (* bijective flag *)
        ctype: RxImpl.t;                                    (* concrete type *)
        atype: RxImpl.t;                                    (* abstract type *)
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
  let crel_identity dl = dl.crel = Identity
  let arel_identity dl = dl.arel = Identity
  let bij dl = dl.bij
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
    
  let mk_t i s bj ct at xto dt st cr ar g put parse c k uid = 
    { info = i;
      string = s;
      bij = bj;
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
      dl.arel
      dl.get
      (rcreate dl)
      
  (* invert -- only for bijective lenses! *)
  let invert i dl = 
    let n = sprintf "invert (%s)" dl.string in 
    let () = if not dl.bij then 
        Berror.static_error i n 
          (sprintf "cannot invert non-bijective lens %s" dl.string) in 
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
    let n = sprintf "cp (%s)" (RxImpl.string_of_t r) in 
    let ct = r in 
    let at = r in
    let xto = Some (Erx.mk_leaf at) in 
    let dt = TMap.empty in
    let st = function
      | S_string s -> RxImpl.match_string r s
      | _ -> false in
      { info = i;
        string = n;
        bij = true;
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
    let n = sprintf "const (%s) \"%s\" \"%s\"" (RxImpl.string_of_t r) (whack u) (whack def) in 
    let ct = r in 
    let bij = RxImpl.is_singleton r in
    let at = (RxImpl.mk_string u) in 
    let xto = Some (Erx.mk_leaf at) in 
    let dt = TMap.empty in
    let st = function
      | S_string s -> RxImpl.match_string r s
      | _ -> false in
    let () = 
      if not (RxImpl.match_string r def) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def (RxImpl.string_of_t r)) in 
      { info = i;
        string = n;
        bij = bij;
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
    let bij = dl1.bij && dl2.bij in 
    let ct = 
      if Prefs.read notypecheck then RxImpl.mk_seq dl1.ctype dl2.ctype 
      else match RxImpl.splittable_cex dl1.ctype dl2.ctype with
        | Misc.Right r -> r
        | Misc.Left(s1,s2,s1',s2') -> 
            let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t dl1.ctype) (RxImpl.string_of_t dl2.ctype) s1 s2 s1' s2' in 
              Berror.static_error i n s in 
    let at = 
      if Prefs.read notypecheck then RxImpl.mk_seq dl1.atype dl2.atype 
      else match RxImpl.splittable_cex dl1.atype dl2.atype with
        | Misc.Right r -> r
        | Misc.Left (s1,s2,s1',s2') -> 
            let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t dl1.atype) (RxImpl.string_of_t dl2.atype) s1 s2 s1' s2' in 
              Berror.static_error i n s in 
    let xto = Misc.map2_option (fun x1 x2 -> Erx.mk_seq x1 x2) dl1.xtype dl2.xtype in
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in 
    let st = function
      | S_concat (s1, s2) -> dl1.stype s1 && dl2.stype s2
      | _ -> false in
      (* lens *) 
      { info = i; 
        string = n;
        bij = bij;
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

  let common_union i dl1 dl2 n b = 
    (* TODO: check equality of keys *)    
    let () = match RxImpl.disjoint_cex dl1.ctype dl2.ctype with 
      | None -> ()
      | Some w -> 
          let s = sprintf "concrete types %s and %s are not disjoint: %s"
            (RxImpl.string_of_t dl1.ctype) (RxImpl.string_of_t dl2.ctype) w in
        Berror.static_error i n s in
    let ct = RxImpl.mk_alt dl1.ctype dl2.ctype in 
    let at = RxImpl.mk_alt dl1.atype dl2.atype in 
    let xto = Misc.map2_option (fun x1 x2 -> Erx.mk_alt x1 x2) dl1.xtype dl2.xtype in
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in
    let st s = dl1.stype s || dl2.stype s in
      { info = i;
        string = n;
        bij = b; 
        ctype = ct;         
        atype = at;
        xtype = xto;
	dtype = dt;
	stype = st;
	crel = combine_rel dl1.crel dl2.crel;
        arel = combine_rel dl1.arel dl2.arel;
        get = 
          (branch dl1.ctype dl1.get dl2.get);
        put = 
          (fun a s d -> 
             match RxImpl.match_string dl1.atype a, 
               RxImpl.match_string dl2.atype a,
               dl1.stype s with
                 | true,_,true  -> dl1.put a s d
                 | _,true,false -> dl2.put a s d
                 | true,false,false -> dl1.create a d 
                 | false,true,true  -> dl2.create a d
                 | false,false,_    -> assert false);
	parse = 
	  (branch dl1.ctype dl1.parse dl2.parse); 
        create =
          (branch2 dl1.atype dl1.create dl2.create);
	key =
	  (branch dl1.atype dl1.key dl2.key);
	uid = next_uid ();
      }

  let union i dl1 dl2 =     
    let n = sprintf "(%s||%s)" dl1.string dl2.string in 
    let bij = dl1.bij && dl2.bij && RxImpl.disjoint dl1.atype dl2.atype in
    let () = match dl1.arel,dl2.arel with
      | _,Unknown
      | Unknown,_ -> 
          let s = sprintf "the union of %s and %s is ill-typed: %s"            
            dl1.string dl2.string 
            "the relations must both be the identity" in 
            Berror.static_error i n s
      | _ -> () in 
     common_union i dl1 dl2 n bij

  let disjoint_union i dl1 dl2 = 
    let n = sprintf "(%s|%s)" dl1.string dl2.string in 
    let () = 
      if not (Prefs.read notypecheck) then 
        match RxImpl.disjoint_cex dl1.atype dl2.atype with 
          | None -> ()
          | Some w -> 
              Berror.static_error i n 
                (sprintf "abtract types %s and %s are not disjoint: %s"
                   (RxImpl.string_of_t dl1.atype) (RxImpl.string_of_t dl2.atype) w) in 
    let bij = dl1.bij && dl2.bij in 
    common_union i dl1 dl2 n bij
        
  let star i dl1 = 
    (* body *)
    let n = sprintf "(%s)*" dl1.string in
    let bij = dl1.bij in 
    let ct = 
      if Prefs.read notypecheck then RxImpl.mk_star dl1.ctype 
      else match RxImpl.iterable_cex dl1.ctype with
        | Misc.Right r -> r
        | Misc.Left (s1,s2,s1',s2') -> 
          let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
            (RxImpl.string_of_t dl1.ctype) s1 s2 s1' s2' in 
            Berror.static_error i n s in       
    let at = 
      if Prefs.read notypecheck then RxImpl.mk_star dl1.atype 
      else match RxImpl.iterable_cex dl1.atype with
        | Misc.Right r -> r
        | Misc.Left(s1,s2,s1',s2') -> 
            let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t dl1.atype) s1 s2 s1' s2' in
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
        bij = bij;
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
                 (RxImpl.star_split dl1.atype a)
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
               ("", d) (RxImpl.star_split dl1.atype a));
	parse = lift_r i (parse_str n) ct 
          (fun c ->
             let (sl, d) = Safelist.fold_left 
               (fun (buf, d) c1 -> 
                  let s1,d1 = dl1.parse c1 in
	          let buf' = s1::buf  in
	          let d' = d ++ d1 in
	            (buf', d'))
               ([], empty_dict)
               (RxImpl.star_split dl1.ctype c) in
	       (S_star (Safelist.rev sl), d));
	key = lift_r i (key_str n) at (star_loop dl1.atype dl1.key);
	uid = next_uid ();
      }

  let iter i l1 min maxo = 
    generic_iter i 
      (copy i RxImpl.epsilon) (disjoint_union i) (concat i) (star i) 
      l1 min maxo

  (* non-standard lenses *)
  let permute info sigma dll = 
    let n = sprintf "permute([%s],[%s])" 
      (Misc.concat_list "," (Safelist.map string_of_int sigma))
      (Misc.concat_list "," (Safelist.map (fun dli -> dli.string) dll)) in  
    let k = Safelist.length dll in 
    let sigma_arr,sigma_inv_arr = permutation info n sigma k in 
    let dl_arr = Array.of_list dll in 
    let bij,ct,dt,crel,arel = 
      (* calculate lens type components *)
      Array.fold_left 
        (fun (b,c,d,cr,ar) dli -> 
           (b && dli.bij,
            (if Prefs.read notypecheck then RxImpl.mk_seq c dli.ctype 
             else match RxImpl.splittable_cex c dli.ctype with
               | Misc.Right c' -> c'
               | Misc.Left(s1,s2,s1',s2') ->
                   Berror.static_error info n 
                     (sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
                        (RxImpl.string_of_t c) (RxImpl.string_of_t dli.ctype) s1 s2 s1' s2')),            
            safe_merge_dict_type info d dli.dtype,            
            combine_rel cr dli.crel,
            combine_rel ar dli.arel))        
        (true,RxImpl.epsilon,TMap.empty,Identity,Identity) dl_arr in
    (* calculate concrete types to use in splitting *)
    let ct_rest_array = Array.create k RxImpl.empty in
    let _ = Array.fold_right 
      (fun dli (i,acc) -> 
         ct_rest_array.(i) <- acc; 
         (pred i,RxImpl.mk_seq dli.ctype acc)) 
      dl_arr (pred k,RxImpl.epsilon) in
    let split_c c =
      let res = Array.create k "" in 
      let _ = 
        Array.fold_left 
          (fun (j,c) dlj -> 
             let cj,crest = seq_split info dlj.ctype ct_rest_array.(j) c in
               res.(j) <- cj;
               (succ j,crest))
          (0,c) dl_arr in 
      res in
    let at =
      Array.fold_left
        (fun acc j ->
           if Prefs.read notypecheck then RxImpl.mk_seq acc dl_arr.(j).atype 
           else match RxImpl.splittable_cex acc dl_arr.(j).atype with
             | Misc.Right acc' -> acc'
             | Misc.Left(s1,s2,s1',s2') ->
                 Berror.static_error info n
                   (sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
                      (RxImpl.string_of_t acc) (RxImpl.string_of_t dl_arr.(j).atype) s1 s2 s1' s2'))
        RxImpl.epsilon sigma_inv_arr in 
    let at_rest_array = Array.create k RxImpl.empty in 
    let _ = 
      Array.fold_right 
        (fun j (i,acc) -> 
           at_rest_array.(i) <- acc;
           (pred i,RxImpl.mk_seq dl_arr.(j).atype acc))
        sigma_inv_arr (pred k,RxImpl.epsilon) in
    let split_a a =
      let res = Array.create k "" in 
      let _ = Array.fold_left
        (fun (i,a) j -> 
           let ai,arest = seq_split info dl_arr.(j).atype at_rest_array.(i) a in
           res.(i) <- ai;
           (succ i,arest))
        (0,a) sigma_inv_arr in
      res in
    let xto =
      Array.fold_left
        (fun acco i ->
           let dli = dl_arr.(i) in
           Misc.map2_option (fun x1 x2 -> Erx.mk_seq x1 x2) acco dli.xtype)
        (Some (Erx.mk_leaf Bregexp.epsilon)) sigma_arr in
    let st s =
      let rec loop dll s = match dll,s with
        | [],S_string "" -> true
        | [dli],_ -> dli.stype s
        | dli::dlrest,S_concat(si,srest) -> dli.stype si && loop dlrest srest
        | _ -> false in
      loop dll s in
    { info = info;
      uid = next_uid ();
      string = n;
      bij = bij;
      ctype = ct;
      atype = at;
      xtype = xto;
      dtype = dt;
      stype = st;
      crel = crel;
      arel = arel;
      get = lift_r info n ct
        (fun c ->
           let c_arr = split_c c in 
           let a_arr = Array.create k "" in
           let rec loop i = 
             if i >= k then ()
             else 
               begin 
                 let j = sigma_arr.(i) in 
                 let ai = dl_arr.(i).get c_arr.(i) in 
                 a_arr.(j) <- ai;
                 loop (succ i)
               end in
           loop 0;
           let a' = concat_array a_arr in 
           a');
      put = lift_rsd info (put_str n) at st 
        (fun a s d -> 
           let a_arr = split_a a in 
           let c_arr = Array.create k "" in 
           let rec loop j s d = 
             if j >= k then d
             else
               begin 
                 let sj = fst_concat_of_skel info s in 
                 let i = sigma_inv_arr.(j) in 
                 let ci,di = dl_arr.(i).put a_arr.(j) sj d in
                 c_arr.(i) <- ci;
                 loop (succ j) (snd_concat_of_skel info s) di 
               end in
           let d' = loop 0 s d in 
           let c' = concat_array c_arr in
           (c',d'));
      create = lift_rd info (create_str n) at 
        (fun a d -> 
           let a_arr = split_a a in 
           let c_arr = Array.create k "" in 
           let rec loop j d = 
             if j >= k then d
             else 
               begin 
                 let i = sigma_inv_arr.(j) in 
                 let ci,di = dl_arr.(i).create a_arr.(j) d in 
                 c_arr.(i) <- ci;
                 loop (succ j) di 
               end in
           let d' = loop 0 d in
           let c' = concat_array c_arr in 
           (c',d'));
      parse = lift_r info (parse_str n) ct 
        (fun c -> 
           let c_arr = split_c c in 
           let s_arr = Array.create k (S_string "") in 
           let d_arr = Array.create k TMap.empty in
           let rec loop i =
             if i < 0 then () 
             else
               begin 
                 let j = sigma_arr.(i) in 
                 let si,di = dl_arr.(i).parse c_arr.(i) in 
                 d_arr.(j) <- di;
                 s_arr.(j) <- si;
                 loop (pred i)
               end in
           loop (pred k);
           let s' = Array.fold_right (fun si sacc -> S_concat(si,sacc)) s_arr (S_string "") in 
           let d' = Array.fold_right (fun di dacc -> di++dacc) d_arr TMap.empty in 
           (s',d'));
      key = lift_r info n at 
        (fun a -> 
           let a_arr = split_a a in 
           let k_buf = Buffer.create 17 in 
           let rec loop j = 
             if j >= k then ()
             else 
               begin 
                 let kj = dl_arr.(j).key a_arr.(j) in 
                 Buffer.add_string k_buf kj;
                 loop (succ j) 
               end in
           loop 0;
           let ky = Buffer.contents k_buf in 
           ky);
    }

  let compose i dl1 dl2 = 
    let n = sprintf "%s; %s" dl1.string dl2.string in 
    let bij = dl1.bij && dl2.bij in 
    let ct = dl1.ctype in
    let at = dl2.atype in 
    let xto = dl2.xtype in 
    let dt = safe_merge_dict_type i dl1.dtype dl2.dtype in
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
      if (not (Prefs.read notypecheck)) && (not (RxImpl.equiv dl1.atype dl2.ctype)) then
	Berror.static_error i n (sprintf "the composition of %s and %s is ill-typed" dl1.string dl2.string);
    { info = i; 
      string = n;
      bij = bij;
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
      uid = next_uid ();
    }

  let default i def dl1 = 
    let n = sprintf "default %s %s" def dl1.string in 
    let at = dl1.atype in 
    let () = 
      if not (RxImpl.match_string dl1.ctype def) then 
        Berror.static_error i n 
          (sprintf "%s does not belong to %s" def
             (RxImpl.string_of_t dl1.ctype)) in 
    let s,d = dl1.parse def in
      { dl1 with
          create = lift_rd i (create_str n) at (fun a d' -> dl1.put a s (d' ++ d)); 
          (* FINISH: think carefully about order of dictionary smashing here *)
	  uid = next_uid ();
      }

  let dmatch i lookup_fun tag dl1 = 
    let n = sprintf "<%s>" dl1.string in
    let bij = dl1.bij in 
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
        bij = bij;
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
    let n = sprintf "filter %s %s" (RxImpl.string_of_t rd) (RxImpl.string_of_t rk) in 
    let () = match RxImpl.disjoint_cex rd rk with
      | None -> ()
      | Some w -> 
          let s = sprintf "%s and %s are not disjoint: %s"
            (RxImpl.string_of_t rd) (RxImpl.string_of_t rk) w in
        Berror.static_error i n s in
    let ru = RxImpl.mk_alt rd rk in
    let ct = 
      if Prefs.read notypecheck then RxImpl.mk_star ru 
      else match RxImpl.iterable_cex ru with
        | Misc.Right r -> r
        | Misc.Left(s1,s2,s1',s2') -> 
            let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t ru) s1 s2 s1' s2' in 
              Berror.static_error i n s in 
    let at = 
      if Prefs.read notypecheck then RxImpl.mk_star rk 
      else match RxImpl.iterable_cex rk with
        | Misc.Right r -> r
        | Misc.Left(s1,s2,s1',s2') -> 
            let s = sprintf "the iteration of %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t rk) s1 s2 s1' s2' in 
            Berror.static_error i n s in 
    let xto = Some (Erx.mk_leaf at) in 
    let dt = TMap.empty in
    let st = function
      | S_string s -> RxImpl.match_string ct s
      | _ -> false in
    let get = lift_r i (get_str n) ct 
      (fun c ->
         let rec loop acc = function 
           | [] -> acc
           | h :: t -> 
               if RxImpl.match_string rd h then 
                 loop acc t
               else 
                 loop (acc ^ h) t in
         let lc = RxImpl.star_split ct c in
           loop "" lc) in
    let put = lift_rsd i (put_str n) at st
      (fun a s d ->
         let c = string_of_skel i s in
         let rec loop acc lc la = match lc,la with
           | [], [] -> acc
           | [], ha :: ta -> loop (acc ^ ha) [] ta
           | hc :: tc, [] -> 
               if RxImpl.match_string rd hc then
                 loop (acc ^ hc) tc []
               else
                 loop acc tc []
           | hc :: tc, ha :: ta -> 
               if RxImpl.match_string rd hc then
                 loop (acc ^ hc) tc la
               else
                 loop (acc ^ ha) tc ta in
         let lc = RxImpl.star_split ct c in
         let la = RxImpl.star_split at a in
           (loop "" lc la, d)) in
    let create = lift_rd i n at (fun a d -> (a,d)) in
    let parse = lift_r i n ct (fun c -> (S_string c, empty_dict))in
      { info = i; 
        string = n;
        bij = false;
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
    let () = 
      if (not (Prefs.read notypecheck)) && (not (RxImpl.equiv (Canonizer.canonized_type cn) dl.ctype)) then 
        Berror.static_error i n (sprintf "%s is ill-typed" n) in 
    let () = match cn.Canonizer.crel,dl.crel with
      | Unknown, Unknown | Unknown,Identity -> 
          Berror.static_error i n (sprintf "%s is ill-typed" n)
      | _ -> () in 
    let bij = dl.bij in 
    let ct = Canonizer.uncanonized_type cn in
    let at = dl.atype in
    let xto = dl.xtype in 
    let dt = dl.dtype in
    let st = dl.stype in
    let canonize = Canonizer.canonize cn in
    let choose = Canonizer.choose cn in
      { info = i; 
        string = n;
        bij = bij;
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
    let () = 
      if (not (Prefs.read notypecheck)) && (not (RxImpl.equiv (Canonizer.canonized_type cn) dl.atype)) then
        Berror.static_error i n (sprintf "%s is ill-typed" n) in 
    let () = match cn.Canonizer.crel,dl.crel with
      | Unknown, Unknown -> 
          Berror.static_error i n (sprintf "%s is ill-typed" n)
      | Unknown,Identity -> 
          Berror.static_error i n (sprintf "%s is ill-typed" n)          
      | _ -> () in 
  let bij = dl.bij in 
  let ct = dl.ctype in
  let at = Canonizer.uncanonized_type cn in
  let xto = dl.xtype in 
  let dt = dl.dtype in
  let st = dl.stype in
  let canonize = Canonizer.canonize cn in
  let choose = Canonizer.choose cn in
    { info = i; 
      string = n;
      bij = bij;
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

  let dup i fst dl f fat = 
    let n = sprintf "dup%d(%s,<function> : %s -> %s)" (if fst then 1 else 2) dl.string (RxImpl.string_of_t (dl.ctype)) (RxImpl.string_of_t fat) in
    let bij = dl.bij in 
    let ct = dl.ctype in 
    let left_at = if fst then dl.atype else fat in 
    let right_at = if fst then fat else dl.atype in 
    let at = 
      if Prefs.read notypecheck then RxImpl.mk_seq left_at right_at 
      else match RxImpl.splittable_cex left_at right_at with
        | Misc.Right r -> r
        | Misc.Left(s1,s2,s1',s2') -> 
            let s = sprintf "the concatenation of %s and %s is ambiguous:\n\"%s\" and \"%s\"\nand also\n\"%s\" and \"%s\""
              (RxImpl.string_of_t left_at) (RxImpl.string_of_t right_at) s1 s2 s1' s2' in 
              Berror.static_error i n s in 
    let xto = Misc.map_option (fun x1 -> Erx.mk_seq x1 (Erx.mk_leaf fat)) dl.xtype in 
    let dt = dl.dtype in 
    let st = function
      | S_dup s -> dl.stype s 
      | _ -> false in
    let split_a a = 
      let a1,a2 = seq_split i left_at right_at a in
      if fst then a1 else a2 in 
    (* lens *)
    { info = i;
      uid = next_uid (); 
      string = n;
      bij = bij;
      ctype = ct;
      atype = at;
      xtype = xto;
      dtype = dt;
      stype = st;
      crel = dl.crel;
      arel = Unknown;
      get = lift_r i (get_str n) ct 
        (fun c -> 
           if fst then (dl.get c) ^ (f c) 
           else (f c) ^ (dl.get c));
      put = lift_rsd i (put_str n) at st 
        (fun a s d ->
           let ai = split_a a in 
	   dl.put ai (dup_of_skel i s) d);      
      parse = lift_r i (parse_str n) ct 
        (fun c ->
	   let s,d = dl.parse c in
	     (S_dup s,d));
      create = lift_rd i n at 
        (fun a d ->
           let ai = split_a a in 
	   dl.create ai d);	       
      key = lift_r i n at 
        (fun a ->
           let ai = split_a a in 
           dl.key ai); }
end
