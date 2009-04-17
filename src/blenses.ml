(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /boomerang/src/blenses.ml                                                  *)
(* Boomerang lens combinators                                                 *)
(* $Id$ *)
(******************************************************************************)

(* ---------------------------------------------------------------------------*)
(* IMPORTS AND ABBREVIATIONS *)

module Rx = Brx
module Err = Berror

let string_concat = (^)
let sprintf = Printf.sprintf
let msg = Util.format
let (@) = Safelist.append

(* ---------------------------------------------------------------------------*)
(* TYPES *)

(* ----- unique identifiers ----- *)
type uid = int
let current_uid = ref 0
let next_uid () = incr current_uid; !current_uid

(* ----- keys ----- *)
type key = string
module KMap = Map.Make(
  struct 
    type t = string 
    let compare = compare 
  end)

(* ----- tags ----- *) 
type tag = string
module TMap = Map.Make(
  struct 
    type t = string 
    let compare = compare 
  end)

(* ----- skeletons ----- *)
type skeleton = 
  | S_string of string
  | S_concat of skeleton * skeleton
  | S_star of skeleton list
  | S_box of tag
  | S_comp of skeleton * skeleton
  | S_quot of string * skeleton 

let rec format_skel sk = 
  let fmt _ sk = format_skel sk in 
  Util.format "@[";
  (match sk with     
    | S_string w -> msg "S_string(\"%s\")" w
    | S_concat(sk1,sk2) -> msg "S_concat(%a,%a)" fmt sk1 fmt sk2
    | S_star(sl) -> msg "S_star(%a)" (fun _ -> Misc.format_list "," format_skel) sl
    | S_box(t) -> msg "S_box(%s)" t
    | S_comp(sk1,sk2) -> msg "S_comp(%a,%a)" fmt sk1 fmt sk2
    | S_quot(w,sk1) -> msg "S_quot(\"%s\",%a)" w fmt sk1);
  Util.format "@]"
       
(* helpers for accessing skeletons *)
let string_of_skel = function
  | S_string s -> s
  | sk -> Err.run_error (Info.M "string_of_skel") 
      (fun () -> 
	 msg "@[expected@ string@ skeleton, got:";
	 format_skel sk;
	 msg "@]") 

let fst_of_skel = function
  | S_concat (s,_) -> s
  | sk -> Err.run_error (Info.M "fst_of_skel") 
      (fun () -> 
	 msg "@[expected@ pair@ skeleton, got:";
	 format_skel sk;
	 msg "@]") 

let snd_of_skel = function
  | S_concat (_, s) -> s
  | sk -> Err.run_error (Info.M "snd_of_skel") 
      (fun () -> 
	 msg "@[expected@ pair@ skeleton, got:";
	 format_skel sk;
	 msg "@]")

let lst_of_skel = function
  | S_star sl -> sl
  | sk -> Err.run_error (Info.M "lst_of_skel") 
      (fun () -> 
	 msg "@[expected@ list@ skeleton, got:";
	 format_skel sk;
	 msg "@]") 

let comp_of_skel = function
  | S_comp(s1,s2) -> (s1,s2)
  | sk -> Err.run_error (Info.M "comp_of_skel")
      (fun () -> 
	 msg "@[expected@ comp@ skeleton, got:";
	 format_skel sk;
	 msg "@]") 

let quot_of_skel = function
  | S_quot(c,s) -> (c,s)
  | sk -> Err.run_error (Info.M "quot_of_skel") 
      (fun () -> 
	 msg "@[expected@ quot@ skeleton, got:";
	 format_skel sk;
	 msg "@]") 

(* ----- dictionaries ----- *)
type dict = ((skeleton * dict) list KMap.t) TMap.t

(* ----- dictionary types ----- *)
type dict_type = uid TMap.t

(* ----- equivalence relation types ----- *)
type equiv = Identity | Unknown

(* helper for merging equivs *)
let equiv_merge r1 r2 = match r1,r2 with 
  | Identity,Identity -> Identity 
  | _                 -> Unknown

(* ---------------------------------------------------------------------------*)
(* HELPERS *)

(* generic helper for iterating a regexp / lens / canonizer *)
let rec generic_iter epsilon union concat star min max x = 
  let rec mk_cats n x = match n with 
    | 0 -> epsilon
    | 1 -> x
    | 2 -> concat x x 
    | _ -> 
        let half_x = mk_cats (n/2) x in 
        let twice_half_x = concat half_x half_x in 
        if n mod 2 = 0 then twice_half_x
        else concat x twice_half_x in 
  let rec mk_alts acc xi j = match j with 
    | 0 -> acc
    | _ -> 
        let xi1 = concat x xi in 
        mk_alts (union acc xi1) xi1 (pred j) in 
   match min,max with
     | (0,-1) -> star x
     | (n,-1) -> concat (mk_cats n x) (star x)
     | (0,0)  -> epsilon
     | (0,1)  -> union epsilon x
     | (m,n)  -> 
         let m_x = mk_cats m x in 
         if m=n then m_x 
         else if m < n then mk_alts m_x m_x (n-m)
         else (* m > n *) 
           Err.run_error (Info.M "generic_iter") 
             (fun () -> msg "@[%d greater than %d@]" m n)

(* helper for concatenating an array *)
let concat_array a = 
  let buf = Buffer.create 17 in
    Array.iter (Buffer.add_string buf) a;
    Buffer.contents buf 

(* helper for splitting *)
let split_one choose t1 t2 s = 
  let ps = Rx.split_positions t1 t2 s in 
  let n = String.length s in 
  let j = choose ps in 
  (String.sub s 0 j, String.sub s j (n-j))

(* helper for splitting a string in an unambiguous concatenation *)
let seq_split t1 t2 w = 
  split_one 
    (fun ps -> 
       if Int.Set.cardinal ps = 1 then Int.Set.choose ps
       else 
(*          let n = String.length w in  *)
(*          let i1 = Int.Set.choose ps in  *)
(*          let i2 = Int.Set.choose (Int.Set.remove i1 ps) in  *)
         Err.run_error (Info.M "seq_split")
           (fun () -> 
              msg "@[concatenation of @[%a@]@ and @[%a@]@ "
                (fun _ -> Rx.format_t) t1 
                (fun _ -> Rx.format_t) t2;
              msg "ambiguous@ on@ [%s]"
                w))
  t1 t2 w


(* helpers for regular operators *)
let do_concat t1 t2 f1 f2 x = 
  let x1,x2 = seq_split t1 t2 x in 
  (f1 x1) ^ (f2 x2)
      
let do_star t f x = 
  let buf = Buffer.create 17 in 
  Safelist.iter
    (fun xi -> Buffer.add_string buf (f xi))
    (Rx.star_split t x);
  Buffer.contents buf

let do_union t f1 f2 x = 
  if Rx.match_string t x then f1 x else f2 x

(* ---------------------------------------------------------------------------*)
(* PERMUTATIONS *)
module Permutations = struct
  let string_of_sigma sigma =   
    sprintf "[%s]" (Misc.concat_list "," (Safelist.map string_of_int sigma))

  let rec identity k = 
    let rec loop acc i = 
      if i < 0 then acc
      else loop (i::acc) (pred i) in 
    loop [] (pred k) 
        
  let valid_permutation sigma ls =
    let k = Safelist.length sigma in
      Safelist.length ls = k
      && Safelist.sort compare sigma = identity k
        
  let permutations k = 
    let rec insertions n ls =
      let (is,_) = 
        Safelist.fold_left 
          (fun (ls_n_acc,ls_acc) i ->
	     let ls_acc' = i::ls_acc in
	     let ls_n_acc' = Safelist.map (fun ls_n -> i::ls_n) ls_n_acc in
	       ((n::ls_acc')::ls_n_acc',ls_acc'))
          ([[n]],[]) ls in
        is in
    let rec mk_perms k =
      if k = 0 then [[]]
      else Safelist.concat (Safelist.map (insertions (pred k)) (mk_perms (pred k))) in
    let id = identity k in 
    id::(Safelist.remove id (mk_perms k))

  let permutation sigma k = 
    let err () = 
      Err.run_error (Info.M "permutation")
        (fun () -> msg "@[%s@ is@ not@ a@ valid@ permutation@ on@ {0,..,%d}@]@\n" 
           (string_of_sigma sigma) (pred k)) in 
    let sigma_arr = Array.create k (-1) in 
    let sigma_inv_arr = Array.create k (-1) in       
    begin 
      let k' = 
        Safelist.fold_left 
          (fun i j -> 
             sigma_arr.(i) <- j;
             if sigma_inv_arr.(j) <> (-1) then err ();
             sigma_inv_arr.(j) <- i;
             succ i)
          0 sigma in
      if k' <> k then err () 
    end;
    (sigma_arr,sigma_inv_arr) 

  let invert_permutation sigma = 
    let sigma_arr = Array.of_list sigma in
    let sigma_inv_arr = Array.create (Array.length sigma_arr) (-1) in
    begin
      Array.iteri 
        (fun i j -> 
           if sigma_inv_arr.(j) = -1 then sigma_inv_arr.(j) <- i
           else
             Err.run_error (Info.M "invert_permutation") 
               (fun () -> msg "@[%s@ is@ not@ a@ valid@ permutation@ on@ {0,..,%d}@]@\n"
                  (string_of_sigma sigma) (pred (Safelist.length sigma))))
        sigma_arr
    end;
    Array.to_list sigma_inv_arr

  let permute_list sigma ls =
    let ls_arr = Array.of_list ls in
    let k = Array.length ls_arr in
    let _,sigma_inv_arr = permutation sigma k in
    Array.fold_right 
      (fun j ls' -> ls_arr.(j)::ls')
      sigma_inv_arr []
end
  
(* ---------------------------------------------------------------------------*)
(* CANONIZERS *)
module Canonizer = struct
  type d = 
    | Copy of Rx.t 
    | Concat of t * t 
    | Union of t * t 
    | Star of t 
    | Normalize of Rx.t * Rx.t * (string -> string) 
    | Sort of int * (int * Rx.t) list
    | Columnize of int * Rx.t * char * string 
    | FromLens of Rx.t * Rx.t * equiv * (string -> string) * (string -> string) 

  and t = 
      { (* ----- meta data ----- *)
        info : Info.t;
        uid : int;
        desc : d;
        (* ----- types ----- *)
        mutable uncanonized_type : Rx.t option;
        mutable canonized_type : Rx.t option;
        mutable cnrel : equiv option;
      }

  let mk i d = 
    { info = i;
      uid = next_uid ();
      desc = d;
      uncanonized_type = None;
      canonized_type = None;
      cnrel = None;
    }

  (* ----- accessors ----- *)
  let rec uncanonized_type cn = match cn.uncanonized_type with 
    | Some ut -> ut
    | None -> 
        let ut = match cn.desc with
          | Copy(r1)             -> r1
          | Concat(cn1,cn2)      -> Rx.mk_seq (uncanonized_type cn1) (uncanonized_type cn2)
          | Union(cn1,cn2)       -> Rx.mk_alt (uncanonized_type cn1) (uncanonized_type cn2)
          | Star(cn1)            -> Rx.mk_star (uncanonized_type cn1)
          | Columnize(k,r,ch,nl) -> Rx.mk_expand r (Char.code ch) (Rx.mk_string nl) 
          | Normalize(ct,ct0,f)  -> ct
          | Sort(_,irl)          -> Rx.mk_star (Safelist.fold_left (fun acc (_,ri) -> Rx.mk_alt acc ri) Rx.empty irl)
          | FromLens(ct,_,_,_,_) -> ct in
        cn.uncanonized_type <- Some ut;
        ut
  and canonized_type cn = match cn.canonized_type with 
    | Some ct -> ct
    | None -> 
        let ct = match cn.desc with
          | Copy(r1)             -> r1
          | Concat(cn1,cn2)      -> Rx.mk_seq (canonized_type cn1) (canonized_type cn2)
          | Union(cn1,cn2)       -> Rx.mk_alt (canonized_type cn1) (canonized_type cn2)
          | Star(cn1)            -> Rx.mk_star (canonized_type cn1)
          | Columnize(k,r,ch,nl) -> r
          | Normalize(ct,ct0,f)  -> ct0
          | Sort(_,irl)          -> Safelist.fold_left (fun acc (_,ri) -> Rx.mk_seq ri acc) Rx.epsilon irl (* NB order! *)
          | FromLens(_,at,_,_,_) -> at in
        cn.canonized_type <- Some ct;
        ct

  and cnrel cn = match cn.cnrel with 
    | Some cr -> cr
    | None -> 
        let cr = match cn.desc with
          | Copy(r1)             -> Identity
          | Concat(cn1,cn2)      -> equiv_merge (cnrel cn1) (cnrel cn2)
          | Union(cn1,cn2)       -> 
              if Rx.is_empty (Rx.mk_diff (canonized_type cn1) (canonized_type cn2)) then (cnrel cn1) 
              else equiv_merge (cnrel cn1) (cnrel cn2)
          | Star(cn1)            -> cnrel cn1
          | Columnize(k,r,ch,nl) -> Identity
          | Normalize(ct,ct0,f)  -> Identity
          | Sort(_)              -> Identity
          | FromLens(_,_,eq,_,_) -> eq in
        cn.cnrel <- Some cr;
        cr

  and canonize cn = match cn.desc with
    | Copy(r1)              -> (fun c -> c)
    | Concat(cn1,cn2)       -> 
        (fun c -> do_concat (uncanonized_type cn1) (uncanonized_type cn2) 
           (canonize cn1) (canonize cn2) c)
    | Union(cn1,cn2)        ->  
        (fun c -> do_union (uncanonized_type cn1) 
           (canonize cn1) (canonize cn2) c)
    | Star(cn1)             -> 
        (fun c -> do_star (uncanonized_type cn1) (canonize cn1) c)
    | Normalize(ct,ct0,f)   -> (fun c -> f c)
    | FromLens(_,_,_,get,_) -> (fun c -> get c)
    | Sort(k,irl)           -> 
        (* INEFFICIENT! *)
        (fun c -> 
           let cl = 
             Rx.star_split 
               (Safelist.fold_left (fun acc (_,ri) -> Rx.mk_alt acc ri) Rx.empty irl) 
               c in
           let c_arr = Array.create k "" in 
           if Safelist.length cl > k then 
             Err.run_error (Info.M "sort.canonize") 
               (fun () -> msg "@[%s@ split@ into@ more@ than@ %d@ pieces@]" c k);
           let rs' = Safelist.fold_left 
             (fun rs ci -> 
                match Safelist.partition (fun (_,ri) -> Rx.match_string ri ci) rs with
                  | [],rs' -> rs'
                  | [i,_],rs' -> c_arr.(i) <- ci; rs'
                  | _ -> Err.run_error (Info.M "sort.canonize")
                      (fun () -> msg "@[%s@ matched@ more@ than@ one@ regexp@]" ci))
             irl cl in 
           let () = Safelist.iter 
             (fun (_,ri) -> 
                if not (Rx.is_final ri) then 
                  Err.run_error (Info.M "sort.canonize")
                    (fun () -> msg "@[no@ string@ matched@ %s@]" (Rx.string_of_t ri))) 
             rs' in 
           concat_array c_arr)
    | Columnize(k,r,ch,nl)  -> 
        (fun c ->
           let c_len = String.length c in
           let nl_len = String.length nl in 
           let buf = Buffer.create c_len in
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
             Buffer.contents buf)

  and choose cn = match cn.desc with
    | Copy(r1)              -> (fun b -> b)
    | Concat(cn1,cn2)       -> 
        (fun b -> 
           let b1,b2 = 
             (split_one Int.Set.min_elt)
               (canonized_type cn1) (canonized_type cn2) b in 
             (choose cn1 b1) ^ (choose cn2 b2))
    | Union(cn1,cn2)        ->  
        (fun b -> do_union (canonized_type cn1) (choose cn1) (choose cn2) b)
    | Star(cn1)             -> 
        (fun b -> 
           let buf = Buffer.create 17 in 
           let rec loop b = 
             if String.length b = 0 then Buffer.contents buf
             else
               let b1,brest = 
                 (split_one Int.Set.min_elt) 
                   (canonized_type cn1) (canonized_type cn) 
                   b in 
               Buffer.add_string buf (choose cn1 b1);
               loop brest in 
           loop b)
    | Normalize(ct,ct0,f)   -> (fun b -> b)
    | FromLens(_,_,_,_,crt) -> (fun b -> crt b)
    | Sort(_)               -> (fun b -> b)
    | Columnize(k,r,ch,nl)  -> 
        (fun b ->
           let b_len = String.length b in
           let nl_len = String.length nl in 
           let buf = Buffer.create b_len in
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
               if i = b_len then (do_space (); do_line ())
               else
                 let i' =
                   if ch = b.[i] then (do_space (); i + 1)
                   else (Buffer.add_char aux_buf b.[i]; succ i) in
                   loop i' in
           loop 0;
           Buffer.contents buf)

  let info cn = cn.info

  let format cn = msg ""

  let string cn = 
    Util.format_to_string 
      (fun () -> 
	 Util.format "@["; 
	 format cn; 
	 Util.format "@]")

  let cnrel_identity cn = cnrel cn = Identity

  (* ----- constructors ----- *)
  let copy i r1 = mk i (Copy(r1))
  let concat i cn1 cn2 = mk i (Concat(cn1,cn2))
  let union i cn1 cn2 = mk i (Union(cn1,cn2))
  let star i cn1 = mk i (Star(cn1))
  let normalize i ct ct0 f = mk i (Normalize(ct,ct0,f))
  let sort i rl = 
    let k,irl = Safelist.fold_left (fun (i,acc) ri -> (succ i,(i,ri)::acc)) (0,[]) rl in
    mk i (Sort(k,irl))
  let columnize i k r sp nl = mk i (Columnize(k,r,sp,nl))
  let from_lens i ct at eq get crt = mk i (FromLens(ct,at,eq,get,crt))
  let iter i cn1 min maxo = 
    generic_iter (copy i Rx.epsilon) (union i) (concat i) (star i) 
      min maxo cn1
end

(* ---------------------------------------------------------------------------*)
(* DICTIONARY LENSES *)
module DLens = struct    
  
  type d = 
    (* ----- string lenses ----- *)
    | Copy of Rx.t
    | Clobber of Rx.t * string * (string -> string)
    | Concat of t * t
    | Union of t * t
    | Star of t
    | Key of Rx.t
    | DMatch of string * t

    (* ----- generic lenses ------ *)
    | Compose of t * t
    | Invert of t
    | Default of  t * string

    (* ----- quotient lenses ----- *)
    | LeftQuot of  Canonizer.t * t
    | RightQuot of t * Canonizer.t 
    | Dup1 of t * (string -> string) * Rx.t 
    | Dup2 of (string -> string) * Rx.t * t

    (* ----- extensions ----- *)
    | SMatch of string * float * t 
    | Partition of Rx.t * Rx.t 
    | Merge of Rx.t 
    | Fiat of t 
    | Forgetkey of t
    | Permute of (int * int array * int array * Rx.t array * Rx.t array) * t array 
    | Probe of string * t

  and t = 
      { (* ----- meta data ----- *)
        info : Info.t;
        uid : int;
        desc : d;
        (* ----- types ----- *)
        mutable bij: bool option;
        mutable ctype : Rx.t option;
        mutable atype : Rx.t option;
        mutable crel : equiv option;
        mutable arel : equiv option;
        mutable xtype : (Erx.t option) option;
        mutable dtype : dict_type;
      }

  let mk i d dt = 
    { info = i;
      uid = next_uid ();
      desc = d;
      bij = None;
      ctype = None;
      atype = None;
      crel = None;
      arel = None;
      xtype = None;
      dtype = dt;
    }        

  (* lookup helpers *)
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
        | None -> begin 
            let reso = KMap.fold
              (fun ki li acco -> 
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

  (* helpers for dictionaries *)
  let dt_merge dt1 dt2 = 
    TMap.fold 
      (fun t u acc ->
         if (not (TMap.mem t dt2)) || (TMap.find t dt2 = u) then 
           TMap.add t u acc
         else 
           Err.run_error (Info.M "dt_merge") 
             (fun () -> msg "@[tag \"%s\" used with different lenses" t))
      dt1 dt2

  let dt_split d t = 
    try 
      let km = TMap.find t d in 
      (TMap.add t km TMap.empty, TMap.remove t d)      
    with Not_found -> 
    (TMap.empty,d)

  let (++) d1 d2 =
    let combine fold find add merge m1 m2 = 
      fold (fun k v -> add k (try merge v (find k m2) with Not_found -> v)) 
        m1 m2 in 
    combine TMap.fold TMap.find TMap.add 
      (fun km1 km2 -> 
	 (combine KMap.fold KMap.find KMap.add 
	    (fun kl1 kl2 -> kl1 @ kl2)
	    km1 km2))
      d1 d2

  (* ----- accessors ----- *)
  let rec bij dl = match dl.bij with 
    | Some b -> b
    | None   -> 
        let b = match dl.desc with          
          | Copy(r1)           -> true
          | Clobber(r1,w1,f1)   -> Rx.is_singleton r1
          | Concat(dl1,dl2)    -> bij dl1 && bij dl2
          | Union(dl1,dl2)     -> bij dl1 && bij dl2 && Rx.disjoint (atype dl1) (atype dl2)
          | Star(dl1)          -> bij dl1
          | Key(r1)            -> true
          | DMatch(t1,dl1)     -> bij dl1
          | Compose(dl1,dl2)   -> bij dl1 && bij dl2
          | Invert(dl1)        -> true
          | Default(dl1,w1)    -> bij dl1
          | LeftQuot(cn1,dl1)  -> bij dl1
          | RightQuot(dl1,cn1) -> bij dl1
          | Dup1(dl1,f1,r1)    -> bij dl1
          | Dup2(f1,r1,dl1)    -> bij dl1
          | SMatch(t1,f1,dl1)  -> bij dl1
          | Partition(r1,r2)   -> Rx.is_empty r1 || Rx.is_empty r2 
          | Merge(r1)          -> Rx.is_singleton r1 
          | Fiat(dl1)          -> bij dl1 
          | Forgetkey(dl1)     -> bij dl1 
          | Permute(_,dls)     -> Array.fold_left (fun b dli -> b && bij dli) true dls 
          | Probe(_,dl1)       -> bij dl1 in
        dl.bij <- Some b;
        b

  and ctype dl = match dl.ctype with 
    | Some ct -> ct
    | None -> 
        let ct = match dl.desc with
          | Copy(r1)           -> r1
          | Clobber(r1,w1,f1)    -> r1
          | Concat(dl1,dl2)    -> Rx.mk_seq (ctype dl1) (ctype dl2)
          | Union(dl1,dl2)     -> Rx.mk_alt (ctype dl1) (ctype dl2)
          | Star(dl1)          -> Rx.mk_star (ctype dl1)
          | Key(r1)            -> r1
          | DMatch(t1,dl1)     -> ctype dl1
          | Compose(dl1,dl2)   -> ctype dl1
          | Invert(dl1)        -> atype dl1
          | Default(dl1,w1)    -> ctype dl1
          | LeftQuot(cn1,dl1)  -> Canonizer.uncanonized_type cn1
          | RightQuot(dl1,cn1) -> ctype dl1
          | Dup1(dl1,f1,r1)    -> ctype dl1
          | Dup2(f1,r1,dl1)    -> ctype dl1
          | SMatch(t1,f1,dl1)  -> ctype dl1
          | Partition(r1,r2)   -> Rx.mk_star (Rx.mk_alt r1 r2) 
          | Merge(r1)          -> Rx.mk_seq r1 r1 
          | Fiat(dl1)          -> ctype dl1 
          | Forgetkey(dl1)     -> ctype dl1 
          | Permute(_,dls)     -> 
              Array.fold_left (fun acc dli -> Rx.mk_seq acc (ctype dli)) 
                Rx.epsilon dls 
          | Probe(_,dl1)       -> ctype dl1 in
        dl.ctype <- Some ct;
        ct
          
  and atype dl = match dl.atype with
    | Some at -> at
    | None -> 
        let at = match dl.desc with 
          | Copy(r1)           -> r1
          | Clobber(r1,w1,f1)    -> Rx.mk_string w1 
          | Concat(dl1,dl2)    -> Rx.mk_seq (atype dl1) (atype dl2)
          | Union(dl1,dl2)     -> Rx.mk_alt (atype dl1) (atype dl2)
          | Star(dl1)          -> Rx.mk_star (atype dl1)
          | Key(r1)            -> r1
          | DMatch(t1,dl1)     -> atype dl1
          | Compose(dl1,dl2)   -> atype dl2
          | Invert(dl1)        -> ctype dl1
          | Default(dl1,w1)    -> atype dl1
          | LeftQuot(cn1,dl1)  -> atype dl1
          | RightQuot(dl1,cn1) -> Canonizer.uncanonized_type cn1
          | Dup1(dl1,f1,r1)    -> Rx.mk_seq (atype dl1) r1
          | Dup2(f1,r1,dl1)    -> Rx.mk_seq r1 (atype dl1)
          | SMatch(t1,f1,dl1)  -> atype dl1
          | Partition(r1,r2)   -> Rx.mk_seq (Rx.mk_star r1) (Rx.mk_star r2)
          | Merge(r1)          -> r1
          | Fiat(dl1)           -> atype dl1
          | Forgetkey(dl1)     -> atype dl1 
          | Permute(p1,dls)    -> 
              let _,_,s2,_,_ = p1 in 
              Array.fold_left 
                (fun acc i -> Rx.mk_seq acc (atype dls.(i)))
                Rx.epsilon s2 
          | Probe(_,dl1)       -> atype dl1 in
        dl.atype <- Some at;
        at

  and xtype dl = match dl.xtype with 
    | Some xto -> xto
    | None -> 
        let xto = match dl.desc with
          | Copy(r1)           -> Some (Erx.mk_leaf (atype dl))
          | Clobber(r1,w1,f1)    -> Some (Erx.mk_leaf (atype dl))
          | Concat(dl1,dl2)    -> Misc.map2_option Erx.mk_seq (xtype dl1) (xtype dl2)
          | Union(dl1,dl2)     -> Misc.map2_option Erx.mk_alt (xtype dl1) (xtype dl2)
          | Star(dl1)          -> 
              begin match xtype dl1 with
                | None -> None
                | Some xt1 -> 
                    if Erx.boxes xt1 <> 0 && not (Erx.iterable xt1) then None 
                    else Some (Erx.mk_star xt1) 
              end
          | Key(r1)            -> Some (Erx.mk_key (atype dl))
          | DMatch(t1,dl1)     -> Misc.map_option (Erx.mk_box t1) (xtype dl1)
          | Compose(dl1,dl2)   -> xtype dl2
          | Invert(dl1)        -> Some (Erx.mk_leaf (ctype dl1))
          | Default(dl1,w1)    -> xtype dl1
          | LeftQuot(cn1,dl1)  -> xtype dl1
          | RightQuot(dl1,cn1) -> None (* FIX? *)
          | Dup1(dl1,f1,r1)    -> 
              Misc.map_option 
                (fun xt1 -> Erx.mk_seq xt1 (Erx.mk_leaf r1)) 
                (xtype dl1)
          | Dup2(f1,r1,dl1)    -> 
              Misc.map_option 
                (fun xt1 -> Erx.mk_seq (Erx.mk_leaf r1) xt1) 
                (xtype dl1)
          | SMatch(t1,f1,dl1)  -> Misc.map_option (Erx.mk_box t1) (xtype dl1)
          | Partition(r1,r2)      -> Some (Erx.mk_leaf (atype dl)) 
          | Merge(r1)          -> Some (Erx.mk_leaf (atype dl))
          | Fiat(dl1)          -> xtype dl1 
          | Forgetkey(dl1)     -> xtype dl1 
          | Permute(p1,dls)    -> 
              let _,s1,_,_,_ = p1 in 
              Array.fold_left 
                (fun acco i -> Misc.map2_option Erx.mk_seq acco (xtype dls.(i)))
                (Some (Erx.mk_leaf (Rx.epsilon))) s1 
          | Probe(_,dl1)       -> xtype dl1 in 
        dl.xtype <- Some xto;
        xto

  and calc_dtype d = match d with
    | Copy(r1)           -> TMap.empty
    | Clobber(r1,w1,f1)  -> TMap.empty
    | Concat(dl1,dl2)    -> dt_merge dl1.dtype dl2.dtype
    | Union(dl1,dl2)     -> dt_merge dl1.dtype dl2.dtype
    | Star(dl1)          -> dl1.dtype
    | Key(r1)            -> TMap.empty
    | DMatch(t1,dl1)     -> TMap.add t1 dl1.uid TMap.empty
    | Compose(dl1,dl2)   -> dt_merge dl1.dtype dl2.dtype
    | Invert(dl1)        -> dl1.dtype
    | Default(dl1,w1)    -> dl1.dtype
    | LeftQuot(cn1,dl1)  -> dl1.dtype
    | RightQuot(dl1,cn1) -> dl1.dtype
    | Dup1(dl1,f1,r1)    -> dl1.dtype
    | Dup2(f1,r1,dl1)    -> dl1.dtype
    | SMatch(t1,f1,dl1)  -> TMap.add t1 dl1.uid TMap.empty
    | Partition(r1,r2)      -> TMap.empty 
    | Merge(r1)          -> TMap.empty
    | Fiat(dl1)          -> dl1.dtype
    | Forgetkey(dl1)     -> dl1.dtype 
    | Permute(_,dls)     -> 
        Array.fold_left 
          (fun acc dli -> dt_merge acc dli.dtype)
          TMap.empty dls 
    | Probe(_,dl1)       -> dl1.dtype 
                
  and stype dl sk = match sk,dl.desc with
    | S_string(s),Copy(r1)              -> Rx.match_string (ctype dl) s
    | S_string(s),Clobber(r1,w1,f1)       -> Rx.match_string (ctype dl) s
    | S_concat(sk1,sk2),Concat(dl1,dl2) -> stype dl1 sk1 && stype dl2 sk2
    | _,Union(dl1,dl2)                  -> stype dl1 sk || stype dl2 sk
    | S_star(sl),Star(dl1)              -> Safelist.for_all (stype dl1) sl
    | S_string s,Key(r1)                -> Rx.match_string (ctype dl) s
    | S_box(b),DMatch(t1,dl1)           -> b = t1
    | S_comp(sk1,sk2),Compose(dl1,dl2)  -> stype dl1 sk1 && stype dl2 sk2
    | _,Invert(dl1)                     -> stype dl1 sk
    | _,Default(dl1,w1)                 -> stype dl1 sk
    | S_quot(_,sk1),LeftQuot(cn1,dl1)   -> stype dl1 sk1
    | _,RightQuot(dl1,cn1)              -> stype dl1 sk
    | _,Dup1(dl1,f1,r1)                 -> stype dl1 sk
    | _,Dup2(f1,r1,dl1)                 -> stype dl1 sk
    | S_box(b),SMatch(t1,f1,dl1)        -> b = t1
    | S_string(s),Partition(r1,r2)      -> Rx.match_string (ctype dl) s 
    | S_string(s),Merge(r1)             -> Rx.match_string (ctype dl) s
    | _,Fiat(dl1)                       -> stype dl1 sk
    | _,Forgetkey(dl1)                  -> stype dl1 sk
    | S_star(sl),Permute(_,dls)         -> 
        (Safelist.length sl = Array.length dls)
        && (fst (Safelist.fold_left 
                   (fun (ok,i) si -> (ok && stype dls.(i) si, succ i))
                   (true,0) sl))
    | _,Probe(_,dl1)                    -> stype dl1 sk 
    | _                                 -> false  

  and crel dl = match dl.crel with
    | Some cr -> cr
    | None -> 
        let cr = match dl.desc with
          | Copy(r1)           -> Identity
          | Clobber(r1,w1,f1)    -> Identity
          | Concat(dl1,dl2)    -> equiv_merge (crel dl1) (crel dl2)
          | Union(dl1,dl2)     -> equiv_merge (crel dl1) (crel dl2)
          | Star(dl1)          -> crel dl1
          | Key(r1)            -> Identity
          | DMatch(t1,dl1)     -> crel dl1
          | Compose(dl1,dl2)   -> crel dl1
          | Invert(dl1)        -> arel dl1
          | Default(dl1,w1)    -> crel dl1
          | LeftQuot(cn1,dl1)  -> Unknown
          | RightQuot(dl1,cn1) -> crel dl1
          | Dup1(dl1,f1,r1)    -> crel dl1
          | Dup2(f1,r1,dl1)    -> crel dl1
          | SMatch(t1,f1,dl1)  -> crel dl1
          | Partition(r1,r2)   -> Identity 
          | Merge(r1)          -> Identity
          | Fiat(dl1)          -> crel dl1 
          | Forgetkey(dl1)     -> crel dl1 
          | Permute(p1,dls)    -> 
              Array.fold_left 
                (fun acc dli -> equiv_merge acc (crel dli)) 
                Identity dls 
          | Probe(_,dl1)       -> crel dl1 in
        dl.crel <- Some cr;
        cr

  and arel dl = match dl.arel with
    | Some ar -> ar
    | None -> 
        let ar = match dl.desc with
          | Copy(r1)           -> Identity
          | Clobber(r1,w1,f1)    -> Identity
          | Concat(dl1,dl2)    -> equiv_merge (arel dl1) (arel dl2)
          | Union(dl1,dl2)     -> equiv_merge (arel dl1) (arel dl2)
          | Star(dl1)          -> arel dl1
          | Key(r1)            -> Identity
          | DMatch(t1,dl1)     -> arel dl1
          | Compose(dl1,dl2)   -> arel dl2
          | Invert(dl1)        -> crel dl1
          | Default(dl1,w1)    -> arel dl1
          | LeftQuot(cn1,dl1)  -> arel dl1
          | RightQuot(dl1,cn1) -> Unknown
          | Dup1(dl1,f1,r1)    -> Unknown
          | Dup2(f1,r1,dl1)    -> Unknown
          | SMatch(t1,f1,dl1)  -> arel dl1
          | Partition(r1,r2)   -> Identity 
          | Merge(r1)          -> Identity 
          | Fiat(dl1)          -> arel dl1
          | Forgetkey(dl1)     -> arel dl1
          | Permute(p1,dls)    -> 
              Array.fold_left (fun acc dli -> equiv_merge acc (arel dli)) 
                Identity dls 
          | Probe(_,dl1)       -> arel dl1 in
        dl.arel <- Some ar;
        ar

  and get dl = match dl.desc with
    | Copy(r1)           -> (fun c -> c)
    | Clobber(r1,w1,t1)    -> (fun c -> w1)
    | Concat(dl1,dl2)    -> 
        (fun c -> 
           do_concat (ctype dl1) (ctype dl2) (get dl1) (get dl2) c)
    | Union(dl1,dl2)     -> 
        (fun c -> do_union (ctype dl1) (get dl1) (get dl2) c)
    | Star(dl1)          -> 
        (fun c -> do_star (ctype dl1) (get dl1) c)
    | Key(r1)            -> (fun c -> c)
    | DMatch(t1,dl1)     -> (fun c -> get dl1 c)
    | Compose(dl1,dl2)   -> (fun c -> get dl2 (get dl1 c))
    | Invert(dl1)        -> (fun c -> rcreate dl1 c)
    | Default(dl1,w1)    -> (fun c -> get dl1 c)
    | LeftQuot(cn1,dl1)  -> (fun c -> get dl1 (Canonizer.canonize cn1 c))
    | RightQuot(dl1,cn1) -> (fun c -> Canonizer.choose cn1 (get dl1 c))
    | Dup1(dl1,f1,r1)    -> (fun c -> get dl1 c ^ f1 c)
    | Dup2(f1,r1,dl1)    -> (fun c -> f1 c ^ get dl1 c)
    | SMatch(t1,f1,dl1)  -> (fun c -> get dl1 c)
    | Partition(r1,r2)      -> 
        (fun c -> 
           let rec loop (acc1,acc2) = function
             | [] -> acc1 ^ acc2 
             | h::t -> 
                 begin match Rx.match_string r1 h, Rx.match_string r2 h with 
                   | true, false -> loop (acc1 ^ h, acc2) t 
                   | false,true  -> loop (acc1, acc2 ^ h) t
                   | _           -> assert false
                 end in 
           loop ("","") (Rx.star_split (ctype dl) c))
    | Merge(r1) -> 
        (fun c -> 
           let c1,_ = seq_split r1 r1 c in 
           c1)
    | Fiat(dl1) -> (fun c -> get dl1 c)
    | Forgetkey(dl1)     -> (fun c -> get dl1 c)
    | Permute(p1,dls)    -> 
        (fun c -> 
           let k,sigma,sigma_inv,cts,ats = p1 in 
           let c_arr = arr_split_c k dls cts c in 
           let a_arr = Array.create k "" in
           let rec loop i = 
             if i >= k then concat_array a_arr 
             else 
               begin 
                 let j = sigma.(i) in 
                 let ai = get dls.(i) c_arr.(i) in 
                   a_arr.(j) <- ai;
                   loop (succ i)
               end in
           loop 0)
    | Probe(t,dl1)       -> 
        (fun c -> 
           msg "@[[[PROBE-GET{%s}@\n%s@\n]]@\n@]" t c;
           get dl1 c)
                 
  and put dl = match dl.desc with
    | Copy(r1)           -> (fun a _ d -> (a,d))
    | Clobber(r1,w1,f1)  -> (fun _ s d -> (string_of_skel s,d))
    | Concat(dl1,dl2)    -> 
        (fun a s d -> 
           let a1,a2 = seq_split (atype dl1) (atype dl2) a in
           let c1,d1 = put dl1 a1 (fst_of_skel s) d in
           let c2,d2 = put dl2 a2 (snd_of_skel s) d1 in 
           (c1 ^ c2,d2))
    | Union(dl1,dl2)     -> 
        (fun a s d -> 
           match Rx.match_string (atype dl1) a,
                 Rx.match_string (atype dl2) a,
                 stype dl1 s with
             | true,_,true      -> put dl1 a s d
             | _,true,false     -> put dl2 a s d
             | true,false,false -> create dl1 a d 
             | false,true,true  -> create dl2 a d 
             | _                -> assert false)
    | Star(dl1)          -> 
        (fun a s d -> 
           let rec loop al sl acc d = match al,sl with
             | [],_ -> (acc,d)
             | ai::at,[] -> 
                 let ci,di = create dl1 ai d in
                 loop at [] (acc ^ ci) di 
             | ai::at,si::st -> 
                 let ci,di = put dl1 ai si d in 
                 loop at st (acc ^ ci) di in
           loop (Rx.star_split (atype dl1) a)
           (lst_of_skel s) "" d)
    | Key(r1)            -> (fun a _ d -> (a,d))
    | DMatch(t1,dl1)     -> 
        (fun a _ d -> 
           do_match std_lookup (key dl1) (put dl1) (create dl1) t1 a d) 
    | Compose(dl1,dl2)   -> 
        (fun a s d -> 
           let s1,s2 = comp_of_skel s in 
           let b,d1 = put dl2 a s2 d in 
           put dl1 b s1 d1)
    | Invert(dl1)        -> (fun a _ d -> (get dl1 a,d))
    | Default(dl1,w1)    -> (fun a s d -> put dl1 a s d)
    | LeftQuot(cn1,dl1)  -> 
        (fun a s d -> 
           let _,s' = quot_of_skel s in 
           let c',d' = put dl1 a s' d in 
           (Canonizer.choose cn1 c',d'))
    | RightQuot(dl1,cn1) -> 
        (fun a s d -> put dl1 (Canonizer.canonize cn1 a) s d)
    | Dup1(dl1,f1,r1)    -> 
        (fun a s d -> 
           let a1,_ = seq_split (atype dl1) r1 a in
           put dl1 a1 s d)
    | Dup2(f1,r1,dl1)    -> 
        (fun a s d -> 
           let _,a2 = seq_split r1 (atype dl1) a in 
           put dl1 a2 s d)
    | SMatch(t1,f1,dl1)  ->
        (fun a _ d -> do_match (sim_lookup f1) (key dl1) (put dl1) (create dl1) t1 a d)
    | Partition(r1,r2)      -> 
        (fun a s d -> 
           let c = string_of_skel s in
           let r1s = Rx.mk_star r1 in 
           let r2s = Rx.mk_star r2 in 
           let a1,a2 = seq_split r1s r2s a in
           let rec loop acc l l1 l2 = 
             match l,l1,l2 with
             | [],[],h2::t2 -> loop (acc ^ h2) [] [] t2 
             | [], h1::t1,_ -> loop (acc ^ h1) [] t1 l2
             | _,[],[] -> acc
             | h::t,h1::t1,_ when Rx.match_string r1 h -> loop (acc ^ h1) t t1 l2
             | h::t,_,h2::t2 when Rx.match_string r2 h -> loop (acc ^ h2) t l1 t2
             | h::t,_,_ -> loop acc t l1 l2 in 
           let c' = loop "" (Rx.star_split (ctype dl) c) (Rx.star_split r1s a1) (Rx.star_split r2s a2) in 
           (c',d))
    | Merge(r1)          -> 
        (fun a s d ->            
           let s1,s2 = seq_split r1 r1 (string_of_skel s) in 
           if s1 = s2 then (a ^ a,d)
           else (a ^ s2,d))
    | Fiat(dl1) -> 
        (fun a s d -> 
           match unparse dl1 a s d with
             | Some(c,d) -> (c,d)
             | None -> put dl1 a s d)
    | Forgetkey(dl1)     -> (fun a s d -> put dl1 a s d)
    | Permute(p1,dls)    -> 
        (fun a s d -> 
           let k,sigma,sigma_inv,cts,ats = p1 in 
           let a_arr = arr_split_a k dls sigma_inv ats a in 
           let s_arr = Array.of_list (lst_of_skel s) in
           let c_arr = Array.create k "" in 
           let rec loop j d = 
             if j >= k then d
             else
               begin 
                 let i = sigma_inv.(j) in 
                 let ci,di = put dls.(i) a_arr.(j) s_arr.(i) d in
                 c_arr.(i) <- ci;
                 loop (succ j) di 
               end in
           let d' = loop 0 d in 
           let c' = concat_array c_arr in
           (c',d'))
    | Probe(t,dl1)       -> 
        (fun a s d -> 
           msg "@[[[PROBE-PUT{%s}@\n%s@\n%a@\n]]@\n@]%!" t a (fun _ -> format_skel) s;           
           put dl1 a s d)

  and unparse dl = match dl.desc with 
    | Copy(r1)           -> 
        (fun a s d -> 
           let c = string_of_skel s in 
           if a=get dl c then Some (c,d) else None)
    | Clobber(r1,w1,f1)  -> 
        (fun a s d -> Some (string_of_skel s,d))
    | Concat(dl1,dl2)    -> 
        (fun a s d -> 
           let a1,a2 = seq_split (atype dl1) (atype dl2) a in
           (match unparse dl1 a1 (fst_of_skel s) d with
              | Some (c1,d1) -> 
                  Misc.map_option
                    (fun (c2,d2) -> (c1 ^ c2, d2))
                    (unparse dl2 a2 (snd_of_skel s) d1)
              | None -> None))
    | Union(dl1,dl2)     -> 
        (fun a s d -> 
           match Rx.match_string (atype dl1) a,
                 Rx.match_string (atype dl2) a,
                 stype dl1 s with
             | true,_,true      -> unparse dl1 a s d
             | _,true,false     -> unparse dl2 a s d
             | true,false,false -> None
             | false,true,true  -> None
             | _                -> assert false)
    | Star(dl1) -> 
        (fun a s d -> 
           let rec loop al sl acc = match acc,al,sl with
             | None,_,_ -> None
             | _,ai::at,[] -> None
             | _,[],si::st -> None
             | _,[],[] -> acc
             | Some(acci,di),ai::at,si::st -> 
                 (match unparse dl1 ai si d with
                    | Some (ci,di) -> loop at st (Some (acci ^ ci,di))
                    | _ -> None) in 
           loop (Rx.star_split (atype dl1) a)
             (lst_of_skel s) (Some ("",d)))
    | Key(r1) -> 
        (fun a s d -> 
           let c = string_of_skel s in 
           if a=get dl c then Some (c,d) else None)
    | DMatch(t1,dl1) -> 
        (fun a s d -> 
           let d1,d2 = dt_split d t1 in 
           match std_lookup t1 (key dl1 a) d1 with
             | Some ((s',d1'),d1'') -> 
                 (match unparse dl1 a s' (d1' ++ d2) with
                    | Some(c',d') -> 
                        let _,d2' = dt_split d' t1 in 
                        Some(c',d1'' ++ d2')
                    | None -> None)
             | None -> None)
    | Compose(dl1,dl2)   -> 
        (fun a s d -> 
           let s1,s2 = comp_of_skel s in 
           (match unparse dl2 a s2 d with
              | Some (b,d') -> unparse dl1 b s1 d'
              | None -> None))
    | Invert(dl1)        -> 
        (fun a s d -> 
           let c = string_of_skel s in 
           if a = get dl c then Some(c,d) else None)
    | Default(dl1,w1)    -> unparse dl1 
    | LeftQuot(cn1,dl1)  -> 
        (fun a s d -> 
           let c,s' = quot_of_skel s in 
           match unparse dl1 a s' d with
             | Some (_,d') -> Some(c,d')
             | None         -> None)
    | RightQuot(dl1,cn1) -> 
        (fun a s d -> 
           unparse dl1 (Canonizer.canonize cn1 a) s d)
    | Dup1(dl1,f1,r1)    -> 
        (fun a s d -> 
           let a1,_ = seq_split (atype dl1) r1 a in
           unparse dl1 a1 s d)
    | Dup2(f1,r1,dl1)    -> 
        (fun a s d -> 
           let _,a2 = seq_split r1 (atype dl1) a in
           unparse dl1 a2 s d)
    | SMatch(t1,f1,dl1)  ->
        (fun a s d -> 
           let d1,d2 = dt_split d t1 in 
           match sim_lookup f1 t1 (key dl1 a) d1 with
             | Some ((s',d1'),d1'') -> 
                 (match unparse dl1 a s' (d1' ++ d2) with
                    | Some(c',d') -> 
                        let _,d2' = dt_split d' t1 in 
                        Some(c',d1'' ++ d2')
                    | None -> None)
             | None -> None)
    | Partition(r1,r2) -> 
        (fun a s d -> 
           let c = string_of_skel s in
           if a=get dl c then Some (c,d) else None)
    | Merge(r1) -> 
        (fun a s d -> 
           let c = string_of_skel s in
           if a=get dl c then Some (c,d) else None)
    | Fiat(dl1) -> 
        (fun a s d -> unparse dl1 a s d)
    | Forgetkey(dl1) -> (fun a s d -> unparse dl1 a s d)
    | Permute(p1,dls) -> 
        (fun a s d -> 
           let k,sigma,sigma_inv,cts,ats = p1 in 
           let a_arr = arr_split_a k dls sigma_inv ats a in 
           let s_arr = Array.of_list (lst_of_skel s) in
           let c_arr = Array.create k "" in 
           let rec loop j d = 
             if j >= k then Some d
             else
               begin 
                 let i = sigma_inv.(j) in 
                 match unparse dls.(i) a_arr.(j) s_arr.(i) d with
                   | Some(ci,di) -> 
                       (c_arr.(i) <- ci;
                       loop (succ j) di)
                   | None -> None 
               end in
           Misc.map_option
             (fun d' -> (concat_array c_arr,d'))
             (loop 0 d))
    | Probe(t,dl1)       -> unparse dl1

  and create dl = match dl.desc with
    | Copy(r1)           -> (fun a d -> (a,d))
    | Clobber(r1,w1,f1)    -> (fun a d -> (f1 a,d))
    | Concat(dl1,dl2)    -> 
        (fun a d -> 
           let a1,a2 = seq_split (atype dl1) (atype dl2) a in
           let c1,d1 = create dl1 a1 d in
           let c2,d2 = create dl2 a2 d1 in 
           (c1 ^ c2,d2))
    | Union(dl1,dl2)     -> 
        (fun a d -> do_union (atype dl1) (create dl1) (create dl2) a d)
    | Star(dl1)          -> 
        (fun a d -> 
           Safelist.fold_left 
             (fun (buf,d) ai -> 
                let ci,d' = create dl1 ai d in                 
                (buf ^ ci, d'))
             ("",d) (Rx.star_split (atype dl1) a));
    | Key(r1)            -> (fun a d -> (a,d))
    | DMatch(t1,dl1)     -> 
        (fun a d -> do_match std_lookup (key dl1) (put dl1) (create dl1) t1 a d)
    | Compose(dl1,dl2)   -> 
        (fun a d -> 
           let b,d1 = create dl2 a d in 
           create dl1 b d1)
    | Invert(dl1)        -> (fun a d -> (get dl1 a),d)
    | Default(dl1,w1)    -> 
        let s1,d1 = parse dl1 w1 in 
        (fun a d -> put dl1 a s1 (d++d1))
    | LeftQuot(cn1,dl1)  -> 
        (fun a d -> 
           let c',d' = create dl1 a d in 
             (Canonizer.choose cn1 c',d'))
    | RightQuot(dl1,cn1) -> (fun a d -> create dl1 (Canonizer.canonize cn1 a) d)
    | Dup1(dl1,f1,r1)    -> 
        (fun a d -> 
           let a1,_ = seq_split (atype dl1) r1 a in
           create dl1 a1 d)
    | Dup2(f1,r1,dl1)    -> 
        (fun a d -> 
           let _,a2 = seq_split r1 (atype dl1) a in
           create dl1 a2 d)
    | SMatch(t1,f1,dl1)  ->
        (fun a d -> do_match (sim_lookup f1) (key dl1) (put dl1) (create dl1) t1 a d)
    | Partition(r1,r2)   -> (fun a d -> (a,d))
    | Merge(r1)          -> (fun a d -> (a ^ a,d))
    | Fiat(dl1)          -> (fun a d -> create dl1 a d)
    | Forgetkey(dl1)     -> (fun a d -> create dl1 a d)
    | Permute(p1,dls)    -> 
        (fun a d -> 
           let k,sigma,sigma_inv,cts,ats = p1 in 
           let a_arr = arr_split_a k dls sigma_inv ats a in 
           let c_arr = Array.create k "" in 
           let rec loop j d = 
             if j >= k then d
             else 
               begin 
                 let i = sigma_inv.(j) in 
                 let ci,di = create dls.(i) a_arr.(j) d in 
                 c_arr.(i) <- ci;
                 loop (succ j) di 
               end in
           let d' = loop 0 d in
           let c' = concat_array c_arr in 
           (c',d'))
    | Probe(t,dl1)       -> 
        (fun a d -> 
           msg "@[[[PROBE-CREATE{%s}@\n%s@\n]]@\n@]" t a;
           create dl1 a d)

  and key dl = match dl.desc with
    | Copy(r1)           -> (fun a -> "")
    | Clobber(r1,w1,f1)    -> (fun a -> "")
    | Concat(dl1,dl2)    -> 
        (fun a -> do_concat (atype dl1) (atype dl2) (key dl1) (key dl2) a)
    | Union(dl1,dl2)     -> 
        (fun a -> do_union (atype dl1) (key dl1) (key dl2) a)
    | Star(dl1)          -> 
        (fun a -> do_star (atype dl1) (key dl1) a)
    | Key(r1)            -> (fun a -> a)
    | DMatch(t1,dl1)     -> (fun a -> key dl1 a)
    | Compose(dl1,dl2)   -> (fun a -> key dl2 a)
    | Invert(dl1)        -> (fun a -> key dl1 (get dl1 a))
    | Default(dl1,w1)    -> (fun a -> key dl1 a)
    | LeftQuot(cn1,dl1)  -> (fun a -> key dl1 a)
    | RightQuot(dl1,cn1) -> (fun a -> key dl1 (Canonizer.canonize cn1 a))
    | Dup1(dl1,f1,r1)    -> 
        (fun a -> 
           let a1,_ = seq_split (atype dl1) r1 a in
           key dl1 a1)
    | Dup2(f1,r1,dl1)    -> 
        (fun a -> 
           let _,a2 = seq_split r1 (atype dl1) a in
           key dl1 a2)
    | SMatch(t1,f1,dl1)  -> (fun a -> key dl1 a)
    | Partition(r1,r2)   -> (fun a -> "")
    | Merge(r1)          -> (fun a -> "")
    | Fiat(dl1)          -> (fun a -> key dl1 a)
    | Forgetkey(dl1)     -> (fun a -> "")
    | Permute(p1,dls)    -> 
        (fun a -> 
           let k,sigma,sigma_inv,cts,ats = p1 in 
           let a_arr = arr_split_a k dls sigma_inv ats a in 
           let k_buf = Buffer.create 17 in 
           let rec loop j = 
             if j >= k then ()
             else 
               begin 
                 let kj = key dls.(sigma_inv.(j)) a_arr.(j) in 
                 Buffer.add_string k_buf kj;
                 loop (succ j) 
               end in
           loop 0;
           let ky = Buffer.contents k_buf in            
           ky)
    | Probe(t,dl1)          -> 
        (fun a -> 
           msg "@[[[PROBE-KEY{%s}@\n%s@\n]]@\n@]" t a;
           key dl1 a)

  and parse dl = match dl.desc with
    | Copy(r1)           -> (fun c -> (S_string c, TMap.empty))
    | Clobber(r1,w1,f1)    -> (fun c -> (S_string c, TMap.empty))
    | Concat(dl1,dl2)    -> 
        (fun c -> 
           let c1,c2 = seq_split (ctype dl1) (ctype dl2) c in 
           let s1,d1 = parse dl1 c1 in
           let s2,d2 = parse dl2 c2 in 
           (S_concat (s1,s2), d1++d2))
    | Union(dl1,dl2)     -> 
        (fun c -> do_union (ctype dl1) (parse dl1) (parse dl2) c)
    | Star(dl1)          -> 
        (fun c -> 
           let sl,d = 
             Safelist.fold_left 
               (fun (buf,d) ci -> 
                  let si,di = parse dl1 ci in
	          (si::buf, d++di))
               ([], TMap.empty)
               (Rx.star_split (ctype dl1) c) in
	   (S_star (Safelist.rev sl),d))
    | Key(r1)            -> (fun c -> (S_string c, TMap.empty))
    | DMatch(t1,dl1)     -> 
        (fun c -> 
           let s,d = parse dl1 c in 
           let d1,d2 = dt_split d t1 in 
           let k = key dl1 (get dl1 c) in 
           let km = KMap.add k [(s,d1)] KMap.empty in 
           (S_box t1,TMap.add t1 km d2))
    | Compose(dl1,dl2)   -> 
        (fun c -> 
           let s1,d1 = parse dl1 c in 
           let s2,d2 = parse dl2 (get dl1 c) in
           (S_comp (s1,s2), d2++d1))
    | Invert(dl1)        -> 
        (fun c -> (S_string c, TMap.empty))
    | Default(dl1,w1)    -> (fun c -> parse dl1 c)
    | LeftQuot(cn1,dl1)  -> 
        (fun c -> 
           let s1,d1 = parse dl1 (Canonizer.canonize cn1 c) in 
           (S_quot (c,s1),d1))
    | RightQuot(dl1,cn1) -> (fun c -> parse dl1 c)
    | Dup1(dl1,f1,r1)    -> (fun c -> parse dl1 c)
    | Dup2(f1,r1,dl1)    -> (fun c -> parse dl1 c)
    | SMatch(t1,f1,dl1)  -> 
        (fun c ->
           let s,d = parse dl1 c in 
           let d1,d2 = dt_split d t1 in 
           let k = key dl1 (get dl1 c) in 
           let km = KMap.add k [(s,d1)] KMap.empty in 
           (S_box t1,TMap.add t1 km d2))
    | Partition(r1,r2)   -> (fun c -> (S_string c, TMap.empty))
    | Merge(r1)          -> (fun c -> (S_string c, TMap.empty))
    | Fiat(dl1)          -> (fun c -> parse dl1 c)
    | Forgetkey(dl1)     -> (fun c -> parse dl1 c)
    | Permute(p1,dls)    -> 
        (fun c -> 
           let k,_,_,cts,_ = p1 in
           let sl,d,_ = 
             Safelist.fold_left 
               (fun (sl,d,i) ci -> 
                  let si,di = parse dls.(i) ci in
	          (si::sl, d++di, succ i))
               ([], TMap.empty,0)
               (Array.to_list (arr_split_c k dls cts c)) in 
	   (S_star (Safelist.rev sl),d))
    | Probe(_,dl1)       -> (fun c -> parse dl1 c)

  and rcreate dl = 
    (fun a -> fst (create dl a TMap.empty))

  and rput dl = 
    (fun a c -> 
       let s,d = parse dl c in
       fst (put dl a s d))       

  and rget dl = 
    (fun c -> get dl c)

  (* helper for dmatch and smatch *)
  and do_match lookup key hit miss tag a d = 
    let d1,d2 = dt_split d tag in 
    match lookup tag (key a) d1 with
      | Some ((s',d1'),d1'') -> 
          let c',d' = hit a s' (d1' ++ d2) in
          let _,d2' = dt_split d' tag in 
            (c',d1''++ d2') 
      | None -> 
          let c',d' = miss a d2 in 
          let _,d2' = dt_split d' tag in 
          (c',d1++d2')
                
  (* helpers for permute *)
  and arr_split_a k dls sigma_inv ats x = 
    let res = Array.create k "" in 
    let _ = 
      Array.fold_left
        (fun (i,a) j -> 
           let ai,arest = seq_split (atype dls.(j)) ats.(i) a in
             res.(i) <- ai;
             (succ i,arest))
        (0,x) sigma_inv in
    res 
        
  and arr_split_c k dls cts x = 
    let res = Array.create k "" in 
    let _ = 
      Array.fold_left 
        (fun (j,c) dlj -> 
           let cj,crest = seq_split (ctype dlj) cts.(j) c in
             res.(j) <- cj;
             (succ j,crest))
        (0,x) dls in 
    res 
  let info dl = dl.info

  let uid dl = dl.uid

  let format dl = msg ""

  let string dl = 
    Util.format_to_string 
      (fun () -> 
	 Util.format "@["; 
	 format dl; 
	 Util.format "@]")

  let arel_identity dl = arel dl = Identity

  let crel_identity dl = crel dl = Identity

  (* ----- constructors ----- *)
  let mk' i d = mk i d (calc_dtype d)
  let copy i r1 = mk' i (Copy(r1))
  let clobber i r1 w1 f1 = mk' i (Clobber(r1,w1,f1))
  let concat i dl1 dl2 = mk' i (Concat(dl1,dl2))
  let union i dl1 dl2 = mk' i (Union(dl1,dl2))
  let star i dl1 = mk' i (Star(dl1))
  let key i r1 = mk' i (Key(r1))
  let dmatch i t1 dl1 = mk' i (DMatch(t1,dl1))
  let compose i dl1 dl2 = mk' i (Compose(dl1,dl2))
  let invert i dl1 = mk' i (Invert(dl1))
  let default i dl1 w1 = mk' i (Default(dl1,w1))
  let left_quot i cn1 dl1 = mk' i (LeftQuot(cn1,dl1))
  let right_quot i dl1 cn1 = mk' i (RightQuot(dl1,cn1))
  let dup1 i dl1 f1 r1 = mk' i (Dup1(dl1,f1,r1))
  let dup2 i f1 r1 dl1 = mk' i (Dup2(f1,r1,dl1))
  let smatch i t1 f1 dl1 = mk' i (SMatch(f1,t1,dl1))
  let partition i r1 r2 = mk' i (Partition(r1,r2))
  let merge i r1 = mk' i (Merge(r1))
  let fiat i dl1 = mk' i (Fiat(dl1))
  let forgetkey i dl1 = mk' i (Forgetkey(dl1))
  let probe i t1 dl1 = mk' i (Probe(t1,dl1))
  let permute i is dls =
    let dl_arr = Array.of_list dls in 
    let k = Array.length dl_arr in 
    let sigma,sigma_inv = Permutations.permutation is k in 
    let ats = Array.create k Rx.empty in 
    let cts = Array.create k Rx.empty in 
    let _ = 
      Array.fold_right 
        (fun j (i,acc) -> 
           ats.(i) <- acc;
           (pred i,Rx.mk_seq (atype dl_arr.(j)) acc))
        sigma_inv (pred k,Rx.epsilon) in
    let _ = Array.fold_right 
      (fun dli (i,acc) -> 
         cts.(i) <- acc; 
         (pred i,Rx.mk_seq (ctype dli) acc))
      dl_arr (pred k,Rx.epsilon) in
      mk' i (Permute((k,sigma,sigma_inv,cts,ats),dl_arr))
        
  let canonizer_of_t i dl = 
    Canonizer.from_lens i (ctype dl) (atype dl) (arel dl) (get dl) (rcreate dl)
  let iter i dl1 min maxo =
    generic_iter (copy i Rx.epsilon) (union i) (concat i) (star i)
      min maxo dl1
end
