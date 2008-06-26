(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007-2008                                                     *)
(* J. Nathan Foster and Benjamin C. Pierce                                     *)
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
(* /boomerang/src/brx.ml                                                       *)
(* Boomerang RegExp engine                                                     *)
(* $Id$ *)
(*******************************************************************************)

(* This code is based on a similar module Jerome Vouillon wrote for
   Unison. *)

(* --------------------- CONSTANTS / HELPERS --------------------- *)

(* debugging *)
let dbg thk = Trace.debug "brx+" thk
let sdbg s = Trace.debug "brx+" (fun () -> Util.format "%s" s)

let string_of_char_code n = String.make 1 (Char.chr n) 
	   
(* ASCII alphabet *)
let min_code = 0
let max_code = 255

(* --------------------- PRETTY PRINTING --------------------- *)
(* ranks: used to determine when parentheses are needed. *)
type r = 
  | Urnk (* union *)
  | Drnk (* diff *)
  | Irnk (* inter *)
  | Crnk (* concat *)
  | Srnk (* star *)
  | Arnk (* atomic *)

let lpar r1 r2 = match r1,r2 with
  | Arnk, _ -> false
  | _, Arnk -> false
  | Srnk, _ -> false
  | _, Srnk -> true
  | Crnk, _ -> false
  | _, Crnk -> true
  | Irnk, _ -> false
  | _, Irnk -> true
  | Urnk, Drnk
  | Drnk, Urnk -> true
  | Drnk, Drnk -> false
  | Urnk, Urnk -> false
      
let rpar r1 r2 = match r1,r2 with
  | Arnk, _ -> false
  | _, Arnk -> false
  | _, Srnk -> true
  | Srnk, _ -> false
  | _, Crnk -> true
  | Crnk, _ -> false
  | _, Irnk -> true
  | Irnk, _ -> false
  | Urnk, Drnk
  | Drnk, Urnk -> true
  | Drnk, Drnk -> true
  | Urnk, Urnk -> true

(* --------------------- CHARACTER SETS --------------------- *)
module CharSet : 
sig
  type p = int * int 
  type t = p list
  val union : t -> t -> t
  val add : p -> t -> t
  val inter : t -> t -> t
  val negate : int -> int -> t -> t
  val diff : t -> t -> t
  val mem : int -> t -> bool
end = struct
  type p = int * int 
  type t = p list
  let rec union l1 l2 = match l1,l2 with
    | _,[] -> l1
    | [],_ -> l2
    | (c1,c2)::r1,(d1,d2)::r2 -> 
        if succ c2 < d1 then 
          (c1,c2)::union r1 l2
        else if succ d2 < c1 then 
          (d1,d2)::union l1 r2
        else if c1 < d2 then 
          union r1 ((min c1 d1,d2)::r2)
        else 
          union ((min c1 d1,c2)::r1) r2

  let add p1 l1 = union [p1] l1

  let rec inter l1 l2 = match l1, l2 with
    | _, [] -> []
    | [], _ -> []
    | (c1, c2)::r1, (d1, d2)::r2 ->
        if c2 < d1 then
          inter r1 l2
        else if d2 < c1 then
          inter l1 r2
        else if c2 < d2 then
          (max c1 d1, c2)::inter r1 l2
        else
          (max c1 d1, d2)::inter l1 r2
            
  let rec negate mi ma l = match l with
    | [] ->
        if mi <= ma then [(mi, ma)] else []
    | (c1, c2)::r ->  
        if ma < c1 then 
          if mi <= ma then [(mi, ma)] else []
        else if  mi < c1 then
          (mi, c1 - 1)::negate c1 ma l
        else (* i.e., c1 <= mi *) 
          negate (max mi (c2 + 1)) ma r 

  let diff l1 l2 = 
    inter l1 (negate min_code max_code l2)

  let mem c l = 
    Safelist.exists (fun (c1,c2) -> c1 <= c && c <= c2) l 
end

(* --------------------- REGULAR EXPRESSIONS --------------------- *)
(* we use a recursive module because t uses Q.ts in representative *) 
module rec M : sig 
  type d = 
    | Anything
    | Empty
    | Epsilon
    | CSet of CharSet.t
    | Alt of t * t list
    | Seq of t * t
    | Star of t
    | Inter of t * t list
    | Diff of t * t
  and t = {
    uid                    : int;
    desc                   : d;
    hash                   : int;
    final                  : bool;
    (* operations *)
    mutable derivative     : int -> t;
    mutable maps           : unit -> (int array * int array);
    mutable next           : unit -> t array;
    mutable reverse        : unit -> t;
    mutable representative : string * Q.t * (t * string * Q.t option) list -> 
			     (string,Q.t) Misc.alternative;
(*     mutable suffs          : unit -> t;   *)
    mutable suffs          : (t * Q.t * t list) -> t;
  } 
end = struct
  type d = 
    | Anything
    | Empty
    | Epsilon
    | CSet of CharSet.t
    | Alt of t * t list
    | Seq of t * t
    | Star of t
    | Inter of t * t list
    | Diff of t * t
  and t = {
    uid                    : int;
    desc                   : d;
    hash                   : int;
    final                  : bool;
    mutable derivative     : int -> t;
    mutable maps           : unit -> (int array * int array);
    mutable next           : unit -> t array;
    mutable reverse        : unit -> t;
    mutable representative : string * Q.t * (t * string * Q.t option) list ->
                                        (string,Q.t) Misc.alternative;
(*     mutable suffs          : unit -> t;   *)
    mutable suffs          : (t * Q.t * t list) -> t;
  } 
end 
and Q : Set.S with type elt = M.t = Set.Make(
  struct
    type t = M.t
    let compare t1 t2 = (t1.M.uid) - (t2.M.uid)
  end)
type t = M.t
open M

(* --------------------- PRETTY PRINTING --------------------- *)
let rank t0 = match t0.desc with
  | Anything -> Arnk
  | Empty    -> Arnk
  | Epsilon      -> Arnk
  | CSet _   -> Arnk
  | Star _   -> Srnk
  | Seq _    -> Crnk
  | Alt _    -> Urnk 
  | Inter _  -> Irnk
  | Diff _   -> Drnk 

let rec format_t t0 = 
  let format_char_code n = Util.format "%s" (string_of_char_code n) in       
  let format_char_code_pair (n1,n2) = 
    if n1=n2 then format_char_code n1 
    else (format_char_code n1; Util.format "-"; format_char_code n2) in 

  let maybe_wrap = Bprint.maybe_wrap format_t in

  let rec format_list sep rnk ri resti = 
    Util.format sep;
    match resti with 
      | [] -> 
          maybe_wrap (rpar (rank ri) rnk) ri
      | rj::restj -> 
          maybe_wrap (lpar (rank ri) rnk || rpar (rank ri) rnk) ri;
          format_list sep rnk rj restj in 
    
    match t0.desc with
      | Anything -> Util.format "@[ANY@]"
      | Empty -> Util.format "@[[]@]"
      | CSet [p1] -> 
          let n1,n2 = p1 in 
            Util.format "@[";
            if n1=min_code && n2=max_code then 
              Util.format "[.]"
            else if n1=n2 then 
              (Util.format "'";
               format_char_code n1;
               Util.format "'")
            else 
              (Util.format "[";
               format_char_code_pair p1;
               Util.format "]");
            Util.format "@]"
      | CSet cs -> 
          let ns = CharSet.negate min_code max_code cs in
          let p,l = 
            if Safelist.length ns < Safelist.length cs 
            then ("^",ns)
            else ("",cs) in           
            Util.format "@[[%s" p;
            Misc.format_list "" format_char_code_pair l;
            Util.format "]@]"
      | Epsilon -> 
          Util.format "@[\"\"@]"
      | Seq (t1,t2) -> 
	  Util.format "@[<1>";
	  maybe_wrap (lpar (rank t1) Crnk) t1;
	  Util.format ".@,";
	  maybe_wrap (rpar (rank t2) Crnk) t2;
	  Util.format "@]"
      | Alt (t1,[]) -> 
          format_t t1
      | Alt (t1,t2::rest) ->
          Util.format "@[";
          maybe_wrap (lpar (rank t1) Urnk) t1;
          format_list "@,|" Urnk t2 rest;
          Util.format "@]"
      | Star(t1) -> 
          Util.format "@[";
          maybe_wrap (lpar (rank t1) Srnk) t1;
          Util.format "*";
          Util.format "@]"
      | Inter(t1,[]) -> 
          format_t t1
      | Inter (t1,t2::rest) ->
          Util.format "@[<1>";
          maybe_wrap (lpar (rank t1) Irnk) t1;
          format_list "@,&" Urnk t2 rest;
          Util.format "@]"
      | Diff(t1,t2) -> 
          Util.format "@[<1>{";
          maybe_wrap (lpar (rank t1) Drnk) t1;
          Util.format "@,-";
          maybe_wrap (lpar (rank t2) Drnk) t2;
          Util.format "}@]"
            
let string_of_t t0 = 
  Util.format_to_string (fun () -> format_t t0)

(* --------------------- HASH CONS CACHES --------------------- *)
module MapCache = Map.Make(
  struct
    type t = (int * int list)
    let rec compare (h1,l1) (h2,l2) = 
      let rec aux l1 l2 = match l1,l2 with
        | [],[] -> 0
        | _,[]  -> 1
        | [],_  -> -1
        | h1::rest1,h2::rest2 -> 
            let c1 = h1 - h2 in 
            if c1 <> 0 then c1
            else aux rest1 rest2 in 
      aux (h1::l1) (h2::l2)
  end)

module ICache = Hashtbl.Make
  (struct 
     type t = int
     let hash x = Hashtbl.hash x
     let equal (x:int) (y:int) = x=y
   end)

module CSCache = Hashtbl.Make
  (struct
     type t = CharSet.t 
     let hash cs1 = Hashtbl.hash cs1
     let equal cs1 cs2 = cs1 = cs2
   end)

module TCache = Hashtbl.Make
  (struct
     type t = M.t
     let hash t = t.hash
     let equal t1 t2 = t1.uid = t2.uid
   end)

module TTCache = Hashtbl.Make
  (struct 
     type t = M.t * M.t
     let hash (t1,t2) = (883 * t1.hash + 859 * t2.hash)
     let equal (t11,t12) (t21,t22) = t11.uid = t21.uid && t12.uid = t22.uid
   end)

module TLCache = Hashtbl.Make
  (struct
     type t = M.t list
     let hash tl = Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 tl
     let rec equal tl1 tl2 = match tl1,tl2 with 
       | h1::rest1,h2::rest2 -> 
	   h1.uid = h2.uid && equal rest1 rest2
       | [],[] -> true
       | _ -> false
   end)

let mcache : (int array * int array) MapCache.t ref = ref MapCache.empty
let cset_cache : t CSCache.t = CSCache.create 131
let neg_cset_cache : t CSCache.t = CSCache.create 131
let star_cache : t TCache.t = TCache.create 131
let seq_cache : t TTCache.t = TTCache.create 131
let alt_cache : t TTCache.t = TTCache.create 131
let inter_cache : t TTCache.t = TTCache.create 131
let diff_cache : t TTCache.t = TTCache.create 131
let seqs_cache : t TLCache.t = TLCache.create 131
let alts_cache : t TLCache.t = TLCache.create 131
let inters_cache : t TLCache.t = TLCache.create 131

(* --------------------- DESC OPERATIONS --------------------- *)
let desc_hash = function
  | Anything     -> 181
  | Empty        -> 443
  | Epsilon      -> 1229
  | CSet(cs)     -> 
      let rec aux = function
        | [] -> 0
        | (i,j)::r -> i + 13 * j + 257 * aux r in 
        aux cs land 0x3FFFFFFF
  | Alt(t1,tl)   -> 199 * Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 (t1::tl)
  | Seq(t1,t2)   -> 821 * t1.hash + 919 * t2.hash
  | Inter(t1,tl) -> 71 * Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 (t1::tl)
  | Diff(t1,t2)  -> 379 * t1.hash + 563 * t2.hash
  | Star(t1)     -> 197 * t1.hash

let desc_final = function
  | Anything     -> true
  | Empty        -> false
  | Epsilon      -> true
  | CSet _       -> false
  | Star _       -> true
  | Seq(t1,t2)   -> t1.final && t2.final
  | Alt(t1,tl)   -> t1.final || Safelist.exists (fun ti -> ti.final) tl
  | Inter(t1,tl) -> t1.final && Safelist.for_all (fun ti -> ti.final) tl
  | Diff(t1,t2)  -> t1.final && not t2.final

(* character maps (hash consed) *)
let desc_maps d0 = 
  let rec split m cs = match cs with
    | [] -> ()
    | (c1,c2)::rest ->
        m.(c1) <- true;
        m.(succ c2) <- true;
        split m rest in
  let rec desc_colorize m d = match d with
    | Anything     -> ()
    | Empty        -> ()
    | Epsilon          -> ()
    | CSet cs      -> split m cs
    | Star(t1)     -> colorize m t1
    | Seq(t1,t2)   -> colorize m t1; if t1.final then colorize m t2
    | Alt(t1,tl)   -> colorize m t1; Safelist.iter (colorize m) tl
    | Inter(t1,tl) -> colorize m t1; Safelist.iter (colorize m) tl
    | Diff(t1,t2)  -> colorize m t1; colorize m t2 
  and colorize m t = desc_colorize m t.desc in
  let key_of_map m =
    let ws = 31 in
    let rec loop i mask cont a1 al =
      if i > max_code then (a1::al)
      else if cont && i mod ws = 0 then
        loop i 1 false 0 (a1::al)
      else
        let mask' = mask lsl 1 in
        let a1' = if m.(i) then mask lor a1 else a1 in
          loop (succ i) mask' true a1' al in
    let l = loop 0 1 false 0 [] in 
    (Hashtbl.hash l,l) in 
  let flatten m = 
    let km = key_of_map m in
    try MapCache.find km !mcache 
    with Not_found ->
      let cm = Array.make (succ max_code) 0 in 
      let rec loop i nc rml = 
        if i > max_code then Safelist.rev rml
        else
          let nc' = if m.(i) then succ nc else nc in 
          let rml' = if m.(i) then i::rml else rml in 
            (cm.(i) <- nc';
             loop (succ i) nc' rml') in 
      let rml = loop 1 0 [0] in
      let ms = (cm,Array.of_list rml) in
        mcache := MapCache.add km ms !mcache;
        ms in 
  let m = Array.make (succ (succ max_code)) false in 
    desc_colorize m d0;
    flatten m

(* --------------------- CONSTRUCTORS --------------------- *)
let uid_counter = ref 0 
let next_uid () = 
  incr uid_counter;
  !uid_counter

let install upd f = 
  (fun args -> 
     let v = f args in 
     upd (fun _ -> v); 
     v)

let dummy_impl _ = assert false  

let mk_constant d is_final t_nexto t_repalt= 
  let (cm,rm) as ms = desc_maps d in 
  let t = 
    { uid = next_uid ();
      desc = d;
      hash = desc_hash d;
      final = is_final;
      derivative = dummy_impl;
      maps = dummy_impl;
      next = dummy_impl;
      reverse = dummy_impl;
      representative = dummy_impl;
      suffs = dummy_impl; } in 
  let t_next = match t_nexto with None -> t | Some t' -> t' in 
  let tr = Array.create (Array.length rm) t_next in
    t.maps <- (fun () -> ms);
    t.derivative <- (fun c -> t_next);
    t.next <- (fun () -> tr);
    t.reverse <- (fun () -> t);
    t.representative <- (fun _ -> t_repalt);
    t.suffs <- (fun _ -> t);
    t 

(* CONSTANTS *)
let empty = mk_constant Empty false None (Misc.Right Q.empty)
let anything = mk_constant Anything true None (Misc.Left "")
let epsilon = mk_constant Epsilon true (Some empty) (Misc.Left "")

(* GENERIC CONSTRUCTOR *)
let rec mk_t d0 = 
  let t0 = 
    { uid = next_uid ();
      desc = d0;
      hash = desc_hash d0;
      final = desc_final d0;      
      derivative = dummy_impl;
      maps = dummy_impl;
      next = dummy_impl;
      reverse = dummy_impl;
      representative = dummy_impl;
      suffs = dummy_impl; } in 

  let maps_impl  = 
    install 
      (fun g -> t0.maps <- g) 
      (fun () -> desc_maps d0) in


  let derivative_impl = 
    let mk_memo f = 
      let der_cache : t ICache.t = ICache.create 17 in
      let fr = ref f in 
      (fun c -> 
         try ICache.find der_cache c
         with Not_found -> 
           let r = !fr c in 
           ICache.add der_cache c r;
           r) in 
    match d0 with 
    | Anything -> (fun c -> t0)
    | Empty    -> (fun c -> t0)
    | Epsilon  -> (fun c -> empty)
    | CSet s   -> mk_memo (fun c -> if CharSet.mem c s then epsilon else empty)
    | Seq(t1,t2) ->
        mk_memo
          (fun c -> 
	     let t12 = mk_seq (t1.derivative c) t2 in 
	       if t1.final then mk_alt t12 (t2.derivative c)
	       else t12)
    | Alt (t1,tl) -> 
        mk_memo
          (fun c -> mk_alts (Safelist.map (fun ti -> ti.derivative c) (t1::tl)))
    | Star(t1) -> 
        mk_memo
          (fun c -> mk_seq (t1.derivative c) (mk_star t0))
    | Inter(t1,tl) ->
        mk_memo 
          (fun c -> mk_inters (Safelist.map (fun ti -> ti.derivative c) (t1::tl)))
    | Diff(t1,t2) -> 
        mk_memo 
          (fun c -> mk_diff (t1.derivative c) (t2.derivative c)) in 

  let next_impl = 
    install
      (fun g -> t0.next <- g)
      (fun () -> 
         let (_,rm) = t0.maps () in
         let nc = Array.length rm in
         let tr = Array.create nc t0 in 
         for i = 0 to pred nc do 
           tr.(i) <- t0.derivative rm.(i) 
         done;
         tr) in

  let reverse_impl = 
    let install_rev thk = install (fun g -> t0.reverse <- g) thk in 
    match d0 with 
      | Anything     -> (fun () -> t0)
      | Empty        -> (fun () -> t0)
      | Epsilon      -> (fun () -> t0)
      | CSet _       -> (fun () -> t0)
      | Seq(t1,t2)   -> install_rev (fun () -> mk_seq (t2.reverse ()) (t1.reverse ()))
      | Alt(t1,tl)   -> install_rev (fun () -> mk_alts (Safelist.map (fun ti -> ti.reverse ()) (t1::tl)))
      | Star(t1)     -> install_rev (fun () -> mk_star (t1.reverse ()))
      | Inter(t1,tl) -> install_rev (fun () -> mk_inters (Safelist.map (fun ti -> ti.reverse ()) (t1::tl)))
      | Diff(t1,t2)  -> install_rev (fun () -> mk_diff (t1.reverse ()) (t2.reverse ())) in 

  let representative_impl = 
    let install_rep g = install (fun g -> t0.representative <- g) g in 
    let jump f p = match p with 
      | [] -> Misc.Right f
      | (ti,wi,None)::rest -> ti.representative (wi,f,rest) 
      | (ti,wi,Some f')::rest -> ti.representative (wi,f',rest) in
    let add w b ci ti f p = 
      if Q.mem ti f then p
      else if b then (ti,w ^ ci,Some f)::p
      else (ti,w ^ ci, None)::p in 
    let full_search (w,f,p) = 
      let f' = Q.add t0 f in 
      if t0.final then Misc.Left w
      else if Q.mem t0 f then jump f' p 
      else 
        let tr = t0.next () in 
	let _,rm = t0.maps () in 
        let len = Array.length tr in 
        let rec loop acc i = 
          if i < 0 then acc 
          else 
            let ti = tr.(i) in 
            let ci = String.make 1 (Char.chr rm.(i)) in 
            let acc' = add w (i = 0) ci ti f' acc in
            loop acc' (pred i) in
       jump f' (loop p (pred len)) in 

    match d0 with
      | Anything | Epsilon | Star _ -> 
          (fun (w,_,_) -> Misc.Left w)
      | Empty | CSet [] -> 
          install_rep
            (fun (_,f,p) -> 
               let f' = Q.add t0 f in 
               jump f' p)
      | CSet((c1,_)::_) -> 
          (fun (w,_,_) -> Misc.Left (w ^ string_of_char_code c1))
      | Seq(t1,t2) -> 
          install_rep 
            (fun (w,f,p) -> 
               let f' = Q.add t0 f in 
               match t1.representative (w,f',p) with 
                 | Misc.Right _ as res -> res
                 | Misc.Left w1 -> match t2.representative ("",f',p) with
                     | Misc.Right _ as res -> res
                     | Misc.Left w2 -> Misc.Left (w1 ^ w2))
      | Alt(t1,tl) -> 
          install_rep 
            (fun (w,f,p) -> 
               let f' = Q.add t0 f in 
               let rec loop acc l = match l with
                 | [] -> acc
                 | ti::rest -> 
                     let acc' = add w (rest = []) "" ti f' acc in 
		       loop acc' rest in 
               (* rev preserves compatibility with old engine *)
               let tl_rev = Safelist.rev (t1::tl) in 
               jump f' (loop p tl_rev))

      | Diff(t1,t2) -> 
          install_rep
            (match t1.desc with
               | Alt(t11,tl1) ->                 
                   (fun (w,f,p) -> 
                      let f' = Q.add t0 f in 
                      let rec loop acc l = match l with
                        | [] -> acc
                        | ti::rest -> 
                            let ti_d_t2 = mk_diff ti t2 in
                            let acc' = add w (rest = []) "" ti_d_t2 f' acc in 
                              loop acc' rest in 
		      let tl_rev = Safelist.rev (t11::tl1) in 
                      jump f' (loop p tl_rev))
               | _ -> full_search)
      | _ -> install_rep full_search in

  let suffs_impl = 
    let upd g = t0.suffs <- g in 
    let install_suffs g = install upd g in
    let add ti f p = if Q.mem ti f then p else (ti::p) in 
    let jump tacc f p = match p with 
      | [] -> tacc
      | ti::rest -> ti.suffs (tacc,f,rest) in 
    let full_search (tacc,f,p) = 
      let f' = Q.add t0 f in 
      if Q.mem t0 f then jump tacc f' p 
      else if t0.final then jump (mk_alt tacc t0) f' p
      else 
        let tr = t0.next () in 
	let len = Array.length tr in 
        let rec loop acc i = 
          if i < 0 then acc 
          else 
            let ti = tr.(i) in 
            let acc' = add ti f p in
            loop acc' (pred i) in
        jump tacc f' (loop p (pred len)) in 

    match d0 with 
      | Anything | Empty | Epsilon | Star _ -> 
          (fun _ -> t0)
      | CSet _ -> 
          (fun _ -> epsilon)
      | Alt(t1,tl) -> 
          install_suffs 
            (fun (tacc,f,p) -> 
               let f' = Q.add t0 f in 
               let p' = 
                 Safelist.fold_left 
                   (fun acc ti -> add ti f' acc) 
                   p 
                   (Safelist.rev (t1::tl)) in 
               jump tacc f' p')
      | Seq(t1,t2) ->           
          install_suffs 
            (fun (tacc,f,p) -> 
               if not t2.final then t2.suffs (tacc,f,p)
               else mk_alt (t1.suffs (tacc,f,p)) (t2.suffs (tacc,f,p)))
      | _ -> install_suffs full_search in
      
  (* backpatch t0 with implementations of the operations *)  
  t0.maps <- maps_impl;
  t0.derivative <- derivative_impl;
  t0.next <- next_impl;
  t0.reverse <- reverse_impl;
  t0.representative <- representative_impl;
  t0.suffs <- suffs_impl;
  t0

and mk_cset cs = match cs with
  | [] -> empty
  | _ -> 
      let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
      try CSCache.find cset_cache cs' 
      with Not_found -> 
	let res = mk_t (CSet cs') in 
	CSCache.add cset_cache cs' res;
	res

and mk_neg_cset cs = 
  let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
    match CharSet.negate min_code max_code cs' with 
      | [] -> empty
      | cs'' -> 
	  try CSCache.find neg_cset_cache cs''
	  with Not_found -> 
	    let res = mk_t (CSet cs'') in 
	    CSCache.add neg_cset_cache cs'' res;
	    res

and mk_seq t1 t2 = 
  let p = (t1,t2) in 
  try TTCache.find seq_cache p
  with Not_found -> 
    let rec aux acc ti = match ti.desc with
      | Seq(ti1,ti2) -> aux (ti1::acc) ti2 
      | _            -> Safelist.fold_left (fun acc ti -> mk_t(Seq(ti,acc))) t2 (ti::acc) in
    let res = match t1.desc,t2.desc with
      | Epsilon,_       -> t2
      | _,Epsilon       -> t1
      | Empty,_         -> empty
      | _,Empty         -> empty
      | _               -> aux [] t1 in 
   TTCache.add seq_cache p res;
   res   

and mk_seqs tl = 
  try TLCache.find seqs_cache tl
  with Not_found -> 
    let res = Safelist.fold_left mk_seq epsilon tl in 
    TLCache.add seqs_cache tl res;
    res

and mk_alt t1 t2 = 
  let p = (t1,t2) in 
  try TTCache.find alt_cache p
  with Not_found -> 
    let rec go acc l = match acc,l with
      | (t,[]),[] -> 
          t
      | (t,t1::l1),[] -> 
          if t = empty then mk_t(Alt(t1,l1))
          else if t = anything then anything
          else mk_t(Alt(t,t1::l1))
      | (t,l1),(t1::rest) -> 
          if t = empty then go (t1,l1) rest 
          else if t = anything then anything
          else go (t1,t::l1) rest in 
    let rec merge acc l1 l2 = match l1,l2 with 
      | [],[] -> begin match acc with
          | [] -> empty
          | t1::rest -> go (t1,[]) rest
	end
      | t1::l1',[] -> merge (t1::acc) l1' []
      | [],t2::l2' -> merge (t2::acc) [] l2'
      | t1::l1',t2::l2' ->           
          let c = compare t1 t2 in 
            if c=0 then merge (t1::acc) l1' l2'
            else if c < 0 then merge (t1::acc) l1' l2
            else merge (t2::acc) l1 l2' in 
    let res = match t1.desc,t2.desc with
	| Empty,_               -> t2
	| _,Empty               -> t1
	| Anything,_            -> t1
	| _,Anything            -> t2
	| CSet s1,CSet s2       -> mk_cset (CharSet.union s1 s2)
	| Alt(t1,l1),Alt(t2,l2) -> merge [] (t1::l1) (t2::l2)
	| Alt(t1,l1),_          -> merge [] (t1::l1) [t2]
	| _,Alt(t2,l2)          -> merge [] [t1] (t2::l2)
	| _                     -> merge [] [t1] [t2] in 
    TTCache.add alt_cache p res;
    res
	    
and mk_alts tl = 
  try TLCache.find alts_cache tl
  with Not_found -> 
    let res = Safelist.fold_right mk_alt tl empty in 
    TLCache.add alts_cache tl res;
    res

and mk_star t0 = 
  try TCache.find star_cache t0 
  with Not_found -> 
    let res = 
      match t0.desc with 
	| Epsilon     -> epsilon
	| Empty       -> epsilon
	| Anything    -> anything
	| Star _      -> t0
	| CSet[mi,ma] -> 
	    if mi=min_code && ma=max_code then anything
	    else mk_t(Star t0)
	| _  -> mk_t(Star t0) in 
    TCache.add star_cache t0 res;
    res
        
and mk_inter t1 t2 = 
  let p = (t1,t2) in 
  try TTCache.find inter_cache p
  with Not_found -> 
    let rec go acc l = match acc,l with
      | (t,[]),[] -> 
          t
      | (t,t1::l1),[] -> 
          if t = empty then empty
          else if t = anything then mk_t(Inter(t1,l1))
          else mk_t(Inter(t,t1::l1))
      | (t,l1),(t1::rest) -> 
          if t = empty then empty
          else if t = anything then go (t1,l1) rest
          else go (t1,t::l1) rest in
    let rec merge acc l1 l2 = match l1,l2 with 
      | [],[] -> begin match acc with
          | [] -> anything
          | t1::rest -> 
              go (t1,[]) rest
	end
      | t1::l1',[] -> merge (t1::acc) l1' []
      | [],t2::l2' -> merge (t2::acc) [] l2'
      | t1::l1',t2::l2' ->           
          let c = compare t1 t2 in 
            if c=0 then merge (t1::acc) l1' l2'
            else if c < 0 then merge (t1::acc) l1' l2
            else merge (t2::acc) l1 l2' in 
    let res = match t1.desc,t2.desc with
	| Empty,_                   -> empty
	| _,Empty                   -> empty
	| Anything,_                -> t2
	| _,Anything                -> t1
	| Epsilon,_                 -> if t2.final then t1 else empty
	| _,Epsilon                 -> if t1.final then t2 else empty
	| CSet s1,CSet s2           -> mk_cset (CharSet.inter s1 s2)
	| Inter(t1,l1),Inter(t2,l2) -> merge [] (t1::l1) (t2::l2)
	| Inter(t1,l1),_            -> merge [] (t1::l1) [t2]
	| _,Inter(t2,l2)            -> merge [] [t1] (t2::l2)
	| _                         -> merge [] [t1] [t2] in
    TTCache.add inter_cache p res;
    res


and mk_inters tl = 
  try TLCache.find inters_cache tl
  with Not_found -> 
    let res = Safelist.fold_left mk_inter anything tl in 
    TLCache.add inters_cache tl res;
    res

and mk_diff t1 t2 = 
  let p = (t1,t2) in 
  try TTCache.find diff_cache p 
  with Not_found -> 
    let res = 
      if t1.uid = t2.uid then empty 
      else
	match t1.desc,t2.desc with
	  | _,Anything       -> empty
	  | Empty,_          -> empty
	  | _,Empty          -> t1
	  | CSet s1, CSet s2 -> mk_cset (CharSet.diff s1 s2)
	  | CSet _,Epsilon   -> t1
	  | Star t11,Epsilon -> mk_seq (mk_diff t11 t2) t1
	  | Epsilon,_        -> if t2.final then empty else epsilon
	  | Diff(t11,t12),_  -> mk_t(Diff(t11,mk_alt t12 t2))
	  | Inter(t1,tl),_   -> mk_inters (Safelist.map (fun ti -> mk_diff ti t2) (t1::tl))
	  | _                -> mk_t(Diff(t1,t2)) in 
    TTCache.add diff_cache p res;
    res

(* OPERATIONS *)
let mk_complement t0 = mk_diff anything t0

let mk_reverse t0 = t0.reverse ()

let representative t0 = match t0.representative ("",Q.empty,[]) with
  | Misc.Left w -> Some w
  | Misc.Right _ -> None

let is_empty t0 = representative t0 = None

let suffs t0 = t0.suffs (empty,Q.empty,[])
(* let suffs t0 = t0.suffs () *)

let splittable_cex t1 t2 = 
  let t2_rev = mk_reverse t2 in 
  let overlap_or_epsilon = mk_inter (suffs t1) (mk_reverse (suffs t2_rev)) in 
  let overlap = mk_diff overlap_or_epsilon epsilon in 
  representative overlap

let iterable_cex t1 = 
  splittable_cex t1 (mk_star t1)

let match_string t0 w = 
  let n = String.length w in 
  let rec loop i ti =     
    if i = n then ti.final
    else loop (succ i) (ti.derivative (Char.code w.[i])) in 
  loop 0 t0
      
let match_string_positions t0 w = 
  let n = String.length w in 
  let rec loop acc i ti = 
    let acc' = 
      if ti.final then Int.Set.add i acc 
      else acc in 
    if i=n then acc'
    else loop acc' (succ i) (ti.derivative (Char.code w.[i])) in 
  loop Int.Set.empty 0 t0

let match_prefix_positions t0 w = 
  let n = String.length w in 
  let rec loop acc i ti = 
    let acc' = 
      if is_empty ti then acc else Int.Set.add i acc in
    if i=n then acc'
    else loop acc' (succ i) (ti.derivative (Char.code w.[i])) in
  loop Int.Set.empty 0 t0

let match_string_reverse_positions t0 w = 
  let n = String.length w in 
  let rec loop acc i ti = 
    let acc' = 
      if ti.final then Int.Set.add (succ i) acc 
      else acc in 
    if i < 0 then acc'
    else loop acc' (pred i) (ti.derivative (Char.code w.[i])) in
  loop Int.Set.empty (pred n) t0

let mk_string s = 
  let n = String.length s in 
  let rec loop i acc = 
    if i >= n then acc
    else
      let m = Char.code s.[pred n-i] in 
      let ti = mk_cset [(m,m)] in 
      loop (succ i) (ti::acc) in 
  mk_seqs (loop 0 [])

let disjoint_cex s1 s2 = 
  representative (mk_inter s1 s2) 

let disjoint s1 s2 = 
  is_empty (mk_inter s1 s2) 

let equiv s1 s2 = 
  is_empty (mk_diff s1 s2) 
  && is_empty (mk_diff s2 s1) 

let splittable s1 s2 = match splittable_cex s1 s2 with 
  | None -> true
  | Some _ -> false

let iterable s0 = match iterable_cex s0 with 
  | None -> true
  | Some _ -> false

let is_singleton s0 = 
  match representative s0 with 
    | None -> false
    | Some w -> is_empty (mk_diff s0 (mk_string w))

let split_positions t1 t2 w = 
  let ps1 = match_string_positions t1 w in 
  let ps2 = match_string_reverse_positions (mk_reverse t2) w in 
    Int.Set.inter ps1 ps2

let split_bad_prefix t1 s = 
  let ps = Int.Set.add 0 (match_prefix_positions t1 s) in 
  let n = String.length s in
  let j = Int.Set.max_elt ps in
    (String.sub s 0 j, String.sub s j (n-j))

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

