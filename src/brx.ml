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

(* Based on Jerome Vouillon's code from Unison. *)

(* --------------------- CONSTANTS / HELPERS --------------------- *)

(* debugging *)
let () = Format.set_margin 300
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
  | Srnk, _ -> false
  | _, Srnk -> true
  | Crnk, _ -> false
  | _, Crnk -> true
  | Irnk, _ -> false
  | _, Irnk -> true
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
type d = 
  | Anything
  | Empty
  | Epsilon
  | CSet of CharSet.t
  | Seq of t * t
  | Alt of t list
  | Rep of t * int * int option
  | Inter of t list
  | Diff of t * t
and t = 
    { uid                        : int;
      desc                       : d;
      hash                       : int;
      size                       : int;
      final                      : bool;
      simple                     : bool;
      (* lazily computed *)     
      mutable maps               : (int array * int array * int) option;
      mutable reverse            : t option;
      mutable representative     : (string option) option;
      mutable suffs              : t option;
      (* operations *)           
      mutable derivative         : int -> t }

type this_t = t 

let compare_t t1 t2 = compare (t1.uid,t1.size) (t2.uid,t2.size)

module Q = Set.Make(
  struct
    type t = this_t
    let compare t1 t2 = compare_t t1 t2
  end)
      
(* --------------------- PRETTY PRINTING --------------------- *)
let rank t0 = match t0.desc with
  | Anything -> Arnk
  | Empty    -> Arnk
  | Epsilon  -> Arnk
  | CSet _   -> Arnk
  | Rep _    -> Srnk
  | Seq _    -> Crnk
  | Alt _    -> Urnk 
  | Inter _  -> Irnk
  | Diff _   -> Drnk 

let tag_of_t t = 
  let rec aux b t =
  match t.desc with
  | Anything -> "Anything"
  | Empty    -> "Empty"
  | Epsilon  -> "Epsilon"
  | CSet _   -> "CSet"
  | Rep _    -> "Rep"
  | Seq _    -> "Seq"
  | Alt _    -> "Alt" 
  | Inter tl -> if b then "Inter[" ^ Misc.concat_list "," (Safelist.map (aux false) tl) ^ "]" else "Inter"
  | Diff _   -> "Diff" in 
  aux true t

let rec format_t t0 = 
  let format_char_code n = Util.format "%s" (Misc.whack (string_of_char_code n)) in 
  let format_char_code_pair (n1,n2) = 
    if n1=n2 then format_char_code n1 
    else (format_char_code n1; Util.format "-"; format_char_code n2) in 

  let wrap l r f = Util.format "%s" l; f (); Util.format "%s" r in 
  let maybe_wrap = Bprint.maybe_wrap format_t in
  let rec format_list sep rnk l = match l with
    | [] -> ()
    | [ti] -> maybe_wrap (rpar (rank ti) rnk) ti
    | ti::rest -> 
        maybe_wrap (lpar (rank ti) rnk || rpar (rank ti) rnk) ti;
        Util.format "%s" sep;
        format_list sep rnk rest in 
    
    Util.format "@[";
    begin match t0.desc with
      | Anything -> Util.format "ANYTHING"
      | Empty    -> Util.format "EMPTY"
      | Epsilon  -> Util.format "EPSILON"
      | CSet [p1] -> 
          let n1,n2 = p1 in 
            if n1=min_code && n2=max_code then Util.format "[.]"
            else if n1=n2 then wrap "'" "'" (fun () -> format_char_code n1)
            else wrap "[" "]" (fun () -> format_char_code_pair p1)
      | CSet cs -> 
          let ns = CharSet.negate min_code max_code cs in
          let p,l = if Safelist.length ns < Safelist.length cs then ("^",ns) else ("",cs) in
          wrap "[" "]" 
            (fun () -> 
               Util.format "%s" p; 
               Misc.format_list "" format_char_code_pair l)
      | Rep(ti,i,jo) -> 
          let format_rep i jo = match i,jo with
            | 0,None -> Util.format "*"
            | 1,None -> Util.format "+"
            | i,None -> Util.format "{%d,}" i
            | 0,Some 1 -> Util.format "?"
            | i,Some j -> Util.format "{%d,%d}" i j in 
          maybe_wrap (lpar (rank t0) Srnk) ti;
          format_rep i jo;
      | Seq(t1,t2)  -> format_list "." Crnk [t1;t2]
      | Alt ts      -> format_list "|" Urnk ts;
      | Inter ts    -> format_list "&" Urnk ts;
      | Diff(t1,t2) -> format_list "-" Drnk [t1;t2]
    end;
    Util.format "@]"

let string_of_t t0 = Util.format_to_string (fun () -> format_t t0)

let size_of_t t = t.size 
let compare_size (t1,_) (t2,_) = t1.size - t2.size

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
     type t = this_t
     let hash t = t.hash
     let equal t1 t2 = t1.uid = t2.uid
   end)

module TIIOCache = Hashtbl.Make
  (struct
     type t = this_t * int * int option
     let hash (t,i,jo) = 197 + 137 * i + (match jo with None -> 552556457 | Some j -> j)
     let equal (t1,i1,jo1) (t2,i2,jo2) = t1.uid = t2.uid && i1 = i2 && jo1 = jo2
   end)

module TTCache = Hashtbl.Make
  (struct 
     type t = this_t * this_t
     let hash (t1,t2) = (883 * t1.hash + 859 * t2.hash)
     let equal (t11,t12) (t21,t22) = t11.uid = t21.uid && t12.uid = t22.uid
   end)

let rec tl_eq l1 l2 = match l1,l2 with
  | [],[] -> true
  | t1::rest1,t2::rest2 -> t1.uid = t2.uid && tl_eq rest1 rest2 
  | _ -> false

module TTLTCache = Hashtbl.Make
  (struct
     type t = this_t * (int * this_t list) * this_t
     let hash (t1,(x,_),t2) = 883 * t1.hash + x + 883 * t2.hash
     let equal (t11,(_,tl12),t13) (t21,(_,tl22),t23) = 
       t11.uid = t21.uid && t13.uid = t23.uid && tl_eq tl12 tl22
   end)

module TLCache = Hashtbl.Make
  (struct
     type t = (int * this_t list)
     let hash (x,_) = x
     let equal (_,l1) (_,l2) = tl_eq l1 l2
   end)

let mcache : (int array * int array * int) MapCache.t ref = ref MapCache.empty
let cset_cache : t CSCache.t = CSCache.create 131
let neg_cset_cache : t CSCache.t = CSCache.create 131
let iter_cache : t TIIOCache.t = TIIOCache.create 131
let seq_cache : t TTLTCache.t = TTLTCache.create 131
let alt_cache : t TLCache.t = TLCache.create 131
let inter_cache : t TLCache.t = TLCache.create 131
let diff_cache : t TTCache.t = TTCache.create 131

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
  | Alt tl           -> 199 * Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 tl
  | Seq(t1,t2)       -> 821 * t1.hash + 919 * t2.hash
  | Inter tl         -> 71 * Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 tl
  | Diff(t1,t2)      -> 379 * t1.hash + 563 * t2.hash
  | Rep(t1,i,Some j) -> 197 * t1.hash + 137 * i + j
  | Rep(t1,i,None)   -> 197 * t1.hash + 137 * i + 552556457

let desc_size = function
  | Anything     -> 1
  | Empty        -> 1
  | Epsilon      -> 1
  | CSet(cs)     -> 1
  | Alt tl       -> Safelist.fold_left (fun s ti -> s + ti.size) 0 tl + 1
  | Seq(t1,t2)   -> t1.size + t2.size + 1
  | Inter tl     -> Safelist.fold_left (fun s ti -> s + ti.size) 0 tl + 1
  | Diff(t1,t2)  -> t1.size + t2.size + 1
  | Rep(t1,i,_) -> i * t1.size + 1

let desc_final = function
  | Anything    -> true
  | Empty       -> false
  | Epsilon     -> true
  | CSet _      -> false
  | Rep(t1,0,_) -> true
  | Rep(t1,_,_) -> t1.final
  | Seq(t1,t2)  -> t1.final && t2.final
  | Alt tl      -> Safelist.exists (fun ti -> ti.final) tl
  | Inter tl    -> Safelist.for_all (fun ti -> ti.final) tl
  | Diff(t1,t2) -> t1.final && not t2.final

let desc_simple = function
  | Anything    -> false
  | Empty       -> true
  | Epsilon     -> true
  | CSet _      -> true
  | Rep(t1,_,_) -> false
  | Seq(t1,t2)  -> t1.simple && t2.simple
  | Alt tl      -> false
  | Inter tl    -> false
  | Diff(t1,t2) -> false

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
    | Epsilon      -> ()
    | CSet cs      -> split m cs
    | Rep(t1,_,_)  -> colorize m t1
    | Seq(t1,t2)   -> colorize m t1; if t1.final then colorize m t2
    | Alt tl       -> Safelist.iter (fun ti -> colorize m ti) tl
    | Inter tl     -> Safelist.iter (fun ti -> colorize m ti) tl
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
      let rm = Array.of_list rml in 
      let len = Array.length rm in 
      let ms = (cm,rm,len) in
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

let mk_constant d t_nexto t_repo = 
  let fin = desc_final d in 
  let sim = desc_simple d in 
  let t = 
    { uid = next_uid ();
      desc = d;
      hash = desc_hash d;
      size = 1;
      final = fin;
      simple = sim;
      maps = Some (desc_maps d);
      reverse = None;
      representative = Some t_repo;
      suffs = None;
      derivative = dummy_impl; } in 
  let t_next = match t_nexto with 
    | None -> t 
    | Some t' -> t' in 
  (* backpatch *)
  t.reverse <- Some t;
  t.suffs <- Some t;
  t.derivative <- (fun c -> t_next);  
  t

(* CONSTANTS *)
let empty    = mk_constant Empty    None         None     
let anything = mk_constant Anything None         (Some "")
let epsilon  = mk_constant Epsilon  (Some empty) (Some "")

let force vo set f x = match vo with 
  | Some v -> v 
  | None -> 
      let v = f x in
      set v; 
      v 

(* GENERIC CONSTRUCTOR *)
let rec mk_t d0 = 
  let t0 = 
    { uid = next_uid ();
      desc = d0;
      hash = desc_hash d0;
      size = desc_size d0;
      final = desc_final d0;      
      simple = desc_simple d0;
      maps = None;
      reverse = None;
      representative = None;
      suffs = None;
      derivative = dummy_impl } in 

  (* derivative *)
  let derivative_impl = 
    let mk_table f = 
      let fr = ref f in
      let diff_cache : t ICache.t = ICache.create 7 in
      (fun c ->
         try ICache.find diff_cache c with Not_found ->
           let r = !fr c in
           ICache.add diff_cache c r;
           r) in
    match d0 with 
      | Anything -> (fun c -> t0)
      | Empty    -> (fun c -> t0)
      | Epsilon  -> (fun c -> empty)
      | CSet s   -> 
          mk_table 
            (fun c -> if CharSet.mem c s then epsilon else empty)
      | Seq(t1,t2) -> 
          mk_table
            (fun c -> 
	       let t12 = mk_seq (t1.derivative c) t2 in 
	       if t1.final then mk_alt t12 (t2.derivative c)
	       else t12)
      | Alt tl -> 
          mk_table
            (fun c -> mk_alts (Safelist.map (fun ti -> ti.derivative c) tl))
      | Rep(t1,0,None) -> 
          mk_table (fun c -> mk_seq (t1.derivative c) t0)
      | Rep(t1,i,None) -> 
          mk_table (fun c -> mk_seq (t1.derivative c) (mk_iter t1 (pred i) None))
      | Rep(t1,0,Some j) -> 
          mk_table (fun c -> mk_seq (t1.derivative c) (mk_iter t1 0 (Some (pred j))))
      | Rep(t1,i,Some j) -> 
          mk_table (fun c -> mk_seq (t1.derivative c) (mk_iter t1 (pred i) (Some (pred j))))
      | Inter tl -> 
          mk_table 
            (fun c -> mk_inters (Safelist.map (fun ti -> ti.derivative c) tl))
      | Diff(t1,t2) -> 
          mk_table 
            (fun c -> mk_diff (t1.derivative c) (t2.derivative c)) in 

  (* backpatch t0 with implementation of derivative *)  
  t0.derivative <- derivative_impl;
  t0

and get_maps t = force t.maps (fun v -> t.maps <- Some v) desc_maps t.desc  

and calc_reverse t = match t.desc with 
  | Anything      -> t
  | Empty         -> t
  | Epsilon       -> t
  | CSet _        -> t
  | Seq(t1,t2)    -> mk_seq (get_reverse t2) (get_reverse t1)
  | Alt tl        -> mk_alts (Safelist.fold_left (fun acc ti -> get_reverse ti::acc) [] tl)
  | Rep(t1,i,jo)  -> mk_iter (get_reverse t1) i jo
  | Inter tl      -> mk_inters (Safelist.fold_left (fun acc ti -> get_reverse ti::acc) [] tl)
  | Diff(t1,t2)   -> mk_diff (get_reverse t1) (get_reverse t2)
and get_reverse t = force t.reverse (fun v -> t.reverse <- Some v) calc_reverse t 

and calc_representative t0 =  
(*   Util.format "CALC_REPRESENTATIVE %d %d (%s)@\n@\n" t0.uid t0.size (tag_of_t t0); *)
  let rec rep_jump f p = match p with 
    | [] -> Misc.Right f 
    | (ti,ri)::rest -> 
        if Q.mem ti f then rep_jump f rest
        else calc_representative ti (ri,f,rest) in
  let add ti wi r f p = 
    match ti.representative with
    | Some (Some t_wi) -> 
        let r' = 
          if wi = "" then (t_wi,ti)::r  
          else (t_wi,epsilon)::(wi,ti)::r in 
        Misc.Left r'
    | Some None -> 
        Misc.Right p
    | None -> 
        let p' = if Q.mem ti f then p else (ti,(wi,ti)::r)::p in
        Misc.Right p' in 
    
  let full_search (r,f,p) =
(*     Util.format "@\nFULL_SEARCH %d@\n" t0.uid; *)
    let f' = Q.add t0 f in
      if t0.final then Misc.Left (("",t0)::r)
      else if Q.mem t0 f || easy_empty t0 then 
        begin 
          if t0.representative = None then t0.representative <- Some None;
          rep_jump f' p
        end
      else
	let _,rm,len = get_maps t0 in
        let rec loop sn acc i = match acc with
          | Misc.Left _ -> acc
          | Misc.Right pacc ->               
              if i < 0 then acc
              else 
                let ci = rm.(i) in
                let w_ci = String.make 1 (Char.chr ci) in 
                let ti = t0.derivative ci in
                  if Q.mem ti sn then loop sn acc (pred i)
                  else loop (Q.add ti sn) (add ti w_ci r f' pacc) (pred i) in 
          match loop Q.empty (Misc.Right p) (pred len) with
            | Misc.Left _ as res -> res
            | Misc.Right p' -> rep_jump f' p' in 

  let rep_of_list l = 
    let buf = Buffer.create 17 in
    Safelist.iter (fun (wi,_) -> Buffer.add_string buf wi) l;
    Buffer.contents buf in 
    
  (* TODO: refactor loop in full_search to use a generalized alt_rep *)
  let rec alts_rep r g f p l = 
    let rec loop acc l = match acc,l with
      | Misc.Left _,_ -> acc
      | _,[]          -> acc
      | Misc.Right pacc,ti::rest -> 
          loop (add (g ti) "" r f pacc) rest in
      match loop (Misc.Right p) l with
        | Misc.Left _ as res -> res
        | Misc.Right p'      -> rep_jump f p' in 

  let go (r,f,p) ti = match ti.representative with
    | Some(Some wi) -> Misc.Left [wi,ti]
    | Some None     -> Misc.Right Q.empty
    | None          -> calc_representative ti (r,f,p) in
    
    match t0.desc with
      | Anything ->          
          (fun (r,f,_) -> Misc.Left(("",t0)::r))
      | Epsilon ->          
          (fun (r,f,_) -> Misc.Left(("",t0)::r))
      | Rep(_,0,_) -> 
          (fun (r,f,_) -> Misc.Left (("",t0)::r))
      | Rep(t1,i,_) -> 
          (fun (r,f,p) -> 
             match go([],f,[]) t1 with                 
               | Misc.Left r1 -> 
                   let w1 = rep_of_list r1 in 
                   (if t1.representative = None then t1.representative <- Some (Some w1));
                   let buf = Buffer.create 17 in 
                   for j=1 to i do Buffer.add_string buf w1 done;
                   Misc.Left [Buffer.contents buf,t0]
               | Misc.Right f' -> 
                   (if t1.representative = None then t1.representative <- Some None);
                   rep_jump (Q.add t0 f) p)
      | Empty | CSet [] -> 
          (fun (r,f,_) -> Misc.Right f)
      | CSet((c1,_)::_) -> 
          (fun (r,f,_) -> Misc.Left ((string_of_char_code c1,t0)::r))
      | Seq(t1,t2) -> 
          (fun (r,f,p) -> 
             match go (r,f,[]) t1 with 
               | Misc.Right f' -> 
                   (if t1.representative = None then t1.representative <- Some None);
                   rep_jump (Q.add t0 f) p
               | Misc.Left r1 -> 
                   let w1 = rep_of_list r1 in 
                   (if t1.representative = None then t1.representative <- Some (Some w1));
                   match go ([],f,[]) t2 with
                   | Misc.Right f' -> 
                       (if t2.representative = None then t2.representative <- Some None);
                       rep_jump (Q.add t0 f) p
                   | Misc.Left r2 ->
                       let w2 = rep_of_list r2 in 
                       (if t2.representative = None then t2.representative <- Some (Some w2));
                       Misc.Left[w1 ^ w2,t0])
      | Alt tl ->
          (fun (r,f,p) -> alts_rep r (fun x -> x) (Q.add t0 f) p tl)
      | Diff(t1,t2) -> 
          (match t1.desc with
             | Alt(tl1) ->                 
                 (fun (r,f,p) -> alts_rep r (fun ti -> mk_diff ti t2) (Q.add t0 f) p tl1)
             | _ -> full_search)
      | Inter (t1::tl2) -> 
          (match t1.desc with
             | Alt tl1 -> 
                 (fun (r,f,p) -> alts_rep r (fun ti -> mk_inters (ti::tl2)) (Q.add t0 f) p tl1)
             | _ -> full_search)
      | _ -> full_search

and get_representative t0 = match t0.representative with
  | Some res -> res 
  | None -> 
      match calc_representative t0 ([],Q.empty,[]) with
        | Misc.Right f' -> 
            Q.iter (fun ti -> ti.representative <- Some None) f';
            t0.representative <- Some None;
            None
        | Misc.Left r -> 
            let w0 = Safelist.fold_left 
              (fun w (wi,ti) -> 
                 let w' = wi ^ w in 
                 (if ti.representative = None then ti.representative <- Some (Some w'));
                 w') 
              "" r in
            t0.representative <- Some (Some w0);
            Some w0

and easy_empty t0 = match t0.representative with
  | Some None -> true
  | _ -> false

and calc_suffs t0 =  
(*   Util.format "CALC_SUFFS %d %d (%s)@\n@\n" t0.uid t0.size (string_of_t t0); *)
  let rec suffs_jump ts f p = match p with 
    | [] -> ts
    | ti::rest -> 
        if Q.mem ti f then suffs_jump ts f rest
        else calc_suffs ti (ts,f,rest) in
  let add ti p = ti::p in
  let full_search (ts,f,p) =
(*     Util.format "@\nFULL_SEARCH %d@\n" t0.uid; *)
    let f' = Q.add t0 f in
    let ts' = if t0.final then Q.add t0 ts else ts in
    if Q.mem t0 f || easy_empty t0 then suffs_jump ts' f' p
    else
      let _,rm,len = get_maps t0 in
      let rec loop sn acc i =              
        if i < 0 then acc
        else 
          let ti = t0.derivative (rm.(i)) in 
          if Q.mem ti sn then loop sn acc (pred i)
          else loop (Q.add ti sn) (add ti acc) (pred i) in 
      let p' = loop Q.empty p (pred len) in
      suffs_jump ts' f' p' in
        
    match t0.desc with
      | Anything ->
          (fun (ts,f,p) -> Q.singleton anything)
      | Epsilon ->          
          (fun (ts,f,p) -> suffs_jump (Q.add epsilon ts) f p)
      | Rep(t1,0,None) ->           
          (fun (ts,f,p) -> 
             let ts' = Q.add t0 ts in
             let f' = Q.add t0 f in 
             let p' = t1::p in 
             suffs_jump ts' f' p')
      | Empty -> 
          (fun (ts,f,p) -> suffs_jump ts f p)
      | CSet((c1,_)::_) -> 
          (fun (ts,f,p) -> suffs_jump (Q.add epsilon ts) f p)
      | Seq(t1,t2) -> 
          if t1.simple then (fun (ts,f,p) -> suffs_jump ts (Q.add t0 f) (t2::p))
          else full_search
      | _ -> full_search

and get_suffs t0 = match t0.suffs with
  | Some res -> res 
  | None -> 
      let ts = calc_suffs t0 (Q.empty,Q.empty,[]) in
      let r = mk_alts (Q.elements ts) in
      t0.suffs <- Some r;
      r

(* and calc_suffs t0 =  *)
(*   let rec full_search ts f p = match p with *)
(*     | [] -> ts *)
(*     | t::rest ->  *)
(*         let f' = Q.add t f in          *)
(*         if Q.mem t f || easy_empty t then full_search ts f rest  *)
(*         else  *)
(*           let _,rm,len = get_maps t in  *)
(*           let rec loop f'' pacc i =  *)
(*             if i < 0 then pacc  *)
(*             else  *)
(*               let ci = rm.(i) in  *)
(*               let ti = t.derivative ci in  *)
(*               if Q.mem ti f'' then loop f'' pacc (pred i) *)
(*               else loop (Q.add ti f'') (ti::pacc) (pred i) in  *)
(*           let p' = loop f' p (pred len) in  *)
(*           let ts' = if t.final then Q.add t ts else ts in  *)
(*           full_search ts' f' p' in *)
(*   let ts = full_search Q.empty Q.empty [t0] in *)
(*   mk_alts (Q.elements ts) *)

(* and get_suffs t0 = match t0.suffs with *)
(*   | Some suffso -> suffso  *)
(*   | None ->  *)
(*       let t0' = calc_suffs t0 in *)
(*       t0.suffs <- Some t0'; *)
(*       t0' *)

and mk_cset cs = match cs with
  | [] -> empty
  | (c1,_)::_ -> 
      let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
      try CSCache.find cset_cache cs' 
      with Not_found -> 
	let res = mk_t (CSet cs') in 
        res.representative <- Some (Some (string_of_char_code c1));
	CSCache.add cset_cache cs' res;
	res

and mk_neg_cset cs = 
  let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
    match CharSet.negate min_code max_code cs' with 
      | [] -> empty
      | (c1,_)::_ as cs'' -> 
	  try CSCache.find neg_cset_cache cs''
	  with Not_found -> 
	    let res = mk_t (CSet cs'') in 
	    CSCache.add neg_cset_cache cs'' res;
            res.representative <- Some (Some (string_of_char_code c1));
	    res

and mk_seq t1 t2 = 
  let rec aux (x,acc) ti = match ti.desc with
    | Seq(ti1,ti2) -> aux (x+883*ti1.hash,(ti1::acc)) ti2 
    | _            ->      
        let p = (ti,(x,acc),t2) in
        begin try TTLTCache.find seq_cache p 
        with Not_found -> 
          let r = Safelist.fold_left (fun acc ti -> mk_t(Seq(ti,acc))) t2 (ti::acc) in 
          TTLTCache.add seq_cache p r;
          r 
        end in
  let res = match t1.desc,t2.desc with
    | Epsilon,_       -> t2
    | _,Epsilon       -> t1
    | _               -> 
        if easy_empty t1 || easy_empty t2 then empty 
        else if t1.uid = t2.uid then mk_iter t1 2 (Some 2)
        else aux (0,[]) t1 in
    res

and mk_seqs tl = Safelist.fold_left mk_seq epsilon tl

and mk_alt t1 t2 =
  let rec go (x,acc) l = match acc,l with
    | [],[]        -> empty
    | [t1],[]      -> t1
    | _,[]         ->
        let p = (x,acc) in
        begin
          try TLCache.find alt_cache p
          with Not_found ->
            let r = mk_t(Alt acc) in
            TLCache.add alt_cache p r;
            r
        end
    | _,(t1::rest) ->
        if easy_empty t1 then go (x,acc) rest
        else if t1.desc = Anything then anything
        else go (883 * t1.hash,t1::acc) rest in
  let rec merge acc l1 l2 = match l1,l2 with
    | [],[]           -> go (0,[]) acc
    | t1::l1',[]      -> merge (t1::acc) l1' []
    | [],t2::l2'      -> merge (t2::acc) [] l2'
    | t1::l1',t2::l2' ->
        let c = compare_t t1 t2 in
        if c=0 then merge (t1::acc) l1' l2'
        else if c < 0 then merge (t1::acc) l1' l2
        else merge (t2::acc) l1 l2' in
  let res = match t1.desc,t2.desc with
    | Empty,_         -> t2
    | _,Empty         -> t1
    | Anything,_      -> anything
    | _,Anything      -> anything
    | CSet s1,CSet s2 -> mk_cset (CharSet.union s1 s2)
    | Alt l1,Alt l2   -> merge [] l1 l2
    | Alt l1,_        -> merge [] l1 [t2]
    | _,Alt l2        -> merge [] [t1] l2
    | _               -> merge [] [t1] [t2] in
  res

and mk_alts tl = Safelist.fold_right mk_alt tl empty

and mk_iter t0 i jo =
  let go t i jo =
    let p = (t,i,jo) in
    try TIIOCache.find iter_cache p
    with Not_found ->
      let r = mk_t(Rep(t,i,jo)) in
      TIIOCache.add iter_cache p r;
      r in
  let res = match t0.desc,i,jo with
    | Empty,0,_               -> epsilon
    | Empty,_,_               -> empty
    | Epsilon,_,_             -> epsilon
    | Anything,_,_            -> anything
    | Rep(_,0,None),0,None    -> t0
    | CSet[mi,ma],0,None      ->
	if mi=min_code && ma=max_code then anything
	else go t0 0 None
    | _,0,Some 0              -> epsilon
    | _,1,Some 1              -> t0
    | _,0,Some 1              -> mk_alt t0 epsilon
    | Rep(t1,i1,Some j1),i,Some j when i1=j1 && i=j ->
        go t1 (i1*i) (Some (j1*j))
    | _                    -> go t0 i jo in
  res
    
and mk_inter t1 t2 =
  let rec go (x,acc) l = 
    match acc,l with
    | [],[]        -> anything
    | [t1],[]      -> t1
    | _,[]         ->
        let p = (x,acc) in
        begin try TLCache.find inter_cache p
        with Not_found ->
          let r = mk_t(Inter acc) in
            TLCache.add inter_cache p r;
            r
        end
    | _,(t1::rest) ->
        if easy_empty t1 then empty
        else if t1.desc = Anything then go (x,acc) rest
        else go (883 * t1.hash,t1::acc) rest in
  let rec merge acc l1 l2 = match l1,l2 with
    | [],[]           -> go (0,[]) acc
    | t1::l1',[]      -> merge (t1::acc) l1' []
    | [],t2::l2'      -> merge (t2::acc) [] l2'
    | t1::l1',t2::l2' ->
        let c = compare_t t1 t2 in
        if c=0 then merge (t1::acc) l1' l2'
        else if c < 0 then merge (t1::acc) l1' l2
        else merge (t2::acc) l1 l2' in
  let res = match t1.desc,t2.desc with
    | Empty,_           -> empty
    | _,Empty           -> empty
    | Anything,_        -> t2
    | _,Anything        -> t1
    | CSet s1,CSet s2   -> mk_cset (CharSet.inter s1 s2)
    | Inter l1,Inter l2 -> merge [] l1 l2
    | Inter l1,_        -> merge [] l1 [t2]
    | _,Inter l2        -> merge [] [t1] l2
    | _                 -> merge [] [t1] [t2] in
  res

and mk_inters tl = Safelist.fold_left mk_inter anything tl

and mk_diff t1 t2 =
  let go t1 t2 =
    let p = (t1,t2) in
    try TTCache.find diff_cache p
    with Not_found ->
      let r = mk_t(Diff(t1,t2)) in
      TTCache.add diff_cache p r;
      r in
  let res =
    if t1.uid = t2.uid || easy_empty t1 then empty
    else if easy_empty t2 then t1
    else match t1.desc,t2.desc with
      | _,Anything           -> empty
      | CSet s1, CSet s2     -> mk_cset (CharSet.diff s1 s2)
      | CSet _,Epsilon       -> t1
      | Seq(t11,t12),Epsilon ->
          if t1.final then mk_seq (mk_diff t11 t2) t1 else t1
      | Rep(t11,0,j),Epsilon -> mk_seq (mk_diff t11 t2) t1
      | Epsilon,_            -> if t2.final then empty else epsilon
      | Diff(t11,t12),_      -> go t11 (mk_alt t12 t2)
      | _                    -> go t1 t2 in
  res

(* -------------------- OPERATIONS -------------------- *)
let mk_complement t0 = mk_diff anything t0

let mk_reverse t0 = get_reverse t0

let mk_star s1 = mk_iter s1 0 None

let mk_iter s1 i j = mk_iter s1 i (if j > 0 then Some j else None)

let representative t0 = get_representative t0 

let is_empty t0 = representative t0 = None

let is_final t0 = t0.final

let suffs t0 = get_suffs t0 

let derivative t w = 
  let n = String.length w in 
  let rec loop i acc = 
    if i >= n then acc
    else loop (succ i) (acc.derivative (Char.code w.[i])) in 
  loop 0 t

let reverse_string w = 
  let n = String.length w in 
  let buf = Buffer.create n in 
  for i=1 to n do
    Buffer.add_char buf w.[n-i]
  done;
  Buffer.contents buf

let splittable_cex t1 t2 = 
  let t1_suffs = suffs t1 in
  let t2_prefs = mk_reverse (suffs (mk_reverse t2)) in
  let overlap_or_epsilon = mk_inter t1_suffs t2_prefs in
  let overlap = mk_diff overlap_or_epsilon epsilon in 
  match representative overlap with
    | Some over -> 
        let t2_suff = derivative t2 over in 
        let t1_pref = mk_reverse (derivative (mk_reverse t1) (reverse_string over)) in
        begin match representative t1_pref,representative t2_suff with
          | Some w1,Some w2 -> Some(w1,over,w2)
          | _ -> 
              Berror.run_error (Info.M "splittable_cex")
                (fun () -> Util.format "error computing representative from %s@\nT1=%s@\nT1_SUFFS=%s@\nT2=%s@\nT2_PEFS=%s@\nOVERLAP_OR_EPSILON=%s@\n" over 
                   (string_of_t t1) (string_of_t t1_suffs) 
                   (string_of_t t2) (string_of_t t2_prefs)
                   (string_of_t overlap_or_epsilon))
        end
    | None -> None

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

