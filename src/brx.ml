(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007-2008                                                    *)
(* J. Nathan Foster and Benjamin C. Pierce                                    *)
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
(* /boomerang/src/brx.ml                                                      *)
(* Boomerang RegExp engine                                                    *)
(* Uses code from Jerome Vouillon's Rx module in Unison.                      *)
(* $Id$ *)
(******************************************************************************)

module H = Hashtbl
let msg = Util.format

(* --------------------- CONSTANTS --------------------- *)
(* ASCII alphabet *)
let min_code = 0
let max_code = 255

(* --------------------- PRETTY PRINTING --------------------- *)
(* ranks: used in formatting to decide when parentheses are needed. *)
type r = 
  | Urnk (* union *)
  | Drnk (* diff *)
  | Irnk (* inter *)
  | Crnk (* concat *)
  | Srnk (* star *)
  | Arnk (* atomic *)

(* lpar: true if an expression with rank on the left needs parantheses *)
let lpar r1 r2 = match r1,r2 with
  | Arnk, _    -> false
  | _, Arnk    -> false
  | Srnk,Srnk  -> true
  | Srnk, _    -> false
  | _, Srnk    -> true
  | Crnk, _    -> false
  | _, Crnk    -> true
  | Irnk, _    -> false
  | _, Irnk    -> true
  | Urnk, Drnk -> true
  | Drnk, Urnk -> true
  | Drnk, Drnk -> false
  | Urnk, Urnk -> false
      
(* lpar: true if an expression with rank on the right needs parantheses *)
let rpar r1 r2 = match r1,r2 with
  | Arnk, _    -> false
  | _, Arnk    -> false
  | Srnk,Srnk  -> true
  | Srnk, _    -> false
  | _, Srnk    -> true
  | Crnk, _    -> false
  | _, Crnk    -> true
  | Irnk, _    -> false
  | _, Irnk    -> true
  | Urnk, Drnk -> true
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
  val equal : t -> t -> bool
end = struct
  (* representated as lists of pairs of ints *) 
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
        else if c2 < d2 then 
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

  let rec equal l1 l2 = l1=l2
end

(* --------------------- REGULAR EXPRESSIONS --------------------- *)
(* regexp descriptions *)
type d = 
  | CSet of CharSet.t
  | Seq of t * t
  | Alt of t list
  | Rep of t * int * int option
  | Inter of t list
  | Diff of t * t

(* full regexps: includes metadata (uid, hash) and several
   cached/memoized fields/operations (final, derivative, reverse,
   representative,etc.) *)
and t = 
    { uid                        : int;
      desc                       : d;
      hash                       : int;
      final                      : bool;
      size                       : int; 
      mutable maps               : (int array * int) option;
      mutable known_singleton    : bool;
      mutable derivative         : int -> t;
      mutable reverse            : t option;
      mutable representative     : (int list option) option;
      mutable suffs              : t option; }

(* alias for t; useful when instantiating a module type also named t *)
type this_t = t 

(* comparison on ts *)
let compare_t t1 t2 = compare t1.uid t2.uid 

let equal_t t1 t2 = t1.uid = t2.uid

let rec equal_ts tl1 tl2 = match tl1,tl2 with
  | [],[]               -> true
  | t1::rest1,t2::rest2 -> t1.uid = t2.uid && equal_ts rest1 rest2 
  | _                   -> false

(* sets of ts *)
module Q = Set.Make(
  struct
    type t = this_t
    let compare t1 t2 = compare_t t1 t2
  end)

module QQ = Set.Make(
  struct
    type t = this_t * this_t
    let compare (t11,t12) (t21,t22) = 
      let c1 = compare_t t11 t21 in 
        if c1 <> 0 then c1 
        else compare_t t12 t22  
  end)
      
(* --------------------- PRETTY PRINTING --------------------- *)
(* rank of a regexp *)
let rank t0 = match t0.desc with
  | CSet _   -> Arnk
  | Rep _    -> Srnk
  | Seq _    -> Crnk
  | Alt _    -> Urnk 
  | Inter _  -> Irnk
  | Diff _   -> Drnk 

(* printing helpers *)
let string_of_char_code n = String.make 1 (Char.chr n)

let string_of_cset_code n = match n with 
  | 9 -> "\\t"
  | 10 -> "\\n"
  | 45 -> "\\-"
  | 92 -> "\\"
  | 93 -> "\\]"
  | 94 -> "\\^"
  | n when n >= 32 && n <= 126 -> String.make 1 (Char.chr n) 
  | _ -> 
      "\\" ^ 
      if n < 100 then "0" else if n < 10 then "00" else "" ^ 
      string_of_int n

let string_of_cset_code_pair (n1,n2) = 
  if n1=n2 then string_of_cset_code n1 
  else 
    string_of_cset_code n1 ^ 
    "-" ^
    string_of_cset_code n2

let wrap l r f = 
  Util.format "%s" l; 
  f (); 
  Util.format "%s" r  

(* format a regexp *)
let rec format_t t0 = 
  let maybe_wrap = Bprint.maybe_wrap format_t in
  let rec format_list sep rnk l = match l with
    | []       -> ()
    | [ti]     -> maybe_wrap (rpar (rank ti) rnk) ti
    | ti::rest -> 
        maybe_wrap (lpar (rank ti) rnk || rpar (rank ti) rnk) ti;
        msg "%s" sep;
        msg "@,";
        format_list sep rnk rest in
    msg "@[";
    begin match t0.desc with
      | CSet [p1] -> 
          let n1,n2 = p1 in 
          if n1=min_code && n2=max_code then msg "[^]"
          else if n1=n2 then wrap "\"" "\"" (fun () -> msg "%s" (string_of_cset_code n1))
          else wrap "[" "]" (fun () -> msg "%s" (string_of_cset_code_pair p1))
      | CSet cs -> 
          let ns = CharSet.negate min_code max_code cs in
          let p,l = if Safelist.length ns < Safelist.length cs then ("^",ns) else ("",cs) in
            wrap ("[" ^ p) "]" (fun () -> Misc.format_list "" (fun pi -> msg "%s" (string_of_cset_code_pair pi)) l)
      | Rep(ti,i,jo) -> 
          let format_rep i jo = match i,jo with
            | 0,None   -> msg "*"
            | 1,None   -> msg "+"
            | i,None   -> msg "{%d,}" i
            | 1,Some 1 -> ()
            | 0,Some 1 -> msg "?"
            | i,Some j -> 
                if i=j then msg "{%d}" i 
                else msg "{%d,%d}" i j in 
          maybe_wrap (lpar (rank ti) Srnk) ti;
          format_rep i jo;
      | Seq(t1,t2)  -> 
          let rec get_str t = match t.desc with
            | CSet[n1,n2] when n1 = n2 -> 
                (Some (string_of_char_code n1), [])
            | Seq(t1,t2) -> 
                begin match get_str t1 with
                  | Some w1,[] -> 
                      begin match get_str t2 with 
                        | Some w2,l2 -> Some(w1 ^ w2),l2
                        | None,l2    -> Some(w1),l2
                      end
                  | Some w1,l1       -> Some(w1),l1@[t2]
                  | None,l1          -> None,l1@[t2]
                end
            | _ -> None,[t] in 
          begin match get_str t0 with
            | Some w1,[] -> wrap "\"" "\"" (fun () -> msg "%s" w1)
            | Some w1,(ti::_ as l) -> 
                wrap "\"" "\"" (fun () -> msg "%s" w1);
                msg "%s" ".";
                format_list "." Crnk l 
            | _ -> format_list "." Crnk [t1;t2] 
          end
      | Alt ts      -> format_list "|" Urnk ts
      | Inter ts    -> format_list "&" Urnk ts
      | Diff(t1,t2) -> format_list "-" Drnk [t1;t2]
    end;
    msg "@]"

let string_of_t t0 = 
  Util.format_to_string 
    (fun () -> 
       Util.format "@["; 
       format_t t0; 
       Util.format "@]")

(* --------------------- STRING MATCHING --------------------- *)
let match_string t0 w = 
  let n = String.length w in 
  let rec loop i ti =     
    if i = n then ti.final
    else loop (succ i) (ti.derivative (Char.code w.[i])) in 
    loop 0 t0

let match_code_list t0 l = 
  let t' = Safelist.fold_left (fun ti ci -> ti.derivative ci) t0 l in 
  t'.final
    
(* --------------------- HASH CONS CACHES --------------------- *)
module ICache = H.Make
  (struct 
     type t = int
     let hash x = Hashtbl.hash x
     let equal (x:int) (y:int) = x=y
   end)

module CSCache = H.Make
  (struct 
     type t = CharSet.t
     let hash cs = Hashtbl.hash cs
     let equal cs1 cs2 = CharSet.equal cs1 cs2
   end)

module TIIOCache = H.Make
  (struct
     type t = this_t * int * int option
     let hash (t,i,jo) = 197 * t.hash + 137 * i + (match jo with None -> 552556457 | Some j -> j)
     let equal (t1,i1,jo1) (t2,i2,jo2) = t1.uid = t2.uid && i1 = i2 && jo1 = jo2
   end)

module TTCache = H.Make
  (struct
     type t = this_t * this_t 
     let hash (t1,t2) = 883 * t1.hash + 859 * t2.hash 
     let equal (t11,t12) (t21,t22) = equal_t t11 t21 && equal_t t12 t22
   end)

module TTLTCache = H.Make
  (struct
     type t = this_t * (int * this_t list) * this_t
     let hash (t1,(x,_),t2) = 883 * t1.hash + x + 883 * t2.hash
     let equal (t11,(_,tl12),t13) (t21,(_,tl22),t23) = 
       t11.uid = t21.uid && t13.uid = t23.uid && equal_ts tl12 tl22
   end)

module TLCache = H.Make
  (struct
     type t = (int * this_t list)
     let hash (x,_) = x
     let equal (_,l1) (_,l2) = equal_ts l1 l2
   end)

let cset_cache : t CSCache.t = CSCache.create 1031
let neg_cset_cache : t CSCache.t = CSCache.create 1031
let seq_cache : t TTLTCache.t = TTLTCache.create 1031
let alt_cache : t TLCache.t = TLCache.create 1031
let rep_cache : t TIIOCache.t = TIIOCache.create 1031
let inter_cache : t TLCache.t = TLCache.create 1031
let diff_cache : t TTCache.t = TTCache.create 1031
let over_cache : t TTCache.t = TTCache.create 1031

(* --------------------- DESC OPERATIONS --------------------- *)
let desc_hash d = 
  let pre_h = match d with
  | CSet(cs)     -> 
      let rec aux = function
        | [] -> 0
        | (i,j)::r -> i + 13 * j + 257 * aux r in 
      aux cs land 0x3FFFFFFF
  | Alt tl           -> 
      199 * Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 tl
  | Seq(t1,t2)       -> 
      821 * t1.hash + 919 * t2.hash
  | Inter tl         -> 
      71 * Safelist.fold_left (fun h ti -> h + 883 * ti.hash) 0 tl
  | Diff(t1,t2)      -> 379 * t1.hash + 563 * t2.hash
  | Rep(t1,i,Some j) -> 197 * t1.hash + 137 * i + j
  | Rep(t1,i,None)   -> 197 * t1.hash + 137 * i + 552556457 in 
  abs pre_h

let desc_final = function
  | CSet _      -> false
  | Rep(t1,0,_) -> true
  | Rep(t1,_,_) -> t1.final
  | Seq(t1,t2)  -> t1.final && t2.final
  | Alt tl      -> Safelist.exists (fun ti -> ti.final) tl
  | Inter tl    -> Safelist.for_all (fun ti -> ti.final) tl
  | Diff(t1,t2) -> t1.final && not t2.final

let desc_size = function
  | CSet _      -> 1
  | Rep(t1,_,_) -> 1 + t1.size
  | Seq(t1,t2)  -> 1 + t1.size + t2.size 
  | Alt tl      -> 1 + Safelist.fold_left (fun acc ti -> acc + ti.size) 0 tl
  | Inter tl    -> 1 + Safelist.fold_left (fun acc ti -> acc + ti.size) 0 tl
  | Diff(t1,t2) -> 1 + t1.size + t2.size 

let desc_maps d0 = 
  let rec split m cs = match cs with
    | [] -> ()
    | (c1,c2)::rest ->
        m.(c1) <- true;
        m.(succ c2) <- true;
        split m rest in
  let rec desc_colorize m d = match d with
    | CSet cs      -> split m cs
    | Rep(t1,_,_)  -> colorize m t1
    | Seq(t1,t2)   -> colorize m t1; if t1.final then colorize m t2
    | Alt tl       -> Safelist.iter (fun ti -> colorize m ti) tl
    | Inter tl     -> Safelist.iter (fun ti -> colorize m ti) tl
    | Diff(t1,t2)  -> colorize m t1; colorize m t2 
  and colorize m t = desc_colorize m t.desc in
  let flatten m = 
    let rec loop i nc rml = 
      if i > max_code then Safelist.rev rml
      else
        let nc' = if m.(i) then succ nc else nc in 
        let rml' = if m.(i) then i::rml else rml in 
        loop (succ i) nc' rml' in 
    let rml = loop 1 0 [0] in
    let rm = Array.of_list rml in 
    let len = Array.length rm in 
    (rm,len) in
  let m = Array.make (succ (succ max_code)) false in 
  desc_colorize m d0;
  flatten m

(* --------------------- CONSTRUCTORS --------------------- *)
(* gensym for uids *)
let uid_counter = ref 0 
let next_uid () = 
  incr uid_counter;
  !uid_counter

let dummy_impl _ = assert false  

(* helper for constructing some constants (anything, empty, epsilon,
   etc.) that are used in the big, mutually-recursive definition of
   mk_t that follows. *)
let mk_constant d t_nexto repo = 
  let t = 
    { uid = next_uid ();
      desc = d;
      hash = desc_hash d;
      final = desc_final d;
      size = desc_size d;
      known_singleton = false;
      maps = None;
      derivative = dummy_impl; 
      reverse = None;
      representative = Some repo;
      suffs = None;
    } in 
  let t_next = match t_nexto with 
    | None -> t 
    | Some t' -> t' in 
  (* backpatch *)
  t.derivative <- (fun c -> t_next);  
  t.reverse <- Some t;
  t

let empty = mk_constant (CSet []) None None  
    
let epsilon = 
  let t = mk_constant (Rep(empty,0,None)) (Some empty) (Some []) in 
  t.known_singleton <- true;
  t

let mk_cset_constant cs = 
  let repo = match cs with [] -> None | (c1,_)::_ -> Some [c1] in 
  let t = mk_constant (CSet cs) None repo in 
  t.derivative <- (fun c -> if CharSet.mem c cs then epsilon else empty);
  t.known_singleton <- true;
  t

let anychar  = 
  let t = mk_constant (CSet [min_code,max_code]) None (Some [min_code]) in
  t.derivative <- (fun c -> epsilon);
  t 

let anything = 
  let t = mk_constant (Rep(anychar,0,None)) None (Some []) in
  t.derivative <- (fun c -> if c > max_code then empty else t);
  t
 
let is_empty t = t.uid = empty.uid
let is_epsilon t = t.uid = epsilon.uid
let is_anything t = t.uid = anything.uid

(* helpers for forcing / installing thunk-ified operations *)
let force vo set f x = match vo with 
  | Some v -> v
  | None -> 
      let v = f x in
      set v; 
      v 

let install upd f = 
  (fun args -> 
     let v = f args in 
     upd (fun _ -> v); 
     v)

(* main constructor *)
let rec mk_t d0 = 
  let t0 = 
    { uid = next_uid ();
      desc = d0;
      hash = desc_hash d0;
      final = desc_final d0;      
      size = desc_size d0;
      known_singleton = false;
      maps = None;
      representative = None;
      suffs = None;
      reverse = None;
      derivative = dummy_impl } in 

  let derivative_impl = 
    let mk_table f = 
      let fr = ref f in
      let der_cache : t ICache.t = ICache.create 7 in
        (fun c ->
           try ICache.find der_cache c with Not_found ->
             let r = !fr c in
             ICache.add der_cache c r;
             r) in
    let res = match d0 with 
      | CSet []  -> 
          (fun c -> empty)
      | CSet [c1,c2]  -> 
          (fun c -> if c1 <= c && c <= c2 then epsilon else empty)
      | CSet s   ->           
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
      | Rep(t1,i,jo) -> 
          mk_table 
            (fun c -> 
               mk_seq 
                 (t1.derivative c) 
                 (mk_rep t1 
                    (max 0 (pred i)) 
                    (match jo with 
                       | None   -> None
                       | Some j -> Some (pred j))))
      | Inter tl -> 
          mk_table 
            (fun c -> mk_inters (Safelist.map (fun ti -> ti.derivative c) tl))
      | Diff(t1,t2) -> 
          mk_table 
            (fun c -> mk_diff (t1.derivative c) (t2.derivative c)) in 
    res in

  (* backpatch t0 with implementation of derivative *)  
  t0.derivative <- derivative_impl;
  t0

(* regexp operations *)
and get_maps t = 
  force t.maps 
    (fun v -> t.maps <- Some v) 
    desc_maps t.desc  

and calc_reverse t = match t.desc with 
  | CSet _        -> t
  | Seq(t1,t2)    -> mk_seq (get_reverse t2) (get_reverse t1)
  | Alt tl        -> mk_alts (Safelist.map get_reverse tl)
  | Rep(t1,i,jo)  -> mk_rep (get_reverse t1) i jo
  | Inter tl      -> mk_inters (Safelist.map get_reverse tl)
  | Diff(t1,t2)   -> mk_diff (get_reverse t1) (get_reverse t2)

and get_reverse t = 
  force t.reverse 
    (fun v -> t.reverse <- Some v) 
    calc_reverse t 

and calc_suffs t =  
  let rec full_search (ts,f,p) = match p with
    | [] -> Q.fold mk_alt ts empty
    | t::rest ->
        let rm,len = get_maps t in 
        let rec loop ((ts',f',p') as acc) i =
          if i < 0 then acc
          else
            let ti = t.derivative rm.(i) in
            if easy_empty ti || Q.mem ti f' then loop acc (pred i)
            else loop ((if ti.final then Q.add ti ts' else ts'),Q.add ti f',ti::p') (pred i) in 
          full_search (loop (ts,f,rest) (pred len)) in
  let go t = 
    let ts0 = if t.final then Q.singleton t else Q.empty in 
    let f0 = Q.singleton t in 
    full_search (ts0,f0,[t]) in
  match t.desc with
    | CSet _ -> epsilon
    | Seq(t1,t2) -> if not t1.final then calc_suffs t2 else go t
    | Rep(t1,0,None) -> if not t1.final then mk_alt t (calc_suffs t1) else go t
    |_ -> go t

and get_suffs t = 
  force t.suffs
    (fun v -> t.suffs <- Some v)
    calc_suffs t 

and calc_representative t = 
  let rec jump st = match st with
    | Misc.Left _ as res -> res
    | Misc.Right (f,q)  ->
        if Queue.is_empty q then Misc.Right f
        else 
          let (ti,ri,fi) = Queue.pop q in
          if Q.mem ti f then jump (Misc.Right(f,q))
          else calc_representative ti (ri,f,q) in
  let add ti co (r,f,q) = match ti.representative with
    | Some (Some wi) -> 
        let r' = match co with
          | None -> (wi,ti)::r  
          | Some c -> (wi,epsilon)::([c],ti)::r in 
        Misc.Left(r')
    | Some None -> 
        Misc.Right(f,q)
    | None -> 
        let r' = match co with 
          | None -> r 
          | Some c -> ([c],ti)::r in
        if not (Q.mem ti f) then Queue.push (ti,r',f) q;
        Misc.Right(f,q) in
   
  let full_search (r,f,q) =
    let f' = Q.add t f in
    if t.final then Misc.Left (([],t)::r)
    else if easy_empty t || Q.mem t f then jump (Misc.Right(f',q))
    else
      let rm,len = get_maps t in 
      let rec loop i sn acc = match i,acc with
        | -1,_                   -> acc
        | _,Misc.Left _          -> acc
        | _,Misc.Right(fi,qi) -> 
            let ci = rm.(i) in 
            let ti = t.derivative ci in 
            let acc' = 
              if easy_empty ti || Q.mem ti sn then acc 
              else add ti (Some ci) (r,f',qi) in
            loop (pred i) (Q.add ti sn) acc' in 
      let st' = loop (pred len) Q.empty (Misc.Right(f',q)) in 
      jump st' in 

  let rep_of_list l = 
    Safelist.fold_left (fun acc (wi,_) -> acc @ wi) [] l in
    
  let rec alts_rep g r f l st = 
    let rec loop acc l = match acc,l with
      | _,[]                          -> acc
      | Misc.Left _,_                 -> acc
      | Misc.Right(fi,qi),ti::rest -> 
        loop (add (g ti) None (r,f,qi)) rest in 
    let st' = loop (Misc.Right st) l in 
    jump st' in 

  let go f ti = match ti.representative with
    | Some(Some wi) -> Some [wi,ti]
    | Some None     -> None
    | None          ->
        begin match calc_representative ti ([],f,Queue.create ()) with
          | Misc.Left(r) -> Some r
          | _ -> None
        end in
  match t.desc with
    | CSet [] ->
        (fun (r,f,_) -> Misc.Right f)
    | CSet((c1,_)::_) ->
        (fun (r,f,_) -> Misc.Left (([c1],t)::r))
    | Rep(_,0,_) ->
        (fun (r,f,_) -> Misc.Left (([],t)::r))
    | Rep(t1,i,_) ->
        (fun (r,f,q) ->
           match go f t1 with
             | Some r1 ->
                 let w1 = rep_of_list r1 in
                 set_representative t1 w1;
                 let rec loop i acc = if i <= 0 then acc else loop (pred i) (acc @ w1) in
                 Misc.Left ((loop i [],t)::r)
             | _ ->
                 jump(Misc.Right(Q.add t f,q)))
    | Seq(t1,t2) ->
        (fun (r,f,q) ->
           match go f t1 with
             | Some r1 ->
                 let w1 = rep_of_list r1 in
                 set_representative t1 w1;
                 begin
                   match go f t2 with
                     | Some r2 ->
                         let w2 = rep_of_list r2 in
                         set_representative t2 w2;
                         Misc.Left ((w1 @ w2,t)::r)
                     | _ ->
                         jump(Misc.Right(Q.add t f,q))
                 end
             | _ -> jump(Misc.Right(Q.add t f,q)))

    | Alt tl ->
        (fun (r,f,q) ->
           let f' = Q.add t f in
           alts_rep (fun x -> x) r f' tl (f',q))
    | Diff(t1,t2) ->
        begin match t1.representative with
          | Some (Some w1) when not (match_code_list t2 w1) ->
              (fun (r,_,_) -> Misc.Left((w1,t)::r))
          | _ -> match t1.desc with
              | Alt(tl1) ->
                  (fun (r,f,q) ->
                     let f' = Q.add t f in
                     alts_rep (fun ti -> mk_diff ti t2) r f' tl1 (f',q))
              | _ -> full_search
        end
    | Inter [t1;t2] ->
        begin match t1.representative,t2.representative with
          | Some (Some w1),_ when match_code_list t2 w1 ->
              (fun (r,_,_) -> Misc.Left((w1,t)::r))
          | _,Some (Some w2) when match_code_list t1 w2 ->
              (fun (r,_,_) -> Misc.Left((w2,t)::r))
          | _ -> match t1.desc with
              | Alt tl1 ->
                  (fun (r,f,q) ->
                     let f' = Q.add t f in
                     alts_rep (fun ti -> mk_inter ti t2) r f' tl1 (f',q))
              | _ -> full_search
        end
    | Inter(t1::tl2) ->
        begin match t1.desc with
          | Alt tl1 ->
              (fun (r,f,q) ->
                 let f' = Q.add t f in
                 alts_rep (fun ti -> mk_inters (ti::tl2)) r f' tl1 (f',q))
          | _ -> full_search
        end
    | _ -> full_search

and get_representative t = match t.representative with
  | Some res -> res 
  | None -> 
      match calc_representative t ([],Q.empty,Queue.create ()) with
        | Misc.Right f' -> 
            Q.iter set_empty (Q.add t f');
            None
        | Misc.Left r -> 
            let w0 = 
              Safelist.fold_left 
                (fun w (wi,ti) -> 
                   let w' = wi @ w in 
                   set_representative ti w';
                   w') 
                [] r in
            set_representative t w0;
            Some w0

and set_representative t w = 
  if t.representative = None then t.representative <- Some (Some w)

and easy_empty t = match t.representative with
  | Some None -> true
  | _ -> false

and set_empty t =
  t.representative <- Some None

(* constructors *)
and mk_cset cs = 
  let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
  match cs' with 
    | [] -> empty
    | (c1,c2)::rest -> 
        try CSCache.find cset_cache cs'
        with Not_found -> 
          let res_t = mk_t (CSet cs') in 
          res_t.representative <- Some (Some [c1]);
          if c1=c2 && rest = [] then res_t.known_singleton <- true;
          CSCache.add cset_cache cs' res_t;
          res_t
            
and mk_neg_cset cs = 
  let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
  let cs'' = CharSet.negate min_code max_code cs' in
  match cs'' with 
    | [] -> empty
    | _ -> 
        try CSCache.find neg_cset_cache cs'' 
        with Not_found -> 
          let res_t = mk_t (CSet cs'') in 
          CSCache.add neg_cset_cache cs'' res_t;
          res_t

and mk_seq t1 t2 = 
  let rec aux (x,acc) ti = match ti.desc with
    | Seq(ti1,ti2) -> aux (x + 883 * ti1.hash, (ti1::acc)) ti2
    | _ ->
        let p = (ti,(x,acc),t2) in
        begin try TTLTCache.find seq_cache p 
        with Not_found -> 
          let res = Safelist.fold_left 
            (fun acc ti -> 
               let res_ti = mk_t (Seq(ti,acc)) in 
               res_ti.known_singleton <- 
                 ti.known_singleton && acc.known_singleton;
               (match ti.representative,acc.representative with
                  | Some (Some l1), Some (Some l2) -> 
                      res_ti.representative <- Some (Some (l1 @ l2))
                  | _ -> ());
               res_ti)
            t2 (ti::acc) in
            TTLTCache.add seq_cache p res;
          res
        end in
  let res = 
    if is_epsilon t1 then t2 
    else if is_epsilon t2 then t1
    else if easy_empty t1 || easy_empty t2 then empty 
    else if t1.uid = t2.uid then mk_rep t1 2 (Some 2)
    else match t1.desc,t2.desc with 
      | Rep(t11,i,jo),_ when t11.uid = t2.uid -> 
          mk_rep t11 (succ i) 
            (match jo with 
               | None -> None 
               | Some j -> Some (succ j))
      | _,Rep(t21,i,jo) when t1.uid = t21.uid -> 
          mk_rep t21 (succ i) 
            (match jo with 
               | None -> None 
               | Some j -> Some (succ j))
      | _ -> aux (0,[]) t1 in 
    res

and mk_seqs tl = Safelist.fold_left mk_seq epsilon tl

and mk_alt t1 t2 =
  let rec go eps (x,acc) l = match acc,l with
    | [],[]        -> if eps then epsilon else empty
    | [t1],[]      -> if eps && not t1.final then mk_rep t1 0 (Some 1) else t1
    | _,[]         ->
        let p = (x,acc) in 
        begin try TLCache.find alt_cache p 
        with Not_found -> 
          let res_t = mk_t(Alt acc) in 
          TLCache.add alt_cache p res_t;
          res_t
        end
    | _,(t1::rest) ->
        if easy_empty t1 then go eps (x,acc) rest
        else if is_anything t1 then anything
        else go (eps || t1.final) (x + 883 * t1.hash,t1::acc) rest in
    let rec merge acc l1 l2 = match l1,l2 with
      | [],[]           -> go false (0,[]) acc
      | t1::l1',[]      -> merge (t1::acc) l1' []
      | [],t2::l2'      -> merge (t2::acc) [] l2'
      | t1::l1',t2::l2' ->
          let c = compare_t t1 t2 in
            if c=0 then merge (t1::acc) l1' l2'
            else if c < 0 then merge (t1::acc) l1' l2
            else merge (t2::acc) l1 l2' in
    let res = match t1.desc,t2.desc with
        | CSet s1,CSet s2 -> mk_cset (CharSet.union s1 s2)
        | Alt l1,Alt l2   -> merge [] l1 l2
        | Alt l1,_        -> merge [] l1 [t2]
        | _,Alt l2        -> merge [] [t1] l2
        | _               -> 
            if easy_empty t1 then t2
            else if easy_empty t2 then t1
            else if is_anything t1 || is_anything t2 then anything
            else if is_epsilon t1 then mk_rep t2 0 (Some 1)
            else if is_epsilon t2 then mk_rep t1 0 (Some 1)
            else merge [] [t1] [t2] in 
    res
                  
and mk_alts tl = Safelist.fold_right mk_alt tl empty

and mk_rep t0 i jo =
  let go t i jo =
    if easy_empty t then if i=0 then epsilon else empty
    else if is_epsilon t then epsilon
    else if is_anything t then anything
    else 
      let p = (t,i,jo) in 
      try TIIOCache.find rep_cache p 
      with Not_found -> 
        let res_t = mk_t (Rep(t,i,jo)) in 
        TIIOCache.add rep_cache p res_t;
        res_t in
    let res = match t0.desc,i,jo with
      | Rep(_,0,None),_,None       -> t0
      | Rep(_,0,Some 1),0,(Some 1) -> t0
      | CSet[mi,ma],0,None         ->
	  if mi=min_code && ma=max_code then anything
	  else go t0 0 None
      | _,0,Some 0                 -> epsilon
      | _,1,Some 1                 -> t0
      | Rep(t1,i1,Some j1),i,Some j when i1=j1 && i=j ->
          go t1 (i1*i) (Some (j1*j))
      | _                    -> 
          go t0 i jo in
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
            let res_t = mk_t (Inter acc) in 
            TLCache.add inter_cache p res_t;              
            res_t 
          end
      | _,(t1::rest) ->
          if easy_empty t1 then empty
          else if is_anything t1 then go (x,acc) rest
          else go (x + 883 * t1.hash,t1::acc) rest in
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
    | CSet s1,CSet s2   -> mk_cset (CharSet.inter s1 s2)
    | Inter l1,Inter l2 -> merge [] l1 l2
    | Inter l1,_        -> merge [] l1 [t2]
    | _,Inter l2        -> merge [] [t1] l2
    | _                 -> 
        if t1.uid = t2.uid then t1
        else if is_anything t1 then t2 
        else if is_anything t2 then t1
        else if easy_empty t1 || easy_empty t2 then empty
        else if is_epsilon t1 then if t2.final then epsilon else empty
        else if is_epsilon t2 then if t1.final then epsilon else empty
        else merge [] [t1] [t2] in
    res

and mk_inters tl = Safelist.fold_left mk_inter anything tl

and mk_diff t1 t2 =
  let go t1 t2 =
    let p = (t1,t2) in 
    try TTCache.find diff_cache p 
    with Not_found -> 
      let res_t = mk_t(Diff(t1,t2)) in
      TTCache.add diff_cache p res_t;        
      res_t in
  let res = match t1.desc,t2.desc with
    | CSet s1, CSet s2     -> mk_cset (CharSet.diff s1 s2)
    | Diff(t11,t12),_      -> go t11 (mk_alt t12 t2)
    | _,Diff(t21,t22)      -> mk_alt (go t1 t21) (mk_inter t1 t22)
    | Alt l1,_             -> 
        mk_alts (Safelist.map (fun ti -> mk_diff ti t2) l1) 
    | CSet s1,_ when is_epsilon t2 -> t1 
    | _ -> 
        if t1.uid = t2.uid || easy_empty t1 || is_anything t2 then empty
        else if easy_empty t2 then t1
        else if is_epsilon t1 then if t2.final then empty else epsilon
        else if is_epsilon t2 then 
          (match t1.desc with 
             | CSet _ -> 
                 t1
             | Seq(t11,t12) -> 
                 if t1.final then 
                   mk_alt
                     (mk_seq (mk_diff t11 t2) t12) 
                     (mk_seq t11 (mk_diff t12 t2)) 
                 else t1                     
             | Rep(t11,0,None) -> 
                 mk_seq (mk_diff t11 epsilon) t1
             | _ -> go t1 t2)
        else if is_anything t1 then 
          match t2.desc with 
            | Diff(t21,t22) when is_anything t21 -> t22
            | _ -> go t1 t2  
        else go t1 t2 in 
    res
        
(* -------------------- OPERATIONS -------------------- *)

let mk_reverse t0 = get_reverse t0

let reverse_string w = 
  let n = String.length w in 
  let buf = Buffer.create n in 
  for i=1 to n do
    Buffer.add_char buf w.[n-i]
  done;
  Buffer.contents buf

let string_of_char_codes l = 
  let buf = Buffer.create 17 in 
  Safelist.iter (fun ci -> Buffer.add_string buf (string_of_char_code ci)) l;
  Buffer.contents buf

let representative t0 = 
  Misc.map_option string_of_char_codes (get_representative t0)

let rec mk_expand t0 c t = match t0.desc with
  | CSet cs       -> 
      if CharSet.mem c cs then mk_alt t t0 else t0
  | Seq(t1,t2) -> 
      mk_seq (mk_expand t1 c t) (mk_expand t2 c t)
  | Alt tl -> 
      mk_alts (Safelist.map (fun ti -> mk_expand ti c t) tl)
  | Rep(t1,i,jo) -> 
      mk_rep (mk_expand t1 c t) i jo
  | Inter tl -> 
      mk_inters (Safelist.map (fun ti -> mk_expand ti c t) tl)
  | Diff(t1,t2) -> 
      mk_diff (mk_expand t1 c t) (mk_expand t2 c t)

let mk_complement t0 = mk_diff anything t0

let mk_star s1 = mk_rep s1 0 None

let easy_representative t0 = match t0.representative with
  | Some (Some l) -> 
      let w = 
        Safelist.fold_left 
          (fun acc ci -> acc ^ string_of_char_code ci) 
          "" l in 
        Some w
  | _ -> None

let is_empty t0 = representative t0 = None

let is_final t0 = t0.final

let derivative t w = 
  let n = String.length w in 
  let rec loop i acc = 
    if i >= n then acc
    else loop (succ i) (acc.derivative (Char.code w.[i])) in 
    loop 0 t

let mk_suffs t0 = get_suffs t0

let splittable_cex t1 t2 = 
  let overo =
    if t1.size > t2.size then
      let t2_rev_suffs = mk_suffs (mk_reverse t2) in
      if is_epsilon t2_rev_suffs then None
      else
        let t1_suffs = mk_suffs t1 in
          if is_epsilon t1_suffs then None
          else
            representative
              (mk_diff
                 (mk_inter t1_suffs (mk_reverse t2_rev_suffs))
                 epsilon)
    else
      let t1_suffs = mk_suffs t1 in
      if is_epsilon t1_suffs then None
      else
        let t2_rev_suffs = mk_suffs (mk_reverse t2) in
        if is_epsilon t2_rev_suffs then None
        else
          representative
            (mk_diff
               (mk_inter t1_suffs (mk_reverse t2_rev_suffs))
               epsilon) in
  match overo with 
    | Some over ->
        let t2_suff = derivative t2 over in
        let t1_pref = mk_reverse (derivative (mk_reverse t1) (reverse_string over)) in
        begin match representative (mk_inter t1 t1_pref),representative (mk_inter t2 t2_suff) with
          | Some w1,Some w2 -> 
              Misc.Left(w1 ^ over, w2, w1, over ^ w2)
          | _ ->
              Berror.run_error (Info.M "splittable_cex")
                (fun () -> msg "error computing representative")
        end
    | None -> Misc.Right(mk_seq t1 t2)

let iterable_cex t1 = 
  if t1.final then Misc.Left("","","","")
  else
    let t1s = mk_star t1 in 
      match splittable_cex t1 t1s with
        | Misc.Right _ -> Misc.Right t1s
        | res -> res
                  
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

let mk_iter s1 i j = mk_rep s1 i (if j > 0 then Some j else None)

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
  match 
    s1.known_singleton,easy_representative s1,
    s2.known_singleton,easy_representative s2 
  with
    | true,Some w1,true,Some w2 -> if w1 = w2 then Some w1 else None
    | true,Some w1,_,_          -> if match_string s2 w1 then Some w1 else None
    | _,_,true,Some w2          -> if match_string s1 w2 then Some w2 else None
    | _                         -> representative (mk_inter s1 s2)

let disjoint s1 s2 = 
  is_empty (mk_inter s1 s2) 

let equiv s1 s2 = 
  s1.uid = s2.uid || (is_empty (mk_diff s1 s2) && is_empty (mk_diff s2 s1))

let splittable s1 s2 = match splittable_cex s1 s2 with 
  | Misc.Right _ -> true
  | _ -> false

let iterable s0 = match iterable_cex s0 with 
  | Misc.Right _ -> true
  | _ -> false

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

let print_stats () = ()
(*   Util.format "@[NUM:%d YES:%d NO:%d SPLIT:%.2f, YES:%.2f@, NO:%.2f]@\n"  *)
(*     !split_tries *)
(*     !num_split *)
(*     (!split_tries - !num_split) *)
(*     (float_of_int !num_split /. float_of_int !split_tries) *)
(*     (float_of_int !len_split /. float_of_int !num_split) *)
(*     (float_of_int !len_non_split /. float_of_int (!split_tries - !num_split)); *)
(*   let f n s = *)
(*     let len,size,sum,min,avg,max = s () in *)
(*     Util.format "@[%s:\tlen:%s\tsize:%s\tsum:%s\tmin:%s\tavg:%s\tmax:%s@]@\n"  *)
(*       n (pad len) (pad size) (pad sum) (pad min) (pad avg) (pad max) in *)
(*     Trace.debug "brx+" *)
(*     (fun () ->  *)
(*        Util.format "@[Cache statistics:@\n"; *)
(*        f "cset_cache" (fun () -> CSCache.stats cset_cache); *)
(*        f "neg_cset_cache" (fun () -> CSCache.stats neg_cset_cache); *)
(*        f "seq_cache" (fun () -> TTLTCache.stats seq_cache); *)
(*        f "alt_cache" (fun () -> TLCache.stats alt_cache); *)
(*        f "inter_cache" (fun () -> TLCache.stats inter_cache); *)
(*        f "rep_cache" (fun () -> TIIOCache.stats rep_cache); *)
(*        f "diff_cache" (fun () -> TTCache.stats diff_cache); *)
(*        Util.format "@]") *)
