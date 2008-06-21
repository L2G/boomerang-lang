(* ACKs: 
   - Jerome's Unison Rx module.
   - Posting by Mark William Hopkins to comp.compilers usenet forum. 
*)

let dbg thk = Trace.debug "brx+" thk
let sdbg s = Trace.debug "brx+" (fun () -> Util.format "%s" s)

let () = Format.set_margin 120
      
(* constants *)
let min_code = 0
let max_code = 255

(* pretty printing ranks *)
type r = 
  | Urnk           (* union *)
  | Drnk           (* diff *)
  | Irnk           (* inter *)
  | Crnk           (* concat *)
  | Srnk           (* star *)
  | Arnk           (* atomic *)

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

module SMap = Map.Make(
  struct
    type t = string
    let compare (s1:string) s2 = Pervasives.compare s1 s2
  end)
type spine_elt = Bx of string | BxStar of string | St of string
type spine = spine_elt list
type kv = string * string list 
type stump = spine * kv list SMap.t

(* CHARACTER SETS *)
module CharSet : 
sig
  type t = (int * int) list
  val union : t -> t -> t
  val add : int * int -> t -> t
  val inter : t -> t -> t
  val negate : int -> int -> t -> t
  val diff : t -> t -> t
  val mem : int -> t -> bool
end = struct
  type t = (int * int) list
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

(* REGULAR EXPRESSIONS *)
module Rx : sig 
  type t 
  type u = 
    | Eps                 (* epsilon *)
    | CSet of CharSet.t   (* character sets *)
    | Alt of t * t list   (* unions *)
    | Seq of t * t        (* concatenations *)
    | Inter of t * t list (* intersections *)
    | Diff of t * t       (* difference *)
    | Star of t           (* kleene stars *)
  val rank : t -> r

  val hash : t -> int 
  val nillable : t -> bool
  val desc : t -> u
  val format_t : t -> unit
  val string_of_t : t -> string
  val tag_of_t : t -> string
  val anychar : t
  val anything : t
  val empty : t
  val epsilon : t
  val mk_cset : (int * int) list -> t
  val mk_neg_cset : (int * int) list -> t
  val mk_seq : t -> t -> t
  val mk_seqs : t list -> t
  val mk_alt : t -> t -> t 
  val mk_alts : t list -> t
  val mk_inter : t -> t -> t
  val mk_inters : t list -> t
  val mk_inters : t list -> t
  val mk_diff : t -> t -> t
  val mk_star : t -> t 
  val derivative : int -> t -> t
  val reverse : t -> t
  val maps : t -> int array * int array
end = struct
  type u = 
    | Eps                 (* epsilon *)
    | CSet of CharSet.t   (* character sets *)
    | Alt of t * t list   (* unions *)
    | Seq of t * t        (* concatenations *)
    | Inter of t * t list (* intersections *)
    | Diff of t * t       (* difference *)
    | Star of t           (* kleene stars *)

  and t = 
      { desc : u;
        nillable : bool;
        hash : int }

  let rank r = match r.desc with
    | Eps     -> Arnk
    | CSet _  -> Arnk
    | Star _  -> Srnk
    | Seq _   -> Crnk
    | Alt _   -> Urnk 
    | Inter _ -> Irnk
    | Diff _  -> Drnk 

  let mk_hash = function
    | Eps          -> 1229
    | CSet cs      -> 
        let rec aux = function
          | [] -> 0
          | (i,j)::r -> i + 13 * j + 257 * aux r in 
        aux cs land 0x3FFFFFFF
    | Alt (r1,rl)  -> 199 * Safelist.fold_left (fun h y -> h + 883 * y.hash) 0 (r1::rl)
    | Seq(r1,r2)   -> 821 * r1.hash + 919 * r2.hash
    | Inter(r1,rl) -> 71 * Safelist.fold_left (fun h y -> h + 883 * y.hash) 0 (r1::rl)
    | Diff(r1,r2)  -> 379 * r1.hash + 563 * r2.hash
    | Star r1      -> 197 * r1.hash

  let mk_nillable = function
    | Eps          -> true
    | CSet _       -> false
    | Star _       -> true
    | Seq(r1,r2)   -> r1.nillable && r2.nillable
    | Alt(r1,rl)   -> r1.nillable || Safelist.exists (fun ri -> ri.nillable) rl
    | Inter(r1,rl) -> r1.nillable && Safelist.for_all (fun ri -> ri.nillable) rl
    | Diff(r1,r2)  -> r1.nillable && not r2.nillable

  let mk u = 
    { desc = u; 
      nillable = mk_nillable u; 
      hash = mk_hash u }

  let desc r0 = r0.desc

  let hash r0 = r0.hash

  let nillable r0 = r0.nillable

  (* pretty printers *)
  let rec format_t r0 = 
    let string_of_char_code n = Char.escaped (Char.chr n) in 
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
    
    match r0.desc with
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
    | Eps -> 
        Util.format "@[\"\"@]"
    | Seq (r1,r2) -> 
        let rec get_string r = match r.desc with
          | Seq(r1,r2) -> 
              begin match get_string r1 with 
                | Some s1, None -> begin match get_string r2 with
                    | Some s2,None     -> Some (s1^s2),None
                    | Some s2,Some r2' -> Some (s1^s2),Some r2'
                    | None,_           -> Some s1,Some r2
                  end
                | Some s1,Some r1' -> 
                    (Some s1, Some (mk (Seq(r1',r2))))
                | None, _ -> 
                    (None, Some r0)
              end
          | CSet[mi,ma] -> 
              if mi=ma then (Some (String.make 1 (Char.chr mi)), None)
              else (None, Some r0)
          | Eps -> (Some "", None)
          | _ -> (None, Some r0) in 
        (match get_string r0 with 
           | Some s, None -> Util.format "@[\"%s\"@]" (Misc.whack s)
           | Some s, Some r -> Util.format "@[\"%s\"@]" (Misc.whack s); Util.format ".@,"; maybe_wrap (rpar (rank r) Crnk) r
           | None,_ -> 
               Util.format "@[<1>";
               maybe_wrap (lpar (rank r1) Crnk) r1;
               Util.format ".@,";
               maybe_wrap (rpar (rank r2) Crnk) r2;
               Util.format "@]")
    | Alt (r1,[]) -> 
        format_t r1
    | Alt (r1,r2::rest) ->
        Util.format "@[";
        maybe_wrap (lpar (rank r1) Urnk) r1;
        format_list "@,|" Urnk r2 rest;
        Util.format "@]"
    | Star r1 -> 
        Util.format "@[";
        maybe_wrap (lpar (rank r1) Srnk) r1;
        Util.format "*";
        Util.format "@]"
    | Inter(r1,[]) -> 
        format_t r1
    | Inter (r1,r2::rest) ->
        Util.format "@[<1>";
        maybe_wrap (lpar (rank r1) Irnk) r1;
        format_list "@,&" Urnk r2 rest;
        Util.format "@]"
    | Diff(r1,r2) -> 
        Util.format "@[<1>(";
        maybe_wrap (lpar (rank r1) Drnk) r1;
        Util.format "@,-";
        maybe_wrap (lpar (rank r2) Drnk) r2;
        Util.format ")@]"
        
  let tag_of_t r = match r.desc with
    | Eps     -> "Eps"
    | CSet _  -> "CSet"
    | Seq _   -> "Seq"
    | Alt _   -> "Alt"
    | Star _  -> "Star"
    | Inter _ -> "Inter"
    | Diff _  -> "Diff"

  let string_of_t r = 
    Util.format_to_string (fun () -> format_t r)

  (* constructors *)
  let anychar = mk (CSet [min_code,max_code])
  let anything = mk (Star anychar)
  let empty  = mk (CSet [])
  let epsilon  = mk Eps

  let delta r0 = 
    if nillable r0 then epsilon else empty

  let mk_cset cs = 
    let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
    mk (CSet cs')

  let mk_neg_cset cs = 
    let cs' = Safelist.fold_left (fun l p -> CharSet.add p l) [] cs in 
    mk (CSet (CharSet.negate min_code max_code cs')) 

  let mk_seq r1 r2 = 
    let rec aux ri rj = match ri.desc with
      | Seq(ri1,ri2) -> mk (Seq(ri1,aux ri2 rj))
      | _            -> mk (Seq(ri,rj)) in 
    match r1.desc,r2.desc with
    | Eps,_           -> r2
    | _,Eps           -> r1
    | CSet [],_       -> empty
    | _,CSet []       -> empty
    | _               -> aux r1 r2

  let mk_seqs rl = 
    Safelist.fold_left mk_seq epsilon rl 
      
  let mk_alt r1 r2 = 
    let rec merge l1 l2 = match l1,l2 with 
      | _,[] -> l1
      | [],_ -> l2
      | r1::l1',r2::l2' ->           
          let c = compare r1 r2 in 
          if c=0 then r1::merge l1' l2'
          else if c < 0 then r1::merge l1' l2
          else r2::merge l1 l2' in 
    let rec prune acc l = match l with 
      | []     -> Safelist.rev acc
      | r1::rl -> 
          if r1 = anything then [anything]
          else if r1 = empty then prune acc rl
          else prune (r1::acc) rl in 
    let go l = match prune [] l with 
      | [] -> empty
      | [r1] -> r1
      | r1::rl -> mk (Alt(r1,rl)) in 
    let c = compare r1 r2 in 
      if c=0 then r1 
      else 
        (match r1.desc,r2.desc with
           | CSet [],_             -> r2
           | _,CSet []             -> r1
           | CSet s1,CSet s2       -> mk_cset (CharSet.union s1 s2)
           | Alt(r1,l1),Alt(r2,l2) -> go (merge (r1::l1) (r2::l2)) 
           | Alt(r1,l1),_          -> go (merge (r1::l1) [r2])
           | _,Alt(r2,l2)          -> go (merge [r1] (r2::l2))
           | _                     -> go (merge [r1] [r2]))
  let mk_alts rl = 
    Safelist.fold_right mk_alt rl empty

  let mk_star r0 = match r0.desc with 
    | Eps     -> epsilon
    | CSet [] -> epsilon
    | Star _  -> r0
    | _       -> mk (Star r0) 
        
  let mk_inter r1 r2 = 
    let rec merge l1 l2 = match l1,l2 with
      | _,[] -> l1
      | [],_ -> l2
      | r1::l1',r2::l2' -> 
          let c = compare r1 r2 in 
            if c=0 then r1::merge l1' l2'
            else if c < 0 then r1::merge l1' l2
            else r2::merge l1 l2' in 
    let rec prune acc l = match l with 
      | []     -> Safelist.rev acc
      | r1::rl -> 
          if r1 = anything then prune acc rl
          else if r1 = empty then [empty]
          else prune (r1::acc) rl in 
    let go l = match prune [] l with 
      | [] -> anything
      | [r1] -> r1
      | r1::rl -> mk (Inter(r1,rl)) in 
    let c = compare r1 r2 in 
      if c=0 then r1 
      else 
        (match r1.desc,r2.desc with
           | CSet [],_                 -> empty
           | _,CSet []                 -> empty
           | CSet s1,CSet s2           -> mk_cset (CharSet.inter s1 s2)
           | Eps,_                     -> delta r2
           | _,Eps                     -> delta r1
           | Inter(r1,l1),Inter(r2,l2) -> go (merge (r1::l1) (r2::l2))
           | Inter(r1,l1),_            -> go (merge (r1::l1) [r2])
           | _,Inter(r2,l2)            -> go (merge [r1] (r2::l2))
           | _                         -> go (merge [r1] [r2]))

  let mk_inters rl = 
    Safelist.fold_left mk_inter anything rl

  let rec mk_diff r1 r2 = match r1.desc,r2.desc with
    | CSet [],_        -> empty
    | _,CSet []        -> r1
    | CSet s1, CSet s2 -> mk_cset (CharSet.diff s1 s2)
    | Eps,Eps          -> empty
    | CSet _,Eps       -> r1
    | Star r11,Eps     -> mk_seq (mk_diff r11 r2) r1
    | Eps,_            -> if r2.nillable then empty else epsilon
    | Diff(r11,r12),_  -> mk (Diff(r11,mk_alt r12 r2))
    | Inter(r1,rl),_   -> mk_inters (Safelist.map (fun ri -> mk_diff ri r2) (r1::rl))
    | _                -> 
        if r1.hash = r2.hash && r1 = r2 then empty
        else mk (Diff(r1,r2))

  type this_t = t
  module DCache = Hashtbl.Make
    (struct
       type t = int * this_t
       let hash (c,x) = x.hash + 11 * c
       let equal ((c:int),x) (d,y) = c=d && x.hash = y.hash && x = y
     end)
  let dcache = DCache.create 1069

  let rec derivative c r0 = 
    let p = (c,r0) in 
    try DCache.find dcache p with Not_found -> 
      let res = match r0.desc with 
        | Eps -> 
            empty
        | CSet s  -> 
            if CharSet.mem c s then epsilon 
            else empty
        | Seq(r1,r2) ->
            mk_alt 
              (mk_seq (derivative c r1) r2)
              (mk_seq (delta r1) (derivative c r2))
        | Alt(r1,[]) -> 
            derivative c r1
        | Alt (r1,rl) -> 
            mk_alts (Safelist.map (derivative c) (r1::rl))
        | Star r1 -> 
            mk_seq (derivative c r1) r0
        | Inter(r1,[]) -> 
            derivative c r1
        | Inter(r1,rl) ->         
            mk_inters (Safelist.map (derivative c) (r1::rl)) 
        | Diff(r1,r2) -> 
            mk_diff
              (derivative c r1)
              (derivative c r2) in
      DCache.add dcache p res;
      res

  let rec reverse r0 = match r0.desc with
    | Eps          -> r0
    | CSet _       -> r0
    | Seq(r1,r2)   -> mk_seq (reverse r2) (reverse r1)
    | Alt(r1,rl)   -> mk_alts (Safelist.map reverse (r1::rl))
    | Star(r1)     -> mk_star (reverse r1)
    | Inter(r1,rl) -> mk_inters (Safelist.map reverse (r1::rl))
    | Diff(r1,r2)  -> mk_diff (reverse r1) (reverse r2)
  module MapCache = Hashtbl.Make(
    struct
      type t = int list
      let hash = Hashtbl.hash  
      let equal = (=)
    end)
  let mcache : (int array * int array) MapCache.t = 
    MapCache.create 101

  let maps r0 = 
    let rec split m0 cs = match cs with
      | [] -> ()
      | (c1,c2)::rest ->
          m0.(c1) <- true;
          m0.(succ c2) <- true;
          split m0 rest in
    let rec colorize m0 r0 = match r0.desc with
      | Eps          -> ()
      | CSet cs      -> split m0 cs
      | Star r1      -> colorize m0 r1
      | Seq(r1,r2)   -> colorize m0 r1; if r1.nillable then colorize m0 r2
      | Alt(r1,rl)   -> colorize m0 r1; Safelist.iter (colorize m0) rl
      | Inter(r1,rl) -> colorize m0 r1; Safelist.iter (colorize m0) rl
      | Diff(r1,r2)  -> colorize m0 r1; colorize m0 r2 in 
    let int_list_of_map m = 
      let ws = 31 in 
      let rec loop i mask cont a1 al = 
        if i > max_code then (a1::al)
        else if cont && i mod ws = 0 then 
          loop i 1 false 0 (a1::al)
        else
          let mask' = mask lsl 1 in 
          let a1' = if m.(i) then mask lor a1 else a1 in 
            loop (succ i) mask' true a1' al in 
        loop 0 1 false 0 [] in 
    let flatten m0 = 
      let ml = int_list_of_map m0 in
      try MapCache.find mcache ml 
      with Not_found -> 
        let cm = Array.make (succ max_code) 0 in 
        let rec loop i nc rml = 
          if i > max_code then Safelist.rev rml
          else
            let nc' = if m0.(i) then succ nc else nc in 
            let rml' = if m0.(i) then i::rml else rml in 
              (cm.(i) <- nc';
               loop (succ i) nc' rml') in 
        let rml = loop 1 0 [0] in
        let ms = (cm,Array.of_list rml) in
        MapCache.add mcache ml ms;
        ms in 
    let m = Array.make (succ (succ max_code)) false in 
    colorize m r0;
    flatten m
end

module DFA : sig 
  type s 
  val format_t : s -> unit
  val rank : s -> r
  val find_state : Rx.t -> s
  val mk_seq : s -> s -> s
  val mk_seqs : s list -> s
  val mk_alt : s -> s -> s
  val mk_alts : s list -> s
  val mk_star : s -> s
  val mk_inter : s -> s -> s
  val mk_diff : s -> s -> s
  val mk_complement : s -> s 
  val mk_reverse : s -> s
  val match_string : s -> string -> bool
  val match_string_positions : s -> string -> Int.Set.t
  val match_prefix_positions : s -> string -> Int.Set.t
  val match_string_reverse_positions : s -> string -> Int.Set.t
  val is_empty : s -> bool
  val splittable_cex : s -> s -> string option
  val iterable_cex : s -> string option
  val representative : s -> string option
end = struct          
  module rec M : sig 
    type s = 
        { uid : int;
          regexp : Rx.t;
          final : bool;
          (* caches *)
          mutable maps : unit -> (int array * int array);
          mutable next : unit -> s array;
          mutable reverse : unit -> s;
          mutable non_empty : (Q.t * (s * Q.t) list) -> bool;
          mutable has_non_empty : bool;
          mutable representative : (string * Q.t * (string * Q.t * s) list) -> string option;
          mutable suffs : unit -> s; }
  end = struct
    type s = 
        { uid : int;
          regexp : Rx.t;
          final : bool;
          (* caches *)
          mutable maps : unit -> (int array * int array);
          mutable next : unit -> s array;
          mutable reverse : unit -> s;
          mutable non_empty : (Q.t * (s * Q.t) list) -> bool;
          mutable has_non_empty : bool;
          mutable representative : (string * Q.t * (string * Q.t * s) list) -> string option;
          mutable suffs : unit -> s; }
  end and Q : Set.S with type elt = M.s = Set.Make(
    struct
      type t = M.s
      let compare s1 s2 = Pervasives.compare (s1.M.uid:int) (s2.M.uid:int)
    end)

  type s = M.s
  open M
    
  let format_t s = Rx.format_t s.regexp 
  let rank s = Rx.rank s.regexp

  (* maps of states *)
  module QM = Map.Make(
    struct
      type t = s
      let compare s1 s2 = Pervasives.compare s1.uid s2.uid
    end)
    
  let uid_counter = ref 0 
  let next_uid () = 
    incr uid_counter;
    !uid_counter
            
  module RCache = Hashtbl.Make
    (struct 
       type t = Rx.t
       let equal r1 r2 = (Rx.hash r1) = (Rx.hash r2) && (r1 = r2)
       let hash r0 = Rx.hash r0
     end)
  let rcache : s RCache.t = RCache.create 1069 

  let search go next last seen init s = 
    let pending = Queue.create () in 
    let rec loop seen acc = 
      if Queue.is_empty pending then acc
      else
        begin 
          let s = Queue.pop pending in 
          let seen',_,acc' = 
          Array.fold_left 
          (fun (sn,i,ac) si ->                
             if Q.mem si sn then 
               (sn,succ i,ac)
             else 
               let ci = Char.chr (snd (s.maps ())).(i) in                    
               let sn' = Q.add si sn in 
               Queue.push si pending;
               (sn',succ i,go sn' s ci si ac))
            (seen,0,acc) (s.next ()) in
            next loop seen' acc' 
        end in
      Queue.push s pending;
      last (loop (seen s) (init s))

  let dummy_impl _ = assert false

  let dummy_state = 
    { uid = 0;
      regexp = Rx.empty;
      final = false;
      maps = dummy_impl;
      next = dummy_impl;
      reverse = dummy_impl;
      non_empty = dummy_impl;
      has_non_empty = false;
      representative = dummy_impl;
      suffs = dummy_impl;
    }
      
  let rec mk_state r0 = 
    let s = 
      { uid = next_uid ();
        regexp = r0;
        final = Rx.nillable r0;
        maps = dummy_impl;
        next = dummy_impl;
        reverse = dummy_impl;
        non_empty = dummy_impl;
        has_non_empty = false;
        representative = dummy_impl;
        suffs = dummy_impl;
      } in 
    let maps_impl () = 
      dbg (fun () -> Util.format "@[MAPS_IMPL #%d@\n  " s.uid);
      let ms = Rx.maps r0 in 
      s.maps <- (fun () -> ms); 
      dbg (fun () -> Util.format "DONE MAPS_IMPL #%d@]@\n" s.uid);
      ms in
    let next_impl () = 
      dbg (fun () -> Util.format "@[NEXT_IMPL #%d@\n  " s.uid);
      let (cm,rm) = s.maps () in
      let nc = Array.length rm in
      let tr = Array.create nc dummy_state in 
      for i = 0 to pred nc do
        tr.(i) <- find_state (Rx.derivative rm.(i) r0)
      done;
      s.next <- (fun () -> tr); 
      dbg (fun () -> Util.format "DONE NEXT_IMPL #%d@]@\n" s.uid);            
      tr in 
    let reverse_impl () = 
      dbg (fun () -> Util.format "@[REVERSE_IMPL #%d@\n  " s.uid);
      let rev = find_state (Rx.reverse r0) in 
      s.reverse <- (fun () -> rev); rev in 
      dbg (fun () -> Util.format "DONE REVERSE_IMPL #%d@]@\n" s.uid);            
    let non_empty_impl (sn,ss) = 
      dbg (fun () -> Util.format "@[NON_EMPTY_IMPL #%d %s@\n  " s.uid (Rx.string_of_t s.regexp));
      let sn' = Q.add s sn in 
      let add si ss = if Q.mem si sn' then ss else (si,sn')::ss in 
      let jump_next ss = match ss with 
        | [] -> false
        | (s1,sn1)::rest -> s1.non_empty (sn1,rest) in 
      if Q.mem s sn then 
        jump_next ss
      else 
        let full_search () = 
          if s.final then true
          else 
            let tr = s.next () in 
            let len = Array.length tr in 
            let rec loop acc i = 
              if i < 0 then acc 
              else 
                let si = tr.(i) in 
                let acc' = add si acc in
                loop acc' (pred i) in
            jump_next (loop ss (pred len)) in 
        let b = 
          match Rx.desc r0 with 
            | Rx.Eps         -> true
            | Rx.CSet []     -> jump_next ss
            | Rx.CSet _      -> true
            | Rx.Seq(r1,r2)  -> 
                (find_state r1).non_empty (sn',ss) && (find_state r2).non_empty (sn',ss)
            | Rx.Star _      -> true
            | Rx.Alt(r1,rl)  -> 
                let ss' = Safelist.fold_left (fun ss' ri -> add (find_state ri) ss') ss (r1::rl) in 
                jump_next ss'
            | Rx.Diff(r1,r2) -> 
                let s2 = find_state r2 in 
                if s2.has_non_empty then 
                  jump_next ((s2,sn')::ss)
                else 
                  begin match Rx.desc r1,Rx.desc r2 with 
                    | Rx.Alt(r11,rl1),_ ->                 
                        let ss' = Safelist.fold_left (fun ss' ri -> add (find_state (Rx.mk_diff ri r2)) ss') ss (r1::rl1) in 
                        jump_next (ss' @ ss)
                    | _ -> full_search ()
                  end
            | _ -> full_search () in
          s.has_non_empty <- true;
          s.non_empty <- (fun _ -> b);
          dbg (fun () -> Util.format "DONE NON_EMPTY_IMPL %b #%d@]@\n" b s.uid);
          b in 

    let representative_impl (w,sn,ss) = 
      dbg (fun () -> Util.format "@[REPRESENTATIVE_IMPL #%d %s@\n  " s.uid (Rx.string_of_t s.regexp));
      let sn' = Q.add s sn in 
      let add ci si ss = if Q.mem si sn' then ss else (w ^ ci,sn',si)::ss in 
      let jump_next ss = match ss with 
        | [] -> None
        | (w1,sn1,s1)::rest -> s1.representative (w1,sn1,rest) in 
      if Q.mem s sn then jump_next ss
      else 
        let full_search () = 
          if s.final then Some w
          else 
            let tr = s.next () in 
            let len = Array.length tr in 
            let rec loop acc i = 
              if i < 0 then acc 
              else 
                let si = tr.(i) in 
                let ci = Char.chr (snd (s.maps ())).(i) in 
                let acc' = add (String.make 1 ci) si acc in
                loop acc' (pred i) in
            jump_next (loop ss (pred len)) in 
        let wo = 
          match Rx.desc r0 with 
            | Rx.Eps         -> Some w
            | Rx.CSet []     -> jump_next ss
            | Rx.CSet ((c1,_)::_)  -> 
                Some (w ^ (String.make 1 (Char.chr c1)))
            | Rx.Seq(r1,r2)  -> 
                begin match (find_state r1).representative (w,sn',ss) with 
                  | None -> None
                  | Some w1 -> 
                      Misc.map_option 
                        (fun w2 -> w1 ^ w2)
                        ((find_state r2).representative (w,sn',ss))
                end
            | Rx.Star _      -> Some w
            | Rx.Alt(r1,rl)  -> 
                let ss' = Safelist.fold_left (fun ss' ri -> add "" (find_state ri) ss') ss (r1::rl) in 
                jump_next (Safelist.rev ss')
            | Rx.Diff(r1,r2) -> 
                begin match Rx.desc r1,Rx.desc r2 with 
                  | Rx.Alt(r11,rl1),_ ->                 
                      let ss' = Safelist.fold_left (fun ss' ri -> add "" (find_state (Rx.mk_diff ri r2)) ss') ss (r1::rl1) in 
                        jump_next ss'
                  | _ -> full_search ()
                end
            | _ -> full_search () in
          s.representative <- (fun _ -> wo);
          s.has_non_empty <- true;
          s.non_empty <- (fun _ -> wo <> None);
          dbg (fun () -> Util.format "DONE REPRESENTATIVE_IMPL #%d@]@\n" s.uid);
          wo in 

    let suffs_impl () = 
      dbg (fun () -> Util.format "@[SUFFS_IMPL #%d@\n  " s.uid);
      let full_search () = 
        (* behaves like the acceptToAccept method from the
           dk.brics.automaton package (algorithm due to Anders Moller) *)
        search
          (fun _ _ _ si acc -> if si.final then Q.add si acc else acc)
          (fun loop -> loop)
          (fun rs -> 
             let rl = Q.fold (fun si l -> si.regexp::l) rs [] in 
               find_state (Rx.mk_alts rl))
          (fun s -> Q.singleton s)
          (fun s -> if s.final then Q.add s Q.empty else Q.empty)
          s in
      let res = match Rx.desc r0 with 
        | Rx.CSet _ -> find_state Rx.epsilon
        | Rx.Eps    -> s
        | Rx.Star _ -> s
        | Rx.Alt(r1,rl) -> 
            let rl' = Safelist.map (fun ri -> ((find_state ri).suffs ()).regexp) rl in 
            find_state (Rx.mk_alts rl')
        | Rx.Inter(r1,rl) -> 
            let rl' = Safelist.map (fun ri -> ((find_state ri).suffs ()).regexp) rl in 
            find_state (Rx.mk_inters rl')
        | Rx.Seq(r1,r2) -> 
            if not (Rx.nillable r2) then 
              (find_state r2).suffs ()
            else 
              find_state 
                (Rx.mk_alt 
                   ((find_state r1).suffs ()).regexp
                   ((find_state r2).suffs ()).regexp)
        | _ -> full_search () in 
      s.suffs <- (fun () -> res); 
      dbg (fun () -> Util.format "DONE SUFFS_IMPL #%d@]@\n" s.uid);
      res in 

      (* backpatch *)
      RCache.add rcache r0 s;      
      s.maps <- maps_impl;
      s.next <- next_impl;
      s.reverse <- reverse_impl;
      s.non_empty <- non_empty_impl;
      s.representative <- representative_impl;
      s.suffs <- suffs_impl;
      s

  and find_state r0 = 
    try RCache.find rcache r0
    with Not_found -> mk_state r0

  let lift_l f sl = find_state (f (Safelist.map (fun si -> si.regexp) sl))
  let mk_seqs = lift_l Rx.mk_seqs
  let mk_alts = lift_l Rx.mk_alts
  let mk_inters = lift_l Rx.mk_inters

  let lift_1 f s1 = find_state (f s1.regexp)
  let mk_star = lift_1 Rx.mk_star
  
  let lift_2 f s1 s2 = find_state (f s1.regexp s2.regexp)
  let mk_seq = lift_2 Rx.mk_seq
  let mk_alt = lift_2 Rx.mk_alt
  let mk_diff = lift_2 Rx.mk_diff
  let mk_inter = lift_2 Rx.mk_inter

  let mk_complement s1 = mk_diff (find_state Rx.anything) s1

  let mk_reverse s = s.reverse ()

  let is_empty s = not (s.non_empty (Q.empty,[]))

  let representative s = s.representative ("",Q.empty,[])

  let splittable_cex s1 s2 = 
(*     Util.format "CHECKING SPLITTABLE@\n%s@\nAND@\n%s@\n%!" *)
(*       (Rx.string_of_t s1.regexp) (Rx.string_of_t s2.regexp); *)
    let s2_rev = s2.reverse () in 
    let overlap_or_epsilon = mk_inter (s1.suffs ()) ((s2_rev.suffs ()).reverse ()) in
    let overlap = mk_diff overlap_or_epsilon (find_state Rx.epsilon) in     
(*     Util.format "DONE! GOING TO REPRESENTATIVE...@\n%!"; *)
    representative overlap

let iterable_cex s1 = 
  splittable_cex s1 (find_state (Rx.mk_star s1.regexp))

let match_string s w = 
  let n = String.length w in 
  let rec loop i s =     
    if i = n then s.final 
    else 
      ( (* Util.format "MATCH @[";
       Berror.nlify (String.sub w i (n-i));
       Util.format "@] with @[%s@]@\n" (Rx.string_of_t s.regexp); *)
      let cm,_ = s.maps () in 
      let c = cm.(Char.code w.[i]) in         
      let s' = (s.next ()).(c) in
      loop (succ i) s') in 
    loop 0 s
      
let match_string_positions s w = 
  let n = String.length w in 
  let rec loop acc i s = 
    let acc' = 
      if s.final then Int.Set.add i acc 
      else acc in 
      if i=n then acc'
      else 
        begin 
          let cm,_ = s.maps () in 
          let c = cm.(Char.code w.[i]) in
            loop acc' (succ i) (s.next ()).(c)
        end in
    loop Int.Set.empty 0 s

let match_prefix_positions s w = 
  let n = String.length w in 
  let rec loop acc i s = 
    let acc' = 
      if is_empty s then acc else Int.Set.add i acc in
      if i=n then acc'
      else 
        begin 
          let cm,_ = s.maps () in 
          let c = cm.(Char.code w.[i]) in
            loop acc' (succ i) (s.next ()).(c)
        end in
    loop Int.Set.empty 0 s

let match_string_reverse_positions s w = 
  let n = String.length w in 
  let rec loop acc i s = 
    let acc' = 
      if s.final then Int.Set.add (succ i) acc 
      else acc in 
      if i < 0 then acc'
      else 
        begin 
          let cm,_ = s.maps () in 
          let c = cm.(Char.code w.[i]) in
            loop acc' (pred i) (s.next ()).(c)
        end in
    loop Int.Set.empty (pred n) s
end

(* MAIN EXPORTS *)
type t = DFA.s
let format_t = DFA.format_t
let rank = DFA.rank
let string_of_t s = 
  Util.format_to_string (fun () -> format_t s) 

let anything = DFA.find_state Rx.anything
let anychar = DFA.find_state Rx.anychar 
let epsilon = DFA.find_state Rx.epsilon
let empty = DFA.find_state Rx.empty

let mk_string s = 
  let n = String.length s in 
  let rec loop i acc = 
    if i >= n then acc
    else
      let m = Char.code s.[pred n-i] in 
      let si = DFA.find_state (Rx.mk_cset [(m,m)]) in 
      loop (succ i) (si::acc) in 
  DFA.mk_seqs (loop 0 [])

let mk_cset pos cl = 
  let mker = if pos then Rx.mk_cset else Rx.mk_neg_cset in 
  DFA.find_state (mker cl)

let mk_seq = DFA.mk_seq
let mk_alt = DFA.mk_alt 
let mk_star = DFA.mk_star
let mk_inter = DFA.mk_inter
let mk_diff = DFA.mk_diff
let mk_complement = DFA.mk_complement
let mk_reverse = DFA.mk_reverse

let is_empty = DFA.is_empty

let representative = DFA.representative

let disjoint_cex s1 s2 = 
  representative (mk_inter s1 s2) 

let disjoint s1 s2 = 
  is_empty (mk_inter s1 s2) 

let equiv s1 s2 = 
     DFA.is_empty (DFA.mk_diff s1 s2) 
  && DFA.is_empty (DFA.mk_diff s2 s1) 

let splittable_cex = DFA.splittable_cex 

let splittable s1 s2 = match splittable_cex s1 s2 with 
  | None -> true
  | Some _ -> false

let iterable_cex = DFA.iterable_cex

let iterable s0 = match iterable_cex s0 with 
  | None -> true
  | Some _ -> false

let is_singleton s0 = 
  match representative s0 with 
    | None -> false
    | Some w -> is_empty (mk_diff s0 (mk_string w))

let match_string = DFA.match_string
let match_string_positions = DFA.match_string_positions
let match_prefix_positions = DFA.match_prefix_positions
let match_string_reverse_positions = DFA.match_string_reverse_positions

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

let init () = ()

