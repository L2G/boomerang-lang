(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* drx.ml - deterministic finite automata from regexps   *)
(*          constructed using derivatives, a technique   *)
(*          discovered by Brzozowski.                    *)
(*********************************************************)
(* $Id$ *)

(* standard imports *)
module L = Safelist
let (@) = L.append
module String = Fclstr.FS
type string = String.t

let (^) = String.append
module Char = Fclstr.FS
type char = Char.elt 

(* module instantiations and abbreviations *) 
module IS = Fclint.S
module Q = IS

(* graph traversals *)
module type GTType = sig
  type q
  type p
  val is_empty: p -> bool
  val push: q -> p -> unit
  val pop: p -> q
  end
  
(* simple graph traversal: iterate until S.p is empty *)
module type GTMod = functor(S:GTType) ->
sig
  type r
  val add: S.q -> r -> unit
  val go_bare: (S.q ->  r -> 'a -> 'a * bool) -> 'a -> S.p -> 'a
  val go_always_bare: (S.q ->  r -> 'a -> 'a) -> 'a -> S.p -> 'a
end
  
module GraphTraverse : GTMod = functor(S:GTType) -> 
struct 
  type r = S.p
  let add = S.push
  let go_bare f init pool =
    let pool' = (* XXX: for later S.copy *) pool in 
    let rec loop acc = 
      if S.is_empty pool' then acc
      else
        let qi = S.pop pool' in
        let acc',continue = f qi pool' acc in 
          if not continue then acc'
          else loop acc' in         
      loop init 
  let go_always_bare f init pool =
    let pool' = (* XXX: for later S.copy *) pool in  
    let rec loop acc = 
      if S.is_empty pool' then acc
      else loop (f (S.pop pool') pool' acc) in 
      loop init 
end

  module type GTType2 = sig
    include GTType
    type t
    type qs
    val fold: (q -> 'a -> 'a) -> qs -> 'a -> 'a 
    val next: t -> q -> qs
    end

  (* more complicated traversal, pass a function for filtering next
   * states 
   *)
  module type GTMod2 = functor(S:GTType2) ->
  sig
    type r 
    val add: S.q -> r -> unit
    val go_bare: (S.q ->  r -> 'a -> 'a * bool) -> 'a -> S.p -> 'a    
    val go_always_bare: (S.q ->  r -> 'a -> 'a) -> 'a -> S.p -> 'a
    val go_filter: (S.q -> bool) -> (S.q -> 'a -> 'a * bool) -> S.t -> 'a -> S.p -> 'a
  end    

  module GraphTraverse2 : GTMod2 = functor(S:GTType2) ->
  struct
    include GraphTraverse(S)
    let go_filter filter f t init pool = 
      let rec loop acc = 
        if S.is_empty pool then acc
        else
          let q = S.pop pool in
          let acc',continue = f q acc in 
            if not continue then acc'
            else 
              begin 
                S.fold 
                  (fun q () -> if filter q then S.push q pool)
                  (S.next t q) ();
                loop acc'
              end in         
        loop init 
  end 

(* alphabet *)
let char_code_min = Char.char_code_min
let char_code_max = Char.char_code_max
let min_char = Char.to_int char_code_min
let max_char = Char.to_int char_code_max
let alphabet = (min_char, max_char)

let rec cunion l l' =
  match l, l' with
    _, [] -> l
  | [], _ -> l'
  | (c1, c2)::r, (c1', c2')::r' ->
      if c2 + 1 < c1' then
        (c1, c2)::cunion r l'
      else if c2' + 1 < c1 then
        (c1', c2')::cunion l r'
      else if c2 < c2' then
        cunion r ((min c1 c1', c2')::r')
      else
        cunion ((min c1 c1', c2)::r) r'

let rec cinter l l' =
  match l, l' with
    _, [] -> []
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
      if c2 < c1' then
        cinter r l'
      else if c2' < c1 then
        cinter l r'
      else if c2 < c2' then
        (max c1 c1', c2)::cinter r l'
      else
        (max c1 c1', c2')::cinter l r'
          
let cmem c l = L.exists (fun (c1,c2) -> c1 <= c && c <= c2) l 
  
let rec cnegate (mi,ma) l =
  match l with
      [] ->
        if mi <= ma then [(mi, ma)] else []
    | (c1, c2)::r when ma < c1 ->
        if mi <= ma then [(mi, ma)] else []
    | (c1, c2)::r when mi < c1 ->
        (mi, c1 - 1) :: cnegate (c1,ma) l
    | (c1, c2)::r (* when c1 <= mi *) ->
        cnegate ((max mi (c2 + 1)),ma) r

module DFA = struct

  (* automata *)
  type t = 
      { cmap : int array;
        trans : int array array;
        init : int;
        final : Q.t;
      }

  let cmap d = d.cmap 
  let trans d = d.trans
  let init d = d.init
  let final d = d.final

  let mk c t f = 
    { cmap = c; 
      trans = t;
      init = 0; 
      final = f }
      
  let states d = Array.length d.trans
  let symbols d = Array.length d.trans.(0)
  let is_final d q = Q.mem q (final d) 

  let copy_matrix a = 
    let dummy = Array.make 0 (-1) in 
    let n = Array.length a in 
    let a' = Array.make n dummy in 
      for i=0 to (pred n) do 
        a'.(i) <- Array.copy a.(i)
      done;
      a'

  (* smap : t -> int array *)
  (* [smap d] calculates the mapping from symbols to chars from [d] *)
  let smap d = 
    let cm = cmap d in 
    let sm = Array.make (symbols d) (-1) in 
      for ci=0 to pred (Array.length cm) do 
        let si = cm.(ci) in 
          if sm.(si) < 0 then sm.(si) <- ci;
      done;
      sm
      
  (* module instantiations *)
  type this_t = t
  module BFS = GraphTraverse2(struct
    type q = Q.elt
    type qs = Q.elt array
    type p = Q.elt Queue.t 
    let is_empty = Queue.is_empty
    let push = Queue.push
    let pop = Queue.pop

    type t = this_t
    let next t q = t.trans.(q)
    let fold f a init = Array.fold_left (fun acc q -> f q acc) init a 
  end)

  let format d = 
    Util.format "@[";
    Util.format "cmap={@[";
    Array.iteri (fun ci si -> Util.format "'%s':s%d@ " (Char.escaped_repr (Char.of_int ci)) si) d.cmap;
    Util.format "@]}@\n";
    Util.format "trans=@[";
    Array.iteri (fun i ti -> 
      if i<> 0 then Util.format "@\n";
      Util.format "@[[";
      Array.iteri (fun j k -> if j <> 0 then Util.format " "; Util.format "q%d" k) ti;
      Util.format "@]]")
      d.trans;
    Util.format "@]@\n";
    Util.format "final={@["; 
    ignore (Q.fold (fun i is_fst -> if not is_fst then Util.format ","; Util.format "q%d" i; false) d.final true); 
    Util.format "@]}";
    Util.format "@]@\n"
      
  (* accept: t -> string -> bool 
   * 
   * [accept d s] is [true] iff [s] belongs to the language of [d].
   *)      
  let accept d s =
    let n = String.length s in 
    let rec loop q i = 
      if i=n then Q.mem q d.final
      else 
        let si = d.cmap.(Char.to_int (String.get s i)) in 
          loop d.trans.(q).(si) (succ i) in
      loop 0 0 

  let accept_prefix d s = 
    let n = String.length s in 
    let rec loop q i acc = 
      let acc' = if Q.mem q d.final then Q.add i acc else acc in 
        if i=n then acc'
        else 
          let si = d.cmap.(Char.to_int (String.get s i)) in 
            loop d.trans.(q).(si) (succ i) acc' in 
    loop 0 0 Q.empty

  let accept_reverse_prefix d s = 
    let n = String.length s in 
    let rec loop q i acc = 
      let acc' = if Q.mem q d.final then Q.add i acc else acc in 
        if i=0 then acc'
        else 
          let si = d.cmap.(Char.to_int (String.get s (pred i))) in 
            loop d.trans.(q).(si) (pred i) acc' in 
      loop 0 n Q.empty
        
  (* is_empty: t -> bool 
   * 
   * [is_empty d] is [true] iff the language of [d] is the empty set.
   *)
  let is_empty d = 
    let unseen = Array.make (states d) true in 
    let queue = Queue.create () in 
      Queue.push (init d) queue;
      BFS.go_filter 
        (fun q -> unseen.(q))
        (fun q acc -> 
          unseen.(q) <- false;
          let b = not (is_final d q) in (b,b))
        d true queue
        
  (* complement: t -> t
   * 
   * [complement t] constructs the automaton accepting the relative
   * complement of the language of [t]. 
   *)
  let mk_complement d =     
    let _,f_inverted = 
      Array.fold_left 
        (fun (qi,acc) _ -> 
          let acc' = 
            if is_final d qi then acc
            else Q.add qi acc in 
          (succ qi, acc'))
        (0,Q.empty) (trans d) in 
      mk
        (cmap d)
        (copy_matrix (trans d))
        f_inverted

  (* representative: t -> string
   * 
   * [representative d] returns a (shortest) string accepted by the 
   * automaton 
   *)
  let representative d =     
    let n = states d in 
    let a = Array.make n (true,-1,-1) in 
    let queue = Queue.create () in 
    let q0 = init d in 
      a.(q0) <- (false, -1, -1);
      Queue.push q0 queue;
    let qo = 
      BFS.go_bare
        (fun q pool _ -> 
          if is_final d q then
            (* if q is final state, return it *)
            (Some q, false)
          else
            (* otherwise, iterate over the transitions whose source is
             * q and for each qi not yet explored, add an entry in a
             * pointing back to q, labeled by a character in the
             * transition.  
             *)
            begin 
              Array.iteri
                (fun si qj -> 
                  match a.(qj) with 
                    | (false,_,_) -> ()
                    | _ -> 
                        BFS.add qj pool;
                        a.(qj) <- (false,q,si))
                d.trans.(q);
              (None,true)
            end)
        None
        queue in
      (* if result of graph search is None, then the language of the
       * automaton is empty.
       * 
       * otherwise, build a string by following the entries in a back
       * to an initial state 
       *)
      match qo with 
        | None -> raise Not_found
        | Some q -> 
            let sm = smap d in 
            let rec build_rep q acc = 
              if q = q0 then acc
              else 
                let _,q',s = a.(q) in
                let s' = String.make 1 (Char.of_int sm.(s)) ^ acc in 
                  build_rep q' s' in 
            build_rep q String.empty
end

module RegExp = struct
  (* syntax trees of regexps *)
  type d = 
        Anything 
      | Nothing 
      | Epsilon 
      | CSet of (int * int) list
      | Seq of d list
      | Alt of d list
      | Int of d list
      | Star of d
      | Neg of d

  type t = 
      { desc: d
      ; mutable comp: DFA.t option
      ; mutable reverse: DFA.t option}
        
  let t_of_d d = 
    { desc=d;
      comp=None;
      reverse = None 
    }

  let rec format_desc = function
      Anything -> Util.format "."
    | Nothing -> Util.format "[]"
    | Epsilon -> Util.format "\"\""
    | CSet(l) -> 
        let format_char_range (c1,c2) = 
          if c1=c2 then
            Util.format "%s" (Char.escaped_repr (Char.of_int c1))
          else
            Util.format "[%s-%s]"
              (Char.escaped_repr (Char.of_int c1))
              (Char.escaped_repr (Char.of_int c2)) in 
        L.iter format_char_range l
    | Seq(l) -> L.iter format_desc l
    | Alt(l) -> 
        Util.format "(";
        ignore (L.fold_left (fun is_fst di -> 
          if not is_fst then Util.format "|";
          format_desc di;
          false) true l);
        Util.format ")"
    | Int(l) -> 
        Util.format "(";
        ignore (L.fold_left (fun is_fst ri -> 
          if not is_fst then Util.format "&";
          format_desc ri;
          false) true l);
        Util.format ")"
    | Star(d) -> 
        Util.format "(";
        format_desc d;
        Util.format ")*";      
    | Neg(d) -> 
        Util.format "~(";
        format_desc d;
        Util.format ")"
          
  let rec format t0 = format_desc t0.desc

  let rec compare d1 d2 = match d1,d2 with
      Anything,Anything 
    | Nothing,Nothing
    | Epsilon,Epsilon  -> 0
    | CSet l1, CSet l2 -> Misc.dict_cmp Pervasives.compare l1 l2
    | Seq l1, Seq l2 
    | Alt l1, Alt l2   
    | Int l1, Int l2    -> Misc.dict_cmp compare l1 l2
    | Star r1, Star r2 
    | Neg r1, Neg r2   -> compare r1 r2
    | Anything,_       -> -1
    | _,Anything       -> 1
    | Nothing,_        -> -1
    | _,Nothing        -> 1
    | Epsilon,_        -> -1
    | _,Epsilon        -> 1
    | CSet _,_         -> -1
    | _,CSet _         -> 1
    | Seq _, _         -> -1
    | _,Seq _          -> 1
    | Alt _, _         -> -1
    | _,Alt _          -> 1
    | Int _, _         -> -1
    | _, Int _         -> 1
    | Star _, _         -> -1
    | _,Star _          -> 1

  (* constructors *)
  let mk_anything = t_of_d Anything
  let mk_nothing = t_of_d Nothing
  let mk_epsilon = t_of_d Epsilon

  let mk_cset_desc l = 
    if l=[] then Nothing
    else if l=[alphabet] then Anything
    else CSet(l)

  let mk_cset s = t_of_d (mk_cset_desc s)

  let mk_char c = 
    let i = Char.to_int c in 
      mk_cset [(i,i)]

  let mk_seq_desc d1 d2 = match d1,d2 with 
    | Epsilon,_       -> d2
    | _,Epsilon       -> d1
    | Nothing,_        -> Nothing        
    | _,Nothing        -> Nothing
    | Seq(l1),Seq(l2) -> (Seq(l1@l2))
    | d1,Seq(l2)      -> (Seq(d1::l2))
    | Seq(l1),r2      -> (Seq(l1@[r2]))
    | d1,d2           -> (Seq[d1;d2])
        
  let rec mk_seqs_desc = function
    | []     -> Epsilon
    | d1::l1 -> mk_seq_desc d1 (mk_seqs_desc l1)

  let mk_seq t1 t2 = t_of_d (mk_seq_desc t1.desc t2.desc)
  let mk_seqs l = L.fold_left mk_seq mk_epsilon l 

  let mk_alt_desc d1 d2 = 
    let rec merge l1 l2 = match l1,l2 with
      | [],_ -> l2
      | _,[] -> l1
      | d1::l1',d2::l2' -> 
          let c = compare d1 d2 in 
            if c=0 then d1::merge l1' l2'
            else if c < 0 then d1 :: merge l1' l2
            else  d2 :: merge l1 l2' in 
    let c = compare d1 d2 in 
      if c=0 then d1
      else 
        begin
          match d1,d2 with
            | Nothing,_        -> d2
            | _,Nothing        -> d1
            | CSet s1, CSet s2 -> mk_cset_desc (cunion s1 s2)
            | Alt(l1), Alt(l2) -> Alt(merge l1 l2)
            | Alt(l1), r2      -> Alt(merge l1 [r2])
            | r1, Alt(l2)      -> Alt(merge [r1] l2)
            | d1,d2            -> Alt(merge [d1] [d2])
        end
          
  let rec mk_alts_desc = function
    | []     -> Nothing
    | d1::l1 -> mk_alt_desc d1 (mk_alts_desc l1)

  let mk_alt t1 t2 = t_of_d (mk_alt_desc t1.desc t2.desc)
  let mk_alts l1 = L.fold_left mk_alt mk_nothing l1

  let mk_star_desc = function
    | Nothing       -> Epsilon
    | Epsilon       -> Epsilon
    | Star(_) as d0 -> d0 
    | d0            -> Star(d0)

  let mk_star t0 = t_of_d (mk_star_desc t0.desc)

  let mk_inter_desc d1 d2 = 
    let rec merge l1 l2 = match l1,l2 with
      | [],_ -> l2
      | _,[] -> l1
      | d1::l1',d2::l2' -> 
          let c = compare d1 d2 in 
            if c=0 then d1::merge l1' l2'
            else if c < 0 then d1::merge l1' l2
            else  d2::merge l1 l2' in 
    let c = compare d1 d2 in 
      if c=0 then d1
      else 
        begin
          match d1,d2 with
            | Nothing,_           -> Nothing
            | _,Nothing           -> Nothing
            | CSet s1, CSet s2    -> mk_cset_desc (cinter s1 s2)
            | Int(l1), Int(l2)    -> Int(merge l1 l2)
            | Int(l1), r2         -> Int(merge l1 [r2])
            | r1, Int(l2)         -> Int(merge [r1] l2)
            | d1,d2               -> Int(merge [d1] [d2])
        end
  let mk_inter t1 t2 = t_of_d (mk_inter_desc t1.desc t2.desc)
          
  let rec mk_inters_desc = function
      [] -> mk_star_desc Anything
    | d1::l1 -> mk_inter_desc d1 (mk_inters_desc l1)

  let mk_inters l1 = L.fold_left mk_inter (mk_star mk_anything) l1

  let mk_neg_desc = function
    | Nothing    -> mk_star_desc Anything
    | Neg(d1)    -> d1
    | d0         -> Neg(d0)

  let mk_neg t0 = t_of_d (mk_neg_desc t0.desc)

  let rec mk_reverse_desc d0 = match d0 with
      Anything 
    | Nothing 
    | Epsilon 
    | CSet _   -> d0
    | Seq(l1)  -> mk_seqs_desc (L.rev (L.map mk_reverse_desc l1))
    | Alt(l1)  -> mk_alts_desc (L.map mk_reverse_desc l1)
    | Int(l1)  -> mk_inters_desc (L.map mk_reverse_desc l1)
    | Star(d1) -> mk_star_desc (mk_reverse_desc d1)
    | Neg(d1) -> mk_neg_desc (mk_reverse_desc d1)

  let rec has_epsilon_desc = function
    | Nothing  -> false
    | Anything -> false
    | Epsilon  -> true
    | CSet _   -> false
    | Star r1  -> true
    | Seq(l1) 
    | Int(l1)  -> L.for_all has_epsilon_desc l1
    | Alt(l1)  -> L.exists has_epsilon_desc l1
    | Neg d1   -> not (has_epsilon_desc d1)
  let rec has_epsilon t0 = has_epsilon_desc t0.desc 
    
  let delta_desc d0 = if has_epsilon_desc d0 then Epsilon else Nothing

  let rec derivative_desc c = function
    | Anything    -> Epsilon
    | Nothing     -> Nothing
    | Epsilon     -> Nothing
    | CSet s      -> if cmem c s then Epsilon else Nothing
    | Seq([])     -> Nothing
    | Seq([d1])   -> derivative_desc c d1
    | Seq(d1::l1) -> 
        mk_alt_desc
          (mk_seqs_desc ((derivative_desc c d1)::l1))
          (mk_seqs_desc [delta_desc d1; derivative_desc c (Seq(l1))])
    | Alt([])       -> Nothing
    | Alt([d1])     -> derivative_desc c d1
    | Alt(d1::l1)   -> mk_alt_desc (derivative_desc c d1) (derivative_desc c (Alt(l1)))
    | Int([]) as d1 -> mk_seq_desc (Epsilon) d1
    | Int([d1])     -> derivative_desc c d1
    | Int(d1::l1)   -> mk_inter_desc (derivative_desc c d1) (derivative_desc c (Int(l1)))
    | Star d0 as d1 -> mk_seq_desc (derivative_desc c d0) d1
    | Neg d1        -> mk_neg_desc (derivative_desc c d1) 
        
  let rec derivative c t0 = derivative_desc c t0.desc
    
  (* compilation, using derivatives *)
  module H = Hashtbl.Make(struct
    type t = d
    let equal r1 r2 = compare r1 r2 = 0
    let hash = Hashtbl.hash
  end)

  module RMP = Mapplus.Make(struct
    type t = d
    let compare = compare
  end)
  module RS = RMP.KeySet
  module RM = RMP.Map

  (* split_sigma *)
  let split_sigma d = 
    let make_cmap () = Array.make (succ (succ max_char)) false in 
    let rec split s cm =
      match s with
          []    -> ()
        | (i,j)::r -> 
            cm.(i) <- true;
            cm.(j + 1) <- true; 
            split r cm in             
    let rec colorize cm d0 =
      let rec colorize = function
        | Anything | Nothing | Epsilon -> ()
        | CSet s -> split s cm 
        | Seq l | Alt l | Int l -> List.iter colorize l
        | Star(d) | Neg(d) -> colorize d in
      colorize d0 in 
    let flatten_cmap cm =    
      let c = Array.make (succ max_char) 0 in
      let s = Array.make (succ max_char) 0 in
      let v = ref 0 in
        for i = min_char to max_char do
          if i <> min_char && cm.(i) then incr v;
          if cm.(i) then s.(!v) <- i;
          c.(i) <- !v;
        done;
        (c, s, !v + 1) in 
      (* main *)
    let cm = make_cmap () in 
      colorize cm d;
      let c2s,s2c,n = flatten_cmap cm in 
      let s2c = Array.sub s2c 0 n in 
        (n,c2s,s2c)

  let dfa_of_d d = 
    let cache : ((int * d array) H.t) = H.create 97 in 
    let symbols,c2s,s2c = split_sigma d in 
    let rec loop uid pending finished = 
      if RS.is_empty pending then ()
      else 
        let r = RS.choose pending in 
        let a = Array.create symbols Nothing in 
        let finished' = RS.add r finished in 
        let rec aux i acc = 
          if i=symbols then acc
          else 
            let ci = s2c.(i) in 
            let r' = derivative_desc ci r in              
            let acc' = 
                if not (RS.mem r' finished') then RS.add r' acc
                else acc in
                a.(i) <- r';
                aux (succ i) acc' in 
        let new_rs = aux 0 RS.empty in 
        let pending' = RS.remove r (RS.union pending new_rs) in 
          H.add cache r (uid,a);
          loop (succ uid) pending' finished' in
    (* main dfa *)
    let () = loop 0 (RS.singleton d) RS.empty in
    let states = H.length cache in 
    let dfa_trans = Array.make_matrix states symbols (-1) in 
    let dfa_final = H.fold (fun ri (i,ti) final -> 
      for j=0 to symbols-1 do                 
        dfa_trans.(i).(j) <- fst (H.find cache ti.(j))
      done; 
      if has_epsilon_desc ri then Q.add i final else final)
      cache Q.empty in 
      DFA.mk c2s dfa_trans dfa_final
        
  let force t0 = 
    match t0.comp with 
        Some d -> d
      | None -> 
          let d = dfa_of_d t0.desc in 
            t0.comp <- Some d;
            d
              
  let force_reverse t0 = 
    match t0.reverse with 
        Some d -> d
      | None -> 
          let d_rev = dfa_of_d (mk_reverse_desc t0.desc) in 
            t0.reverse <- Some d_rev;
            d_rev 

  let is_empty t0 = match t0.desc with
    | Nothing  -> true
    | Anything -> false
    | Epsilon  -> false
    | CSet _   -> false
    | Star r1  -> false
    | _        -> DFA.is_empty (force t0)

  let match_string t0 s = match t0.desc with
    | Nothing -> false
    | Anything -> String.length s = 1
    | Epsilon -> String.length s = 0
    | CSet cs -> String.length s = 1 && (cmem (Char.to_int (String.get s 0)) cs)
    | _ -> DFA.accept (force t0) s
    
end
module R = RegExp

type t = R.t 
let epsilon = R.mk_epsilon 
let empty = R.mk_nothing
let mk_char = R.mk_char
let mk_str ignore_case s = 
  let n = String.length s in 
  let rec loop i acc = 
    if i=n then L.rev acc 
    else loop (succ i) (R.mk_char (String.get s i)::acc) in 
  R.mk_seqs (loop 0 [])

let mk_cset pos l = 
  let l' = L.map (fun (c1,c2) -> (Char.to_int c1, Char.to_int c2)) l in 
  R.mk_cset (if pos then l' else cnegate alphabet l')

let mk_alt = R.mk_alt
let mk_seq = R.mk_seq
let mk_seqs = R.mk_seqs
let mk_star = R.mk_star
let mk_rep _ _ = R.mk_star
let mk_lowercase _ = assert false
let mk_uppercase _ = assert false
let mk_complement = R.mk_neg 
let mk_diff t1 t2 = R.mk_inter t1 (R.mk_neg t2)
let mk_inter = R.mk_inter
let is_empty = R.is_empty
let representative t0 = DFA.representative (R.force t0)
let equiv t1 t2 = (is_empty (mk_diff t1 t2)) && (is_empty (mk_diff t2 t1))

let match_str = R.match_string
let match_prefix t0 s = DFA.accept_prefix (R.force t0) s
let find_exit_automaton t0 s = raise Not_found

type dual_single_split = { dss_example : string;
			   dss_cut1 : int;
			   dss_cut2 : int}

let example_of_dss _ = assert false

type not_ambig = NA_true | NA_false of dual_single_split

type dual_multi_split = { dms_example : string;
			  dms_cut1 : int list;
			  dms_cut2 : int list}

let example_of_dms _ = assert false

type not_star_ambig = NSA_true | NSA_empty_word | NSA_false | NSA_false_ce of dual_multi_split

let unambig_seq _ = assert false
let unambig_rep _ = assert false

let split_positions t1 t2 s = 
  let ps1 = match_prefix t1 s in 
  let ps2 = DFA.accept_reverse_prefix (R.force_reverse t2) s in 
    IS.inter ps1 ps2

let unambig_split t1 t2 s = 
  let ps12 = split_positions t1 t2 s in 
    if IS.is_empty ps12 then None
    else 
      let i = IS.choose ps12 in 
      let n = String.length s in 
        assert (IS.cardinal ps12 = 1);
        Some (String.sub s 0 i, String.sub s i (n-i))
    
let unambig_star_split t1 s =
  let star_t1 = mk_star t1 in 
  let ps = match_prefix star_t1 s in 
  let ps_rev = DFA.accept_reverse_prefix (R.force_reverse star_t1) s in 
  let ps12 = IS.remove 0 (IS.inter ps ps_rev) in 
    L.rev
      (snd (IS.fold 
               (fun i (j,acc) -> (i,String.sub s j (i-j)::acc))
               ps12
               (0,[])))

let count_unambig_star_split _ _ = assert false
let print_multi_split _ _ = ()
let print_split _ _ = ()

let print_stat_trim () = ()

let determinize t = t

(************************************************)
(* OLD JUNK                                      *) 
(* let mk_char c = mk_char (Char.of_char c) *)
(* let r1 = mk_char 'n' *)
(* let r2 = mk_seqs [mk_char 'x'; *)
(*                   mk_alt  *)
(*                     (mk_char 'y') *)
(*                     (mk_char 'z')] *)
(* let r = mk_alt r1 r2 *)
(* let r' = mk_alt r2 r1 *)
(* let d = R.force r *)
(* let t1 = mk_alt (mk_char 'a') (mk_char 'b') *)
(* let t2 = mk_alt (mk_char 'b') (mk_char 'a') *)
(* let t3 = mk_seq (mk_char 'a') (mk_star (mk_char 'a')) *)
(* let t4 = mk_seq t1 t1 *)
(* let _ = *)
(*   Util.format "--- Welcome to Drx ---@\n"; *)
(*   Util.format "R: "; R.format r; Util.format "@\n"; *)
(*   Util.format "REP: %s@\n" (String.to_string (representative r1)); *)
(*   Util.format "REP: %s@\n" (String.to_string (representative r2)); *)
(*   Util.format "REP: %s@\n" (String.to_string (representative r)); *)
(*   Util.format "IS_EMPTY: %b@\n" (DFA.is_empty d); *)
(*   Util.format "EQUIV: [%b]@\n" (equiv t1 t2); *)
(*   Util.format "REP: %s@\n" (String.to_string (representative t2)); *)
(*   let s = "aa" in *)
(*     Util.format "POSES: of %s # " s; *)
(*     R.format t3; *)
(*     Util.format " = {%s}@\n" *)
(*       (Misc.concat_list "," (L.map string_of_int *)
(*                                 (Q.elements *)
(*                                     (match_prefix t3 (String.of_string "aa"))))); *)
(*     let sl = unambig_star_split (mk_star t4) (String.of_string "abba") in *)
(*       Util.format "SL: [%s]@\n" (Misc.concat_list "," (L.map String.to_string sl)); *)
(*   L.iter *)
(*     (fun s -> Util.format "%s # %b@\n" s (DFA.accept d (String.of_string s))) *)
(*     [ "nate foster" *)
(*     ; "aB" *)
(*     ; "ac" *)
(*     ; "xy" *)
(*     ; "xZ" *)
(*     ]; *)
(*   exit 0 *)
