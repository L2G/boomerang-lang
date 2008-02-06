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
(* /boomerang/src/erx.ml                                                       *)
(* Extended regexps                                                            *)
(* $Id$                                                                        *)
(*******************************************************************************)

let sprintf = Printf.sprintf 
let (@) = Safelist.append
module RS = Bstring
let (^) = RS.append
open Num

(* --- Some debugging options --- *)
  
let test_trim = Prefs.createBool "testtrim" false
  "Cause to print stats on the efficiency of trimming" 
  "Cause to print stats on the efficiency of trimming"

let use_trim =  Prefs.createBool "usetrim" false
  "Cause to effectively use the triming. Usefull only with \"testtrim\" activated" 
  "Cause to effectively use the triming. Usefull only with \"testtrim\" activated" 

let no_cache =  Prefs.createBool "nocache" false
  "Desactivate the usage of cache"
  "Desactivate the usage of cache"


type debug_trim = 
    { name_calling_fun : string;
      mutable number_of_call : num;
      mutable total_size_before : num;
      mutable total_size_after : num}

let incr_debug_trim dt sb sa = 
  dt.number_of_call <- succ_num dt.number_of_call;
  dt.total_size_before <- add_num dt.total_size_before (num_of_int sb);
  dt.total_size_after <- add_num dt.total_size_after (num_of_int sa)

let dt_other = 
  { name_calling_fun = "the other functions";
    number_of_call = num_of_int 0;
    total_size_before = num_of_int 0;
    total_size_after = num_of_int 0 }

let dt_str = {dt_other with name_calling_fun = "mk_str"}
let dt_cset = {dt_other with name_calling_fun = "mk_cset"}
let dt_alt = {dt_other with name_calling_fun = "mk_alt"}
let dt_seq = {dt_other with name_calling_fun = "mk_seq"}
let dt_rep = {dt_other with name_calling_fun = "mk_rep"}
let dt_complement = {dt_other with name_calling_fun = "mk_complement"}
let dt_diff = {dt_other with name_calling_fun = "mk_diff"}
let dt_inter = {dt_other with name_calling_fun = "mk_inter"}
let dt_determinize = {dt_other with name_calling_fun = "determinize"}

let print_debug_trim dt = 
  if dt.number_of_call >/ (num_of_int 0) then
  begin 
    Util.format "@[**** Usage of [trim] in %s ****@\n" dt.name_calling_fun;
    Util.format "@[trim has been called %s times@\n" (string_of_num dt.number_of_call);
    let asbbi = div_num dt.total_size_before dt.number_of_call in
    let asabi = div_num dt.total_size_after dt.number_of_call in
    let asbf = float_of_num asbbi in
    let asaf = float_of_num asabi in
    let percent = int_of_float (((asbf -. asaf) /. asbf) *. 100.) in
    Util.format "Average size before calling : %d@\n" (int_of_num (floor_num asbbi));
    Util.format "Average size after calling : %d@\n" (int_of_num (floor_num asabi));
    Util.format "Average gain : %d%%@\n@]@\n@]" percent
  end
  else
    Util.format "@[ **** [trim] has not been called from %s ****@\n@]" dt.name_calling_fun

let print_stat_trim () = 
  if Prefs.read test_trim then begin
    Util.format "@[XXXXXX Stats about the usage of [trim] XXXXXX@\n@\n";
    print_debug_trim dt_str;
    print_debug_trim dt_cset;
    print_debug_trim dt_alt;
    print_debug_trim dt_seq;
    print_debug_trim dt_rep;
    print_debug_trim dt_complement;
    print_debug_trim dt_diff;
    print_debug_trim dt_inter;
    print_debug_trim dt_determinize;
    print_debug_trim dt_other;
    Util.format "@]"
  end

(* --- global constants --- *)

(* alphabet is ascii *)
let char_code_max = 255
let min_char = Char.chr 0
let max_char = Char.chr char_code_max

(* --- module instantiations --- *)

(* character maps, sets *)
module CM = Map.Make(struct 
  type t = char
  let compare = Char.compare
end)
module IS = Rint.Set

(* formatting functions *)
let format_set fold format_elt set = 
  Format.printf "{@[";
  ignore 
    (fold (fun e not_fst -> 
      if not_fst then Format.printf ","; 
      format_elt e; 
      true) set false);
  Format.printf "@]}"

let format_map fold format_key format_val map = 
  Format.printf "{@[";
  ignore 
    (fold (fun k v not_fst -> 
      if not_fst then Format.printf "@\n"; 
      format_key k;
      Format.printf "->";
      format_val v; 
      true) map false);
  Format.printf "@]}"

(* --- non-deterministic automata --- *)  
module NFA = struct

  (* standard data structures over states *)
  module QS = Rint.Set          (* sets of states *)
  module QM = Rint.Map          (* maps keyed by states *)
  module QSM = Map.Make(QS)     (* maps keyed by sets of states *)
  module QP = struct   (* pairs of states *)
    type t = int * int 
    let compare (a,b) (c,d) = 
      if a < c then -1
      else if c < a then 1
      else if b < d then -1
      else if d < b then 1
      else 0
  end
  module QPS = Set.Make(QP)
  module QPM = Map.Make(QP)

  (* map a function over a state set *)
  let qs_map f s = QS.fold
    (fun si acc -> QS.add (f si) acc)
    s QS.empty 

  (* add a constant to every element of a state set *)
  let qs_add n s = qs_map ((+) n) s

 (* transition relation: maps from character ranges to sets of
  * states 
  *)
  module T = Crm.Make(struct 
    include QS 
  end)(Bstring)

  (* --- type to keep track of the (un)ambiguity of automata --- *)

  type ambiguity = 
      Amb_unambig
    | Amb_unknown
    | Amb_ambig of RS.t

  (* --- limits for the "heuristic" --- *)
  let h_det = 0
  let h_trim = 0
  let h_reverse = 0 
  let h_star = 0
  type 'a heurist = Heur of int | More of 'a
    
  (* automata *)
  type t = 
      { init : QS.t;
	final : QS.t;
        trans : T.t array;
	mutable ambiguity : ambiguity; 
	mutable det : t heurist;
	mutable trim : t heurist;
	mutable reverse : t heurist;
	mutable star : t heurist;
      }

  let heur_det t = match t.det with
    | Heur i -> 
	t.det <- Heur (succ i);
	(not (Prefs.read no_cache)) && i >= h_det
    | _ -> true

  let heur_det_non_incr t =  match t.det with
    | Heur i -> false
    | _ -> true

  let heur_trim t = match t.trim with
    | Heur i -> 
	t.trim <- Heur (succ i);
	(not (Prefs.read no_cache)) && i >= h_trim
    | _ -> true

  let heur_reverse t = match t.reverse with
    | Heur i -> 
	t.reverse <- Heur (succ i);
	(not (Prefs.read no_cache)) && i >= h_reverse
    | _ -> true

  let heur_star t = match t.star with
    | Heur i -> 
	t.star <- Heur (succ i);
	(not (Prefs.read no_cache)) && i >= h_star
    | _ -> true

  let reset_heur t =
    t.ambiguity <- Amb_unknown;
    t.det <- Heur 0;
    t.trim <- Heur 0;
    t.reverse <- Heur 0;
    t.star <- Heur 0;
    t
        

  let define_to_be_unambig t =
    t.ambiguity <- Amb_unambig

  (* initial set of states *)
  let init t = t.init
  let final t = t.final

  (* number of states in an automaton *)
  let num_states t = Array.length t.trans

  (* next: t -> QS.t -> char -> QS.t
   *   
   *   [next t f c] computes the set of states reachable by 
   *   transitions from [f] on [c] in [t]
   *)
  let next t f c = QS.fold 
    (fun q acc -> QS.union acc
      (try T.find_elt c t.trans.(q)
        with Not_found -> QS.empty))
    f QS.empty 

(* succ_qs: t -> QS.elt -> QS.t 
 * 
 * [q_succ t q] gives the set of states reachable by 
 * transitions from [q] in [t]
 *)
  let q_succ t q = 
    T.fold 
      (fun _ qs acc -> QS.union qs acc) 
      t.trans.(q) QS.empty
    
  let failsize () = 
    raise (Error.Harmony_error (fun () -> 
      Util.format "Cannot allocate Array.t for transition function"))
      
  (* wrapped Array functions *)
  let array_make n e = try 
      Array.make n e 
    with Invalid_argument _ -> failsize()    
  
  let array_append a1 a2 = try 
      Array.append a1 a2 
    with Invalid_argument _ -> failsize()
    
  (* formatting *)
  let format_state s = Util.format "q%d" s

  let format_trans t = 
    Array.iteri (fun qi crmi -> 
      format_state qi;
      Util.format " -> ";
      format_map T.fold 
        (fun (a,b) -> if a=b then Util.format "'%s'" (RS.escaped_repr a) else Util.format "['%s'-'%s']" (RS.escaped_repr a) (RS.escaped_repr b))
        (format_set QS.fold format_state)
        crmi;
      Util.format "@\n")
      t

  let format a = 
    Util.format "TRANSITION: {@["; format_trans a.trans;
    Util.format "@]}@\n";
    Util.format "INITIAL   : "; format_set QS.fold format_state a.init; Util.format "\n";
    Util.format "FINAL     : "; format_set QS.fold format_state a.final;
    Util.format "@\n"

  (* --- constructors --- *)
  (* empty: t
   *   automaton accepting the empty language 
   *)
  let rec empty = 
    { init = QS.empty;
      final = QS.empty;
      ambiguity = Amb_unambig;
      trans = array_make 0 T.empty;
      det = More empty;
      trim = More empty;
      reverse = More empty;
      star = Heur 0;}
      

  (* epsilon: t
   * 
   * automaton accepting the singleton set containing the empty
   * string 
   *)
  let rec epsilon = 
    let epsilon_trans = array_make 1 T.empty in
    let epsilon_init = QS.singleton 0 in
    let epsilon_final = epsilon_init in
      { init = epsilon_init;
	trans = epsilon_trans;
        final = epsilon_final;
	ambiguity = Amb_unambig;
	det = More epsilon;
	trim = More epsilon;
	reverse = More epsilon;
	star = More epsilon; }

  let mk_lowercase t = 
    let trans = t.trans in 
    let n = Array.length trans in 
    let a = array_make n T.empty in 
    let az_cr = (RS.sym_of_char 'A', RS.sym_of_char 'Z') in 
      for i=0 to pred n do
        let ti = 
          T.fold (fun cr qs acc -> 
            match T.isect_range cr az_cr with
                None -> T.add cr qs acc
              | Some(ci,cj) -> 
                  let acc1 = T.add cr qs acc in 
                  let acc2 = T.rem (ci,cj) acc1 in                 
                    T.add (RS.lowercase ci, RS.lowercase cj) qs acc2)
            trans.(i) T.empty in 
          a.(i) <- ti
      done;
      { init = t.init;
        trans = a;
        final = t.final;
        ambiguity = Amb_unknown;
	det = Heur 0;
        trim = Heur 0;
        reverse = Heur 0;
        star = Heur 0 }
      
  let mk_uppercase t = 
    let trans = t.trans in 
    let n = Array.length trans in 
    let a = array_make n T.empty in 
    let az_cr = (RS.sym_of_char 'a', RS.sym_of_char 'z') in 
      for i=0 to pred n do
        let ti = 
          T.fold (fun cr qs acc -> 
            match T.isect_range cr az_cr with
                None -> T.add cr qs acc
              | Some(ci,cj) -> 
                  let acc1 = T.add cr qs acc in 
                  let acc2 = T.rem (ci,cj) acc1 in                 
                    T.add (RS.uppercase ci, RS.uppercase cj) qs acc2)
            trans.(i) T.empty in 
          a.(i) <- ti
      done;
      { init = t.init;
        trans = a;
        final = t.final;
	ambiguity = Amb_unknown;
        det = Heur 0;
        trim = Heur 0;
        reverse = Heur 0;
        star = Heur 0 }

  let extend t old_sym new_sym = 
    let trans = t.trans in 
    let n = Array.length trans in 
    let a = array_make n T.empty in 
    let old_cr = (old_sym,old_sym) in 
    let new_cr = (new_sym,new_sym) in 
      for i=0 to pred n do
        let ti = 
          T.fold (fun cr qs acc -> 
                    match T.isect_range cr old_cr with 
                      | None -> T.add cr qs acc
                      | Some _ -> 
                          let acc1 = T.add cr qs acc in 
                          T.add new_cr qs acc1)
            trans.(i) T.empty in 
          a.(i) <- ti
      done;
      { init = t.init;
        trans = a;
        final = t.final;
        ambiguity = Amb_unknown;
        det = Heur 0;
        trim = Heur 0;
        reverse = Heur 0;
        star = Heur 0 }
                          
  (* mk_reverse: t -> t
   *  
   * [mk_reverse t] is the automaton accepting the 
   * reversal of the language accepted by [t]. 
   *)
  let mk_reverse t = 
    match t.reverse with
      | More tr -> tr
      | _ ->
	  (let trans = t.trans in
	   let n = Array.length trans in
	   let a = array_make n T.empty in
	     for i = 0 to n - 1 do
	       let si = QS.singleton i in
		 T.iter (fun cr qs -> 
			   QS.iter (fun q -> a.(q) <- T.add cr si a.(q)) qs)
		   trans.(i)
	     done;
	     let tr = { init = t.final;
			final = t.init;
			trans = a;
			ambiguity = Amb_unknown;
			det = Heur 0;
			trim = Heur 0;
			reverse = More t;
			star = Heur 0} in
	       if heur_reverse t then t.reverse <- More tr;
	       tr)
         
  (* ---- Graph Traversal Functors --- *)
  module type GTType = sig
    type q
    type p
    val copy: p -> p
    val is_empty: p -> bool
    val push: q -> p -> unit
    val pop: p -> q
    end

  (* simple graph traversal, basically just iterates until S.p is empty *)
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
    val go_filter: (S.q -> bool) -> (S.q -> 'a -> 'a * bool) -> t -> 'a -> S.p -> 'a
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

  (* functor instantiations *)
  module QBFS = GraphTraverse2(struct
    type q = QS.elt
    type qs = QS.t
    type p = QS.elt Queue.t
    let is_empty = Queue.is_empty
    let copy = Queue.copy
    let push = Queue.push
    let pop = Queue.pop
    let fold = QS.fold
    let next = q_succ 
  end)


  module QPairBFS = GraphTraverse(struct
    type q = QPS.elt
    type p = QPS.elt Queue.t
    let is_empty = Queue.is_empty
    let copy = Queue.copy
    let push = Queue.push
    let pop = Queue.pop
  end)

  module IQSBFS = GraphTraverse(struct
    type q = (int * QS.t)
    type p = (int * QS.t) Queue.t
    let is_empty = Queue.is_empty
    let copy = Queue.copy
    let push = Queue.push
    let pop = Queue.pop
  end)

  module QPBFS = GraphTraverse(struct
    type q = (int * QP.t)
    type p = (int * QP.t) Queue.t
    let is_empty = Queue.is_empty
    let copy = Queue.copy
    let push = Queue.push
    let pop = Queue.pop
  end)

  (* trim: t -> t
   * 
   * [trim t] returns the trimmed automaton accepting the language of
   * [t]. 
   *)
  type status = Unknown | Accessible | CoAccessible of int
  let trim ?(loged = false) t =
    match t.trim with
      | More tt -> tt;
      | _ ->
	  (let n = num_states t in 
	   let a = array_make n Unknown in 
	   let queue = Queue.create () in 
	     QS.iter
               (fun qi -> 
		  a.(qi) <- Accessible;
		  Queue.push qi queue)
               t.init;
	     (* mark accessible states *)
	     QBFS.go_filter
               (fun q -> a.(q) = Unknown)
               (fun q () ->                  
                 a.(q) <- Accessible;
                 ((), true))
               t
               ()
               queue;
	     Queue.clear queue;
	     (* calculate accessible final states *)
	     let i = QS.fold (fun qi i -> 
	       if a.(qi) = Accessible then 
		 begin 
		   a.(qi) <- CoAccessible(i);
		   Queue.push qi queue;
		   (succ i)
		 end
	       else i)
               t.final 0 in 
               (* if no final states accesible, then the trim automaton is empty *)
               if Queue.is_empty queue then 
		 begin
		   t.trim <- More empty;
		   empty
		 end
               else
		 (* otherwise, mark coaccessible states in reverse automaton *)
		 let n' = 
	           QBFS.go_filter
                     (fun qi -> a.(qi) = Accessible)
                     (fun q i -> 
			match a.(q) with
			  | Accessible ->
			      a.(q) <- CoAccessible(i);
			      (succ i, true)
			  | CoAccessible _ ->
			      (i, true)
			  | _ -> assert false
		     )
                     (mk_reverse t)
                     i
                     queue in 
		   (* rewrite a state set in the trim automaton *)
		 let qs_trim qs = QS.fold
		   (fun qi acc -> match a.(qi) with 
		       CoAccessible(qi') -> QS.add qi' acc
		     | _ -> acc)
		   qs QS.empty in
		   (* finally, construct the trim automaton *)
		 let trim_trans = array_make (succ n') T.empty in 
		   for qi=0 to (pred n) do 
		     match a.(qi) with 
			 CoAccessible(qi') -> 
			   trim_trans.(qi') <-
			     T.fold (fun cri qsi acc -> 
			       T.add cri (qs_trim qsi) acc)
			     t.trans.(qi) T.empty
		       | _ -> ()
		   done;
		   let trim_init = qs_trim t.init in 
		   let trim_final = qs_trim t.final in 
		   let tt = { init = trim_init;
			      final = trim_final;
			      trans = trim_trans;
			      ambiguity = Amb_unknown;
			      det = Heur 0;
			      trim = Heur 0;
			      reverse = Heur 0;
			      star = Heur 0 } in
		     if not loged then incr_debug_trim dt_other (Array.length t.trans) (Array.length tt.trans);
		     tt.trim <- More tt;
		     if heur_trim t then t.trim <- More tt;
		     tt)
            
  let debuging_trim dt t = 
    if Prefs.read test_trim then begin
      let tt = trim ~loged:true t in
	incr_debug_trim dt (Array.length t.trans) (Array.length tt.trans);
	if Prefs.read use_trim then
	  tt
	else
	  t
    end
    else 
      t
	

  (* mk_cset: bool -> (RS.elt * RS.elt) list -> t
   * 
   * [mk_cset b s] is an automaton accepting the (negated) character
   * set [s] 
   *)
  let mk_cset b cl = 
    let q0 = 0 in 
    let q1 = 1 in 
    let sq0 = QS.singleton q0 in 
    let sq1 = QS.singleton q1 in
    let trans = array_make 2 T.empty in
    let m = 
      if b then 
        Safelist.fold_left 
          (fun m cr -> T.add cr sq1 m) 
          T.empty cl
      else
        let all = (RS.char_code_min,RS.char_code_max) in 
        Safelist.fold_left 
          (fun m cr -> T.rem cr m)
          (T.add all sq1 T.empty) cl in 
      trans.(0) <- m;
      debuging_trim dt_cset 
	{ init = sq0;
	  final = sq1;
	  trans = trans;
	  ambiguity = Amb_unambig;
	  det = Heur 0;
	  trim = Heur 0;
	  reverse = Heur 0;
	  star = Heur 0 }  
        
        
          
  (* mk_elt: RS.elt -> t
   * 
   * [mk_char c] is an automaton accepting 'c' 
   *)
  let mk_elt c = mk_cset true [(c,c)]

  (* mk_seq: t -> t -> t
   *  
   * [mk_seq t1 t2] is the automaton accepting the concatenation 
   * of [t1] and [t2] 
   *)    
  let mk_seq t1 t2 =
    let n1 = num_states t1 in
    let n2 = num_states t2 in 
    let seq_trans = 

      (* allocate array with concatenation of the transition relations
         of t1 and t2 *)
      let a = array_append t1.trans t2.trans in
        (* increment states mentioned in transitions in t2 *) 
        for si=n1 to n1+n2-1 do 
          a.(si) <- T.map (qs_add n1) a.(si) 
        done;

        (* calculate initial transitions from t2 *)
	let trans_init_t2 = QS.fold 
          (fun q acc -> T.union acc a.(q + n1)) 
          t2.init T.empty in

	  (* add initial transitions from t2 to transitions from final
	   * states in t1 
           *)
          QS.iter 
            (fun si -> a.(si) <- T.union a.(si) trans_init_t2)
            t1.final;
          a in 

    let seq_final = 
      (* if t2 recognizes epsilon, then t1's final states are final in t1.t2 *)
      let t2_final = qs_add n1 t2.final in 
        if not (QS.is_empty (QS.inter t2.init t2.final)) then 
          QS.union t1.final t2_final 
        else t2_final in
      debuging_trim dt_seq 
      { init = t1.init;
	trans = seq_trans;
        final = seq_final;
	ambiguity = Amb_unknown;
	det = Heur 0;
	trim = Heur 0;
	reverse = Heur 0;
	star = Heur 0 } 

  (* mk_str: RS.t -> t
   *  
   * [mk_str s] is the automaton accepting [s]. 
   *)
  let mk_str ignore_case s = 
    let n = RS.length s in 
    let a = array_make (succ n) T.empty in
    let othercase c = 
      let lc = RS.lowercase c in 
        if c = lc then RS.uppercase c else lc in         
      for i = 0 to (pred n) do
        let ci = RS.get s i in 
        let nexti = succ i in 
	  a.(i) <- T.single_trans ci nexti;
          if ignore_case then a.(i) <- 
            T.union a.(i) (T.single_trans (othercase ci) nexti)
      done;
      debuging_trim dt_str 
      { init = QS.singleton 0;
        final = QS.singleton n;
        trans = a;
	ambiguity = Amb_unambig;
	det = Heur 0;
	trim = Heur 0;
	reverse = Heur 0;
	star = Heur 0 }

  (* mk_alt: t -> t -> t
   *  
   * [mk_alt t1 t2] is the automaton accepting the union 
   * of [t1] and [t2] 
   *)    
  let mk_alt t1 t2 = 
    let n1 = num_states t1 in 
    let n2 = num_states t2 in 
    let alt_trans = 
      (* allocate array with concatenation of the transition relations
         of t1 and t2 *)
      let a = array_append t1.trans t2.trans in
        (* increment states mentioned in transitions in t2 *) 
        for si=n1 to n1+n2-1 do           
          a.(si) <- T.map (qs_add n1) a.(si) 
        done;
        a in 
    let alt_final = 
      let t2_final = qs_add n1 t2.final in 
        QS.union t1.final t2_final in
    let alt_init = 
      let t2_init = qs_add n1 t2.init in 
        QS.union t1.init t2_init in 
      debuging_trim dt_alt 
      { init = alt_init;
	trans = alt_trans;
        final = alt_final;
	ambiguity = Amb_unknown;
	det = Heur 0;
	trim = Heur 0;
	reverse = Heur 0;
	star = Heur 0 }

  (* mk_star:  t -> t
   *  
   * [mk_star t] is the automaton accepting 
   * [t]*.
   *)    
  let rec mk_star t = (*match i, jo with
    | 0, None ->*)  
    match t.star with
      | More ts -> ts
      | _ ->
          (let star_trans = 
	     let a = Array.copy t.trans in 

	     (* calculate initial transitions from t *)
	     let trans_init = QS.fold 
	       (fun q acc -> T.union acc a.(q)) 
	       t.init T.empty in

	       (* add initial transitions to transitions from final
		* states 
		*)
	       QS.iter (fun si -> a.(si) <- T.union trans_init a.(si)) t.final;
	       a in 
	   let ts =  debuging_trim dt_rep
	     { init = t.init;
	       trans = star_trans;
	       final = QS.union t.init t.final;
	       ambiguity = Amb_unknown;
	       det = Heur 0;
	       trim = Heur 0;
	       reverse = Heur 0;
	       star = Heur 0 } in
	     ts.star <- More ts;
	     if heur_star (t) then t.star <- More ts;
	     ts
	  ) 
(*    | i, None -> 
        if (i < 0) then 
          raise (Invalid_argument "Minimum number of repetitions must be positive");
        mk_seq t (mk_rep (i-1) None t)
    | 0, Some 0 -> epsilon
    | 0, Some 1 -> 
	 { init = t.init;
	   final = QS.union t.init t.final;
	   trans = Array.copy t.trans;
	   ambiguity = Amb_unknown;
	   det = Heur 0;
	   trim = Heur 0;
	   reverse = Heur 0;
	   star = Heur 0
	 }
    | 0, Some j ->
        let t' = mk_seq t (mk_rep 0 (Some (j - 1)) t) in
          reset_heur {t' with 
	              final = QS.union t'.init t'.final }
    | 1, Some 1 -> t
    | i, Some j ->
	if (i < 0 ) then 
          raise (Invalid_argument "Minimum number of repetitions must be positive");
	if (j < i) then 
          raise (Invalid_argument "Maximum number of repetition must be greater than the minimum repetitions.");
        mk_seq t (mk_rep (i -1) (Some (j - 1)) t)           
*)



  (* is_empty: t -> bool 
   * 
   * [is_empty t] returns [true] iff the language of [t] is empty
   *)
  let is_empty t = 
    let n = num_states t in 
    let a = array_make n true in 
    let queue = Queue.create () in 
      QS.iter (fun qi -> Queue.push qi queue) t.init;
      QBFS.go_filter
        (fun q -> a.(q))
        (fun q _ -> 
          a.(q) <- false;
          let b = not (QS.mem q t.final) in 
            (b,b))
        t
        true
        queue

  (* determinize: t -> t
   * 
   * [determinize t] constructs an automaton recognizing the language
   * of [t] whose transitions are all to singleton sets. the algorithm
   * used is essentially the subset construction, but constructed by
   * exploring the subsets reachable from the initial states, rather
   * than literally taking every subset of the states of [t].
   *)
  let determinize t =
    match t.det with 
      | More td -> td
      | _ ->
	  (* state *)
          let queue = Queue.create () in
          let assoc_cell = ref QSM.empty in 
          let n_cell = ref 0 in 
          (* helper functions *)
          let add_qs qs = 
            try QSM.find qs !assoc_cell
            with Not_found -> 
              let qi = !n_cell in 
                assoc_cell := QSM.add qs qi !assoc_cell;
                incr n_cell;
                Queue.add (qi,qs) queue;
                qi in 
          (* setup initial states *)
          let q0 = add_qs t.init in 
          (* loop over transition graph *)
          let qmap =
            IQSBFS.go_always_bare
              (fun (qi,qs) _ qmap ->   
                let qs_next = 
                  QS.fold (fun q' acc -> T.union t.trans.(q') acc) 
                    qs T.empty in
	        let qs_next' = 
		  T.fold (fun cr qs' qs_next' -> 
                    T.add cr (QS.singleton (add_qs qs')) qs_next')
                    qs_next T.empty in 
                  QM.add qi qs_next' qmap)
              QM.empty
              queue in
          (* main construction of dfa *)	    
          let n = !n_cell in 
          let qsm = !assoc_cell in 
          let a = array_make n T.empty in            
          let dfa_finals = QSM.fold 
            (fun qs qi acc -> 
              if QS.is_empty (QS.inter qs t.final) then acc 
              else QS.add qi acc)
            qsm QS.empty in
	    QM.iter (fun i ti -> a.(i) <- ti) qmap;
	    let td = 
              if QS.is_empty dfa_finals then empty else                 
                { init = QS.singleton q0;
		  final = dfa_finals;
		  trans = a;
		  ambiguity = Amb_unambig;
		  det = Heur 0;
		  trim = Heur 0;
		  reverse = Heur 0;
		  star = Heur 0} in
	      td.det <- More td;
	      if heur_det t then t.det <- More td;
	      debuging_trim dt_determinize td

  (* optional_determinize t -> t option
   * determinize the automation according to an heuristic
   *)
  let optional_determinize t = 
    if heur_det t then Some (determinize t) else None 

  let optional_determinize_non_incr t = 
    if heur_det_non_incr t then Some (determinize t) else None 
		

  (* mk_inter: t -> t -> t
   * 
   * [mk_inter t1 t2] returns the automaton accepting the intersection
   * of the languages of [t1] and [t2].
   * 
   *)
  let mk_inter t1 t2 =
    (* helper, used by mk_inter *)
    (* state *)
    let queue = Queue.create () in
    let assoc_cell = ref QPM.empty in 
    let n_cell = ref 0 in 
    (* helper functions *)
    let add_qp qp = 
      try QPM.find qp !assoc_cell
      with Not_found -> 
        let n = !n_cell in 
	  if n < 0 then  raise (Error.Harmony_error 
            (fun () -> Util.format "@[Failed to create a so big cross-product automaton@]"));
          assoc_cell := QPM.add qp n !assoc_cell;
          incr n_cell;
          Queue.add (n,qp) queue;
          n in 
    let product qs1 qs2 = 
      QS.fold (fun q1i acc1 -> 
        QS.fold (fun q2j qs12 ->
          let qij = add_qp (q1i,q2j) in 
            QS.add qij qs12)
          qs2 acc1)
        qs1 QS.empty in 
    (* setup initial states *)
    let t1t2_inits =
      QS.fold (fun q1 acc1 -> 
        QS.fold (fun q2 inits -> 
          let q12 = add_qp (q1,q2) in            
            QS.add q12 inits)
          t2.init acc1)
        t1.init QS.empty in
    (* loop over transition graph *)
    let qmap =
      QPBFS.go_always_bare
        (fun (q12,(q1,q2)) _ qmap ->   
          let trans = 
            T.fold (fun cr1 qs1 acc1 -> 
              T.fold (fun cr2 qs2 acc2 -> 
                match T.isect_range cr1 cr2 with
                  | None -> acc2 
                  | Some cr -> T.add cr (product qs1 qs2) acc2)
                t2.trans.(q2) acc1)
              t1.trans.(q1) T.empty in  
            QM.add q12 trans qmap)
        QM.empty queue in
    (* construct product automaton *)
    let n = !n_cell in 
    let qpm = !assoc_cell in 
    let a = array_make n T.empty in     
      QM.iter (fun i ti -> a.(i) <- ti) qmap;
      let t1t2_finals = QPM.fold 
        (fun (q1,q2) q12 acc -> 
          if QS.mem q1 t1.final && QS.mem q2 t2.final then 
            QS.add q12 acc
          else acc)
        qpm QS.empty in         
        if QS.is_empty t1t2_finals then empty else 
	debuging_trim dt_inter 
         { init = t1t2_inits;
	    final = t1t2_finals;
	    trans = a;
	    ambiguity = Amb_unknown;
	    det = Heur 0;
	    trim = Heur 0;
	    reverse = Heur 0;
	    star = Heur 0 } 
          
  (* mk_complement: t -> t
   * 
   * [mk_complement t] is the automaton accepting the complement of the
   * language accepted by [t]. the resulting nfa is actually
   * deterministic. 
   *)
  let mk_complement t =     
    let t' = determinize t in
    let tr = t'.trans in
    let n = Array.length tr in      
    (* add a new state to handle transitions that were omitted in t;
     * in the complement, these need to go to a (fresh) final state sn *)      
    let a = array_make (succ n) T.empty in
    let sn = QS.singleton n in
      (* add transitions for every char range not in the map to sn *)
      for i = 0 to (pred n) do
	a.(i) <- T.fill_holes sn tr.(i)
      done;
      (* add a transition from sn to itself for every char *)
      a.(n) <- T.add (RS.char_code_min, RS.char_code_max) sn T.empty;
      
      (* compute complemenet of t's final states *)
      let rec loop qs i = 
	if i = (succ n)  then qs
	else if QS.mem i t'.final then loop qs (succ i)
	else loop (QS.add i qs) (succ i) in
      let init' = if QS.is_empty t'.init then sn else t'.init in
      let final' =  loop QS.empty 0 in
      debuging_trim dt_complement
	{ final = final';
	  init = init';
	  trans = a;
	  ambiguity = Amb_unknown;
	  det = Heur 0;
	  trim = Heur 0;
	  reverse = Heur 0;
	  star = Heur 0 }

  (* mk_diff: t -> t
   * 
   * [mk_diff t1 t2] is the automaton accepting the relative complement of 
   * [t2] with respect to [t1]. *)
  let mk_diff t1 t2 =
    let t2' = mk_complement t2 in
    let it =
      debuging_trim dt_diff 
	(mk_inter t1 t2')in
    trim it


  let equiv t1 t2 = 
       (is_empty (mk_diff t1 t2))
    && (is_empty (mk_diff t2 t1))

  (* representative: t -> RS.t
   * 
   * [representative t] returns a (shortest) string accepted by the 
   * automaton 
   *)
  let representative t =     
    let n = num_states t in 
    let a = array_make n (true,-1,RS.sym_of_int 0) in 
    let queue = Queue.create () in 
    QS.iter (fun qi -> 
      Queue.push qi queue;
      a.(qi) <- (false,-1,RS.sym_of_int 0))
      t.init;
    let qo = 
      QBFS.go_bare
        (fun q pool _ -> 
          if QS.mem q t.final then 
            (* if we have a final state, return q *)
            (Some q, false)
          else
            (* otherwise, iterate over the transitions whose source is
             * q and for each qi not yet explored, add an entry in a
             * pointing back to q, labeled by a character in the
             * transition.  
             *)
            (T.iter 
                (fun (c,_) qs -> 
                  QS.iter (fun qi -> 
                    match a.(qi) with 
                      | (true,_,_) -> 
                          a.(qi) <- (false,q,c);
                          QBFS.add qi pool
                      | _ -> ())
                    qs)
                t.trans.(q);
             None, true))
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
            let rec build_rep q acc = 
              if QS.mem q t.init then acc
              else 
                begin 
                  let _,q',c = a.(q) in                     
                    build_rep q' ((RS.make 1 c) ^ acc) 
                end in 
              build_rep q RS.empty

  (* module for traversing the transition graph *)
  module OldGraphTraverse(S:sig
    type 'a t
    val is_empty: QS.elt t -> bool
    val push: QS.elt -> QS.elt t -> unit
    val pop: QS.elt t -> QS.elt
    end) = struct
      (* go: 
         next: (QS.elt -> QS.t) -> 
         f: (QS.elt -> 'a -> 'a) -> 
         init_acc : 'a -> 
         mark : (QS.elt -> 'b -> 'b) ->
         fold: ((QS.elt -> 'b -> 'b) -> QS.t -> 'b -> 'b) -> 
         is_unmarked: (QS.elt -> 'b -> bool) ->
         marked : 'b ->
         queue: S.t -> 
         'a 
      *)  
    let go next f init_acc fold mark is_unmarked marked queue = 
      let rec loop acc marked = 
        if S.is_empty queue then acc 
        else
          let q = S.pop queue in
          let acc',continue = f q acc in 
            if not continue then acc'
            else
              let marked' = fold 
                (fun qi marked' -> 
                  if is_unmarked qi marked' then 
                    begin 
                      S.push qi queue;
                      mark qi marked'
                    end
                  else 
                    marked')
                (next q) marked in 
                loop acc' marked' in
        loop init_acc marked 
  end
  module BFS = OldGraphTraverse(Queue)

  let str_of_list l = 
    let n = Safelist.length l in
    let a = RS.make n (RS.sym_of_int 0) in
    let rec loop i = function
      | [] -> a
      | c::rest ->
	  (RS.set a i c;
	   loop (succ i) rest) in
      loop 0 l
    
  (* Some extra operations on integers from the Num lib. Modified to be tail rec *)
	
  let rec num_bits_int_aux n acc =
    if n = 0 then acc else num_bits_int_aux (n lsr 1) (succ acc)
      
  let num_bits_int n = num_bits_int_aux (abs n) 0
    
  let length_of_int = Sys.word_size - 2
    
  let ambiguous_word t = 
    match t.ambiguity with
      | Amb_unambig -> None
      | Amb_ambig s -> Some s
      | Amb_unknown ->
    ((*let n = num_states t in*)
    (* helper functions *)
    (* cross_states: QS.elt -> QS.elt -> QS.elt
    * 
    *   [cross_states q1 q2] computes a state [q12] representing
    *     [(q1,q2)] in the square of [t].  it returns a state in 
    *     the upper triangle of the transition matrix.  
    *)
    let rec cross_states q1 q2 = 
      if q1 > q2 then 
	cross_states q2 q1
      else (q1,q2)
(*begin
	let to_big = Error.Harmony_error 
	  (fun () -> Util.format "@[Failed to type check a so big automaton@]") in
	(* computation of q1 * n + q2, with checking for overflow *)
	if num_bits_int n + num_bits_int q1 < length_of_int then begin
	  let p = q1 * n in
	  let s = p + q2 in
	    (* test for overflow taken from the Num lib...*)
	    if (p lxor q2) lor (p lxor (s lxor (-1))) < 0 then
	      s
	    else
	      raise to_big
	end else
	  raise to_big
      end *)in
    (* uncross_states: QS.elt -> QS.elt * QS.elt
     * 
     *   [uncross_states q12] is the inverse of [cross_states] and
     *     computes a pair [(q1,q2)].  
     *)
    let uncross_states q12 = 
      let q1 = fst q12 (*q12 / n*) in
      let q2 = snd q12 (*q12 mod n*) in
        if q1 <= q2 then (q1, q2) else (q2, q1) in 
    (* qs_product QS.t -> QS.t -> QS.t 
     * 
     *   [qs_product s1 s2] lifts [cross_states] to sets of states.
     *)
    let qs_product s1 s2 = 
      QS.fold (fun qi acci -> 
        QS.fold (fun qj accj -> 
	  if qi <= qj then 
            QPS.add (cross_states qi qj) accj
	  else accj)
	  s2 acci)
        s1 QPS.empty in
      
    (* PHASE I: 
     *   calculate accessible states (qi,qj) in square of t,
     *   and a word (represented as a list of chars) that reaches
     *   (qi,qj). represented as a (char list) QM.t
     *)
    let qp_queue = Queue.create () in 
    let init_map = 
      QS.fold (fun qi acci ->
	QS.fold (fun qj accj ->
	  if qi <= qj then 
	    begin
	      let qij = cross_states qi qj in
		Queue.push qij qp_queue;
		QPM.add qij [] accj
            end
          else accj)
	  t.init acci)
        t.init QPM.empty in
    let acc_map = 
      QPairBFS.go_always_bare 
        (fun qij pool acc -> 
          let lij = QPM.find qij acc in 
          let qi,qj = uncross_states qij in 
          let qij_trans = T.ext_product qs_product t.trans.(qi) t.trans.(qj) QPS.union in 
            T.fold (fun (c1,_) qs acct -> 
              let lij' = c1::lij in 
	        QPS.fold
		  (fun qkl acc ->
		    if QPM.mem qkl acc then acc
		    else begin 
                      QPairBFS.add qkl pool;
                      QPM.add qkl lij' acc
                    end)
		  qs acct)
	      qij_trans acc)
        init_map
        qp_queue in
   (* PHASE II: look for path from pair of initial states to pair of
    * final states that is not entirely on the diagonal.
    *)
    let queue = Queue.create () in
    let t_rev = mk_reverse t in
    (* check an easy case and push t_rev's init states onto queue *) 
    let rev_init_alt = QS.fold (fun qi alt -> Misc.map_right alt (fun acc -> 
      Queue.push qi queue; (* only push qi if accessible? *)
      let alt' = Misc.Right (QM.add qi [] acc) in 
        QS.fold (fun qj alt -> Misc.map_right alt (fun _ -> 
          let qij = cross_states qi qj in 
            (* easy case: accessible final states (qi,qj) with qi<>qj *)                  
            if qi < qj && QPM.mem qij acc_map then 
              let lij = QPM.find qij acc_map in                 
                Misc.Left (Safelist.rev lij)
            else alt))
          t_rev.init alt'))
      t_rev.init (Misc.Right QM.empty) in      
     
    (* if not in the easy case, then search along diagonal for
     * incoming transitions from (qi,qj) with qi<>qj 
     *)
    let alt = Misc.map_right rev_init_alt (fun _ ->         
      QBFS.go_always_bare 
        (fun qd pool alt -> Misc.map_right alt (fun acc ->
          let lqd = QM.find qd acc in 
            T.fold (fun (c1,_) qs alt -> Misc.map_right alt (fun acc ->
              let lqd' = c1::lqd in
                QS.fold (fun qi alt -> Misc.map_right alt (fun acc -> 
                  let acc' = 
                    if not (QM.mem qi acc) then 
                      begin 
                        QBFS.add qi pool;
                        QM.add qi lqd' acc
                      end
                    else acc in
                    QS.fold (fun qj alt -> Misc.map_right alt (fun _ -> 
                      let qij = cross_states qi qj in 
                        if qi < qj && QPM.mem qij acc_map then                             
                          let l1 = QPM.find qij acc_map in 
                            Misc.Left (Safelist.rev l1 @ lqd')
                        else alt))
                    qs (Misc.Right acc')))
                  qs alt))
              t_rev.trans.(qd) alt))
          rev_init_alt
          queue)in 
      match alt with 
        | Misc.Left l -> 
	    let s = str_of_list l in
	      t.ambiguity <- Amb_ambig s;
	      Some (str_of_list l)
        | Misc.Right r -> 
	    t.ambiguity <- Amb_unambig;
	    None)
	    

  let unambig_star t = 
    if not (QS.is_empty (QS.inter t.init t.final)) then Misc.Left (RS.empty, -1, true) else
      (let tt = trim t in
       let dt = determinize tt in
       let n = Array.length dt.trans in
       let a = array_make n T.empty in
	 for i = 0 to n - 1 do
	   a.(i) <- 
	     (T.fold
		(fun cr qs acc -> 
		   if QS.is_empty (QS.inter qs dt.final) then
		     T.add cr qs acc
		   else T.add cr (QS.union qs dt.init) acc)
		dt.trans.(i) T.empty)
	 done;
	 let tp = { init = dt.init; (* recognize L+ *)
		    final = dt.final;
		    trans = Array.copy a;
		    ambiguity = Amb_unknown;
		    det = Heur 0;
		    trim = Heur 0;
		    reverse = Heur 0;
		    star = Heur 0} in
	 let tq = { init = QS.singleton n; 
		    final = tp.final;
		    trans = Array.append tp.trans (array_make 1 T.empty);
		    ambiguity = Amb_unknown;
		    det = Heur 0;
		    trim = Heur 0;
		    reverse = Heur 0;
		    star = Heur 0} in
	 let res = 
	   QS.fold 
	     (fun q acc ->
		if acc = None then
		  (tq.trans.(n) <- tq.trans.(q);
		   ignore (reset_heur tq);
		   let inter = trim (mk_inter tq tp) in
		     if Array.length inter.trans = 0 then None else Some (inter, q))
		else acc)
	     tq.final None in
	   match res with
	     | None -> 
		 let ts = mk_star dt in
		   define_to_be_unambig ts;
		   Misc.Right ts
	     | Some (inter, q) ->
		 let t' = reset_heur {dt with final = QS.singleton q} in
		   try 
		     let beg_word = representative t' in
		     let end_word = representative inter in
		       Misc.Left (beg_word^end_word, RS.length beg_word, false)
		   with
		     | Not_found -> assert false)
		

  module Matching (Front:
    sig 
      type frontier
      val init : t -> frontier
      val next : t -> frontier -> RS.sym -> frontier
      val is_empty : frontier -> bool
      val is_final : t -> frontier -> bool
    end) = 
  struct
    include Front
    (*let init = Front.init
    let next = Front.next
    let is_empty = Front.is_empty
    let is_final = Front.is_final*)
    let match_str t s =
      let n = RS.length s in
      let rec loop frontier i =
	if i=n then is_final t frontier
	else if is_empty frontier then false
	else
          let frontier' = next t frontier (RS.get s i) in
            loop frontier' (succ i) in
	loop (init t) 0

    let match_prefix t s =
      let n = RS.length s in
      let rec loop frontier acc i =
	if is_empty frontier then (acc, false) else
          let acc' = 
            if is_final t frontier then
              IS.add i acc 
            else acc in
            if i = n then 
              (acc', true) 
            else
              loop (next t frontier (RS.get s i)) acc' (succ i) in
	fst (loop (init t) IS.empty 0)	     

    let match_reverse_prefix t s = 
      let n = RS.length s in
      let rec loop frontier acc i =
	if is_empty frontier then (acc, false) else
          let acc' = if is_final t frontier then
            IS.add i acc else acc in
            if i = 0 then (acc', true) else
              let frontier' = next t frontier (RS.get s (i - 1)) in
		loop frontier' acc' (i - 1) in
	loop (init t) IS.empty n


    (* find_exit_automaton does almost the same as match_prefix,
       but if the path exists the automaton, the exit point is
       added to the set, and the boolean is set to true. *)
    let find_exit_automaton t s = 
      let n = RS.length s in
      let rec loop frontier acc i =
	if is_empty frontier then (IS.add i acc, true) else
          let acc' = 
            if is_final t frontier then 
              IS.add i acc 
            else acc in
            if i = n then 
              (acc', false) 
            else
              loop (next t frontier (RS.get s i)) acc' (succ i) in
	loop (init t) IS.empty 0

    let partial_unambig_star_split t s is init_acc add_acc =
      let n = RS.length s in
      if not (IS.mem 0 is) then init_acc else
	(let rec loop i f acc = 
	   let acc', f' = if (is_final t f) && (IS.mem i is)then (add_acc acc i, init t) else (acc, f) in
	     if i = n then acc' else
	       loop (i + 1) (next t f' (RS.get s i)) acc' in
	 loop 0 (init t) init_acc)
	  

  end

  module Matching_nfa = Matching(
    struct 
      type frontier = QS.t
      let init t = t.init
      let next = next
      let is_empty = QS.is_empty
      let is_final t frontier = not (QS.is_empty (QS.inter frontier t.final))
    end)

  module Matching_dfa = Matching(
    struct
      type frontier = int option
      let init t =
	let i = t.init in
	  if QS.is_empty i then None 
  	  else begin
	    (*XXXXXX Debugging version XXXXXXXXX*)
	    let r = QS.choose i in
	      assert (QS.is_empty (QS.remove r i));
	      Some r
	  end
      let next t frontier c = match frontier with
	| None -> None
	| Some q ->
	    let tr = t.trans.(q) in
	      try 
		let s = T.find_elt c tr in
		if QS.is_empty s then None
		else begin
		  (*XXXXXX Debugging version XXXXXXXXX*)
		  let r = QS.choose s in
		    assert (QS.is_empty (QS.remove r s));
		    Some r
		end
	      with 
		  Not_found -> None

      let is_empty = function
	| None -> true
	| Some _ -> false

      let is_final t = function
	| None -> false
	| Some q -> QS.mem q t.final
    end)

  let mk_unambig t = 
    match t.ambiguity with
      | Amb_unambig -> t
      | _ -> determinize t


  (*** easy splitability ****)


  (* two trimed automata wich are unambiguously concatenable are said
   * to be easily slitable if after having read a word recognized by
   * t1, if we try to continue in t1 by reading a word recognized by
   * t2, we go out of t1 before the end of this word. A way to check
   * that is by testing if there is a common word in t1' and t2, where
   * t1' is t1 with t1.final as initial states, and all the states as
   * final states *)
  let easily_splitable t1 t2 = 
    let t1 = determinize (trim t1) in
    let n = num_states t1 in
    let p = n + 1 in 
    let trans' = Array.make (p + 1) T.empty in
      (* all transition in the initial automaton are in the new one*)
    for i = 0 to n -1 do
      trans'.(i) <- t1.trans.(i)
    done;
      (* n has all the transitions from any final state *)
    trans'.(n) <- QS.fold (fun f acc -> T.union acc t1.trans.(f)) t1.final T.empty;
    let ps = QS.singleton p in
    (* p is looping on itself by reading any char *)
    trans'.(p) <- T.add (RS.char_code_min, RS.char_code_max) ps T.empty;
    (* from every final state, can go to p by reading any char *)
    QS.iter (fun f -> trans'.(f) <- T.add (RS.char_code_min, RS.char_code_max) ps trans'.(f)) t1.final;
    let init' = QS.singleton n in
    let rec loop i acc = 
      if i < 0 then acc 
      else loop (i - 1) (QS.add i acc) in
    let final' = loop (n - 1) ps in
    let t1' = 
      { init = init';
	final = final';
	trans = trans';
	ambiguity = Amb_unknown;
	det = Heur 0;
	trim = Heur 0;
	reverse = Heur 0;
	star = Heur 0;} in
      is_empty (mk_inter t1' t2)
      
      
end


module N = NFA
type t = N.t
let format = N.format

let init = N.init
let next = N.next
let is_empty = N.is_empty

let mk_elt = N.mk_elt
let mk_str = N.mk_str
let mk_alt = N.mk_alt
let mk_seq = N.mk_seq
let mk_cset = N.mk_cset
let mk_star = N.mk_star
let mk_inter = N.mk_inter
let mk_complement = N.mk_complement
let mk_diff = N.mk_diff
let mk_reverse = N.mk_reverse
let mk_lowercase = N.mk_lowercase
let mk_uppercase = N.mk_uppercase
let extend = N.extend
let ambiguous_word = N.ambiguous_word
let determinize = N.determinize
let trim = N.trim ~loged:false
let mk_unambig = N.mk_unambig
let define_to_be_unambig = N.define_to_be_unambig

(* operations *)

let representative = N.representative
let match_str t s = match N.optional_determinize_non_incr t with
  | None -> N.Matching_nfa.match_str t s
  | Some dt -> N.Matching_dfa.match_str dt s

let match_prefix t s = match N.optional_determinize_non_incr t with
  | None -> N.Matching_nfa.match_prefix t s
  | Some dt -> N.Matching_dfa.match_prefix dt s
 
let match_reverse_prefix t s = match N.optional_determinize_non_incr t with
  | None -> N.Matching_nfa.match_reverse_prefix t s
  | Some dt -> N.Matching_dfa.match_reverse_prefix dt s

let find_exit_automaton t s = match N.optional_determinize_non_incr t with
  | None -> N.Matching_nfa.find_exit_automaton t s
  | Some dt -> N.Matching_dfa.find_exit_automaton dt s
 

type dual_single_split = { dss_example : RS.t;
			   dss_cut1 : int;
			   dss_cut2 : int}

let example_of_dss dss = 
  let i1 = dss.dss_cut1 in 
  let i2 = dss.dss_cut2 in 
  let s = dss.dss_example in 
  let n = RS.length s in 
    ((RS.sub s 0 i1, RS.sub s i1 (n-i1)),
    (RS.sub s 0 i2, RS.sub s i2 (n-i2)))

type not_ambig = NA_true of N.t | NA_false of dual_single_split

let is_ambiguous t = match N.ambiguous_word t with
    None -> (true,RS.empty)
  | Some s -> (false,s)

let unambig_seq t1 t2 = 
  let t1' = mk_unambig t1 in
  let t2' = mk_unambig t2 in
  let t = mk_seq t1' t2' in
  let res, s = is_ambiguous t in
    if res then NA_true t else
      (let is1 = match_prefix t1' s in 
       let t2r = mk_reverse t2' in
       let is2, _ = match_reverse_prefix t2r s in
       let is = IS.inter is1 is2 in
	 assert (not (IS.is_empty is));
	 let n1 = IS.choose is in
	 let is' = IS.remove n1 is in
	   assert (not (IS.is_empty is'));
	   let n2 = IS.choose is' in
	     NA_false { dss_example =s;
			dss_cut1 = n1;
			dss_cut2 = n2})

type dual_multi_split = { dms_example : RS.t;
			  dms_cut1 : int list;
			  dms_cut2 : int list}

type not_star_ambig = NSA_true of t| NSA_empty_word | NSA_false | NSA_false_ce of dual_multi_split


let example_of_dms dms =
  let s = dms.dms_example in 
  let _,s1 = Safelist.fold_left 
    (fun (i0,acc) i1 -> 
      (i1, 
      Safelist.fold_left RS.append 
        RS.empty 
        [acc; 
         RS.t_of_string "[";
         RS.sub s i0 (i1-i0);
         RS.t_of_string "]"]))    
    (0,RS.empty) dms.dms_cut1 in 
  let _,s2 = Safelist.fold_left 
    (fun (i0,acc) i1 -> 
      (i1, 
       Safelist.fold_left RS.append 
         RS.empty
         [acc; 
          RS.t_of_string "[";
          RS.sub s i0 (i1-i0);
          RS.t_of_string "]"]))    
    (0,RS.empty) dms.dms_cut2 in 
    RS.append (RS.t_of_string "[") 
      (RS.append s (RS.append (RS.t_of_string "]\nSPLITS INTO\n") 
                       (RS.append s1 
                           (RS.append (RS.t_of_string "\nAND\n") s2))))

let rec unambig_star t = 
  match N.unambig_star t with
    | Misc.Right ts -> 
	NSA_true ts
    | Misc.Left (_, _, true) -> NSA_empty_word
    | Misc.Left (s, p, false) ->
	(let n = RS.length s in
	 let rev = mk_reverse (mk_star t) in
	 let is, _ = match_reverse_prefix rev s in
	   assert (IS.mem 0 is);
	   let rec loop i front can_split acc = 
	     let acc', front', cs' = 
	       if i < p then (acc, front, can_split) 
	       else if i = p then 
		 (assert ((not (N.QS.is_empty (N.QS.inter (N.final t) front))) && (IS.mem i is));
		  if can_split then (i :: acc, init t, true) 
		  else (acc, front, true))
	       else if (* i > p &&*)((not (N.QS.is_empty (N.QS.inter (N.final t) front)))) && (IS.mem i is ) then (i :: acc, init t, true)
	       else (acc, front, true) in
	       if i = n then  Safelist.rev acc' else
		 loop (i + 1) (next t front' (RS.get s i)) cs' acc' in
	   let cl1 = loop 0 (init t) true [] in
	   let cl2 = loop 0 (init t) false [] in
	     NSA_false_ce { dms_example = s;
			    dms_cut1 = cl1;
			    dms_cut2 = cl2;
			  })


let equiv = N.equiv
  
let empty = N.empty
let epsilon = N.epsilon
    
let split_positions t1 t2 s = 
  let is = match_prefix t1 s in
  let is', _ = match_reverse_prefix (mk_reverse t2) s in
    IS.inter is is'

let unambig_split t1 t2 s = 
  let inter = split_positions t1 t2 s in 
    if IS.is_empty inter then None
    else 
      let i = IS.choose inter in 
        assert (IS.is_empty (IS.remove i inter));
        Some (RS.sub s 0 i,
             RS.sub s i ((RS.length s) - i))

let unambig_star_split_ints t s = 
  let rev = mk_reverse (mk_star t) in
  let is, _ = match_reverse_prefix rev s in
  let init_acc = [] in
  let add_acc acc i = i :: acc in
    match N.optional_determinize t with
      | None -> Safelist.rev (N.Matching_nfa.partial_unambig_star_split t s is init_acc add_acc)
      | Some dt -> Safelist.rev (N.Matching_dfa.partial_unambig_star_split dt s is init_acc add_acc)

let count_unambig_star_split t s = 
  let rev = mk_reverse (mk_star t) in
  let is, _ = match_reverse_prefix rev s in
  let init_acc = 0 in
  let add_acc acc i = succ acc in
    match N.optional_determinize t with
      | None -> N.Matching_nfa.partial_unambig_star_split t s is init_acc add_acc
      | Some dt -> N.Matching_dfa.partial_unambig_star_split dt s is init_acc add_acc

let unambig_star_split t s = 
  Safelist.rev 
    (snd (Safelist.fold_left 
             (fun (j,acc) i -> (i,RS.sub s j (i-j)::acc))
             (0,[])
             (unambig_star_split_ints t s)))


let print_multi_split lst s = 
  let n = RS.length s in
  let rec loop i fst = function 
    | [] -> 
	(if not fst then print_char '^';
	 Printf.printf "\"%s\"" (RS.string_of_t (RS.sub s i (n - i))))
    | j :: tl -> 
	(if not fst then print_char '^';
	 if j < n  then 
	   (Printf.printf "\"%s\"" (RS.string_of_t(RS.sub s i (j - i)));
	    loop j false tl)
	 else Printf.printf "\"%s\"" (RS.string_of_t(RS.sub s i (n - i)))) in
    loop 0 true lst

let print_split i s = print_multi_split [i] s

(***** Easy splitability *****)

let easy_seq t1 t2 = 
  N.easily_splitable t1 t2 

let easy_star t1 = 
  (not (match_str t1 Bstring.empty) && N.easily_splitable t1 t1)
