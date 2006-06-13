(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* presburger.ml - presburger arithmetic                 *)
(*********************************************************)
(* $Id$ *)

let sprintf = Printf.sprintf

module IntMapplus =
  Mapplus.Make(
    struct
      type t = int
      let compare = compare
      let to_string = string_of_int
    end)
module IntMap = IntMapplus.Map
module IntSet = IntMapplus.KeySet

(* -------------- abstract syntax ------------- *)
(* exps are now only used for easy construction of EqZs and GeqZs. *)
type exp = 
    Const of int
  | Var of int 
  | Sum of exp * exp 

(* formulas, represented in de Bruijn notation *)
type t = 
    EqZ of (int * int) list * int
  | GeqZ of (int * int) list * int
  | Not of t
  | Or of t list
  | And of t list
  | Exists of t

(* --------------- utility functions --------------- *)
let rec fvs_t = function
    EqZ(ps_xs,_) | GeqZ(ps_xs,_) -> 
      Safelist.fold_left 
        (fun u (_,xi) -> IntSet.add xi u) 
        IntSet.empty
        ps_xs

  | Not(f)    -> fvs_t f

  | Or(fs) | And(fs) -> 
      Safelist.fold_left 
        (fun vs fi -> IntSet.union (fvs_t fi) vs)
        IntSet.empty        
        fs

  | Exists(f)    -> 
      IntSet.fold 
        (fun x s -> if (x <> 0) then (IntSet.add (x-1) s) else s) 
        (fvs_t f)
        IntSet.empty 

(* -- formatter -- *)
let rec fformat_exp_aux fmtr = function
    Const(n) -> Format.fprintf fmtr "%d" n
  | Var(x) -> Format.fprintf fmtr "n%d" x
  | Sum(e1,e2) -> 
      fformat_exp_aux fmtr e1; 
      Format.fprintf fmtr "+";
      fformat_exp_aux fmtr e2

let format_exp = fformat_exp_aux Format.std_formatter

let rec fformat_t_aux fmtr g t0 = 
  let format_atom ps_xs opr c = 
    if ps_xs = [] then Format.printf "0"
    else
      Misc.fformat_list fmtr "+"
        (fun (pi,xi) -> 
           Format.printf "%s%s"
             (if pi=1 then "" else if pi=(-1) then "-" else (sprintf "%d*" pi))
             ((snd g) xi))
        ps_xs;
    if c=0 then () else Format.printf "+%d" c;
    Format.printf "%s0" opr in
    match t0 with
        EqZ(ps_xs,c) -> format_atom ps_xs "=" c
      | GeqZ(ps_xs,c) -> format_atom ps_xs ">=" c
      | Not(f1) -> 
          Format.fprintf fmtr "!("; 
          fformat_t_aux fmtr g f1;
          Format.fprintf fmtr ")"
      | Or(fs) ->
          Format.fprintf fmtr "(";
          Misc.fformat_list fmtr  " | " (fformat_t_aux fmtr g) fs;
          Format.fprintf fmtr ")";
      | And(fs) ->
          Format.fprintf fmtr "(";
          Misc.fformat_list fmtr " & " (fformat_t_aux fmtr g) fs;
          Format.fprintf fmtr ")"
      | Exists(f1) -> 
          let next (qd,g) = (qd+1,fun n->if n=0 then sprintf "x%d" qd else g (n-1)) in
          let rec format_exs g = function
              Exists(f1) -> 
                Format.fprintf fmtr ",x%d" (fst g);
                format_exs (next g) f1
            | f -> 
                Format.fprintf fmtr ".";
                fformat_t_aux fmtr g f in
            Format.printf "EX x%d" (fst g);
            format_exs (next g) f1

let fformat_t fmtr f = 
  fformat_t_aux fmtr (0,fun n -> sprintf "n%d" n) f

let format_t = fformat_t Format.std_formatter

(* --------------- de Bruijn shifting -------------- *)
(* cutoff *)
let rec shift_exp_aux n c e0 = match e0 with
    Const(n)   -> e0
  | Var(x)     -> if x < c then e0 else Var(x+n)
  | Sum(e1,e2) -> Sum(shift_exp_aux n c e1, shift_exp_aux n c e2)      

let rec shift_aux n c = 
  let shift_snd = Safelist.map (fun (p,x)->(p,if x<c then x else x+n)) in
    function 
        EqZ(ps_xs,c)  -> EqZ(shift_snd ps_xs, c)
      | GeqZ(ps_xs,c) -> GeqZ(shift_snd ps_xs, c)
      | Not(f)        -> Not(shift_aux n c f)
      | Or(fs)        -> Or(Safelist.map (shift_aux n c) fs)
      | And(fs)       -> And(Safelist.map (shift_aux n c) fs)
      | Exists(f)     -> Exists(shift_aux n (c+1) f)

(* plain shifting *)
let shift_exp n = shift_exp_aux n 0
let shift_t n = shift_aux n 0

(** substitute: -> list -> (int * exp list)  -> t -> t *)
let rec substitute_exp es e0 = match e0 with 
    Const(_) -> e0
  | Var(x) -> (try Safelist.assoc x es with Not_found -> e0)
  | Sum(e1,e2) -> Sum(substitute_exp es e1,substitute_exp es e2)

let rec substitute es t0 = 
  let rec mult (vacc,cacc) pi e0 = match e0 with
      Const(n) -> (vacc, pi*n + cacc)
    | Var(x)   -> ((pi,x)::vacc, cacc)
    | Sum(e1,e2) -> mult (mult (vacc,cacc) pi e1) pi e2 in        
  let substitute_ps_xs ps_xs c = Safelist.fold_left
    (fun acc (pi,xi) ->
       (try mult acc pi (Safelist.assoc xi es)
        with Not_found -> let ps_xs,c = acc in ((pi,xi)::ps_xs,c)))
    ([],c)
    ps_xs in 
  let res = match t0 with
        EqZ(ps_xs,c) -> 
          let ps_xs',c' = substitute_ps_xs ps_xs c in
            EqZ(ps_xs',c')
      | GeqZ(ps_xs,c) -> 
          let ps_xs',c' = substitute_ps_xs ps_xs c in
            GeqZ(ps_xs',c')
      | Not(f)       -> Not(substitute es f)
      | Or(fs)       -> Or(Safelist.map (substitute es) fs)
      | And(fs)      -> And(Safelist.map (substitute es) fs)
      | Exists(f)    ->
          let shifted_es = Safelist.map 
            (fun (x,e) -> (x+1, shift_exp 1 e)) es in
            Exists(substitute shifted_es f)
  in
(*     Format.print_newline (); *)
(*     Format.printf "SUBSTITUTE"; Format.print_newline(); *)
(*     Format.printf " es=["; begin *)
(*       Misc.format_list ","  *)
(*         (fun (x,e) -> Format.printf "n%d->" x; format_exp e)  *)
(*         es;  *)
(*       Format.printf "]"; *)
(*       Format.print_newline() end; *)
(*     Format.printf " t0="; format_t t0; Format.print_newline(); *)
(*     Format.printf "res="; format_t res; Format.print_newline(); *)
    res

(* --------------- constants --------------- *)

let zero = Const(0)

let one = Const(1)

let tru = EqZ([],0)

let fls = EqZ([],1)

(* --------------- constructors --------------- *)

(** mkConst: int -> exp *)
let mkConst n = Const(n)

(** mkVar: int -> exp *)
let mkVar x = Var(x)

(** mkSum: exp -> exp -> exp *)
let mkSum e1 e2 = match e1,e2 with
    Const(0),_ -> e2
  | _,Const(0) -> e1
  | _ -> Sum(e1,e2)

(** mkNot: t -> t *)
let mkNot f = Not(f)

(** mkOr  t -> t list -> t *)
let mkOr fs = 
  if fs = [] then fls
  else
    let fs' = Safelist.fold_left 
      (fun fs fi -> 
         (match fi with
              Or(gs) -> gs @ fs
            | _         -> fi::fs))
      [] fs in
      Or(Safelist.rev fs')

(** mkAnd  t -> t list -> t *)
let mkAnd fs = 
  if fs = [] then tru
  else
    let fs' = Safelist.fold_left 
      (fun fs fi -> 
         match fi with
             And(gs) -> gs @ fs
           | _         -> fi::fs)
      []
      fs in
      And(Safelist.rev fs')

(** mkExists: t -> t *)
let mkExists f = Exists(f)

(* helper *)
let rec mk_cnstr (vacc,cacc) polarity = function
    Const(c)   -> (vacc,(polarity * c) + cacc)
  | Var(x)     -> ((polarity,x)::vacc,cacc)
  | Sum(e1,e2) -> mk_cnstr (mk_cnstr (vacc,cacc) polarity e1) polarity e2

let mkEq e1 e2 = 
  let ps_xs,c = mk_cnstr (mk_cnstr ([],0) 1 e1) (-1) e2 in 
    EqZ(ps_xs,c)

let mkLt e1 e2 = 
  let ps_xs,c = mk_cnstr (mk_cnstr ([],0) 1 e2) (-1) e1 in 
    GeqZ(ps_xs,c-1)

(** mkLe: exp -> exp -> t *)
let mkLe e1 e2 = 
  let ps_xs,c = mk_cnstr (mk_cnstr ([],0) 1 e2) (-1) e1 in 
    GeqZ(ps_xs,c)

(** mkGt: exp -> exp -> t *)
let mkGt e1 e2 = 
  let ps_xs,c = mk_cnstr (mk_cnstr ([],0) 1 e1) (-1) e2 in
    GeqZ (ps_xs,c-1)

(** mkGe: exp -> exp -> t *)
let mkGe e1 e2 = 
  let ps_xs,c = mk_cnstr (mk_cnstr ([],0) 1 e1) (-1) e2 in
    GeqZ (ps_xs,c)

let rec get_zeros f0 = match f0 with
    EqZ(ps_xs,c) -> 
      if c = 0 then 
        let _,zs = Safelist.fold_left 
          (fun (ws,zs) (p,x) -> 
             let wx = (IntMap.safe_find x ws 0) + p in
               (IntMap.add x wx ws, if wx=0 then zs else IntSet.add x zs))
          (IntMap.empty, IntSet.empty)
          ps_xs in 
          zs
      else IntSet.empty
  | GeqZ(_) -> IntSet.empty
  | Not(f) -> begin match f with
        Not(g) -> get_zeros g
      | _        -> IntSet.empty
    end
  | Or(fs) ->
      begin match Safelist.map get_zeros fs with
          [] -> IntSet.empty
        | h::t -> Safelist.fold_left IntSet.inter h t
      end
  | And(fs) ->
      begin match Safelist.map get_zeros fs with
          [] -> IntSet.empty
        | h::t -> Safelist.fold_left IntSet.union h t
      end
  |  Exists(f) -> IntSet.fold
       (fun xi u -> IntSet.add (xi-1) u) 
        (IntSet.remove 0 (get_zeros f))
        IntSet.empty

(** add2: t -> t -> t *)
let add2 f1 f2 =
  let fv1,fv2 = fvs_t f1, fvs_t f2 in
  let zs1,zs2 = get_zeros f1, get_zeros f2 in
  let common_vars = IntSet.diff (IntSet.inter fv1 fv2) (IntSet.union zs1 zs2) in
  let f1_zeroed = substitute (Safelist.map (fun z -> (z,zero)) (IntSet.elements zs1)) f1 in
  let f2_zeroed = substitute (Safelist.map (fun z -> (z,zero)) (IntSet.elements (IntSet.diff zs2 zs1))) f2 in
    if IntSet.is_empty common_vars then mkAnd[f1_zeroed;f2_zeroed]
    else
      let c = IntSet.cardinal common_vars in

      (* next we construct a triple of lists:         
         - the first element is a list of the names of the vars used
         to instantiate f1. these will be [0..c-1], but it is handy
         to actually construct the list since we're iterating over
         fvs anyways.

         - similarly the second element is a list of the names of the vars 
         used to instantiate f2, (i.e., [c,..,2c-1])

         - the third element is a list associating each free var n in
         fvs to options wrapping the variables used to instantiate
         the variable formerly known as n in f1 and f2 respectively.
         e.g., if n is in fvs(f1) and fvs(f2) and was instantiated
         with var(ci) in f1 and var(cj) in f2 then 
         (n, Some ci, Some cj) appears in assoc *)
      let rec mk_assoc i1 i2 (vs1,vs2,assoc) = function
          [] -> (List.rev vs1, List.rev vs2, List.rev assoc)
        | n::rest -> 
            let (vs1',i1',o1),(vs2',i2',o2) = match IntSet.mem n fv1,IntSet.mem n fv2
            with true,true -> 
              (((n,Var(i1))::vs1,i1+1,Some i1),
               ((n,Var(i2))::vs2,i2+1,Some i2))
              | true,false ->
                  (((n,Var(i1))::vs1,i1+1,Some i1),
                   ((n,Var(n+2*c))::vs2,i2,None))
              | false,true ->
                  (((n,Var(n+2*c))::vs1,i1,None),
                   ((n,Var(i2))::vs2,i2+1,Some i2)) 
              | false,false -> assert false in
              mk_assoc i1' i2' (vs1',vs2',(n,o1,o2)::assoc) rest in
      let f1_subst, f2_subst, assoc = 
        mk_assoc 0 c ([],[],[]) (IntSet.elements common_vars) in

      (* instantiate f1 and f2 with vars generated by mk_assoc *)
      let f1_fresh,f2_fresh = substitute f1_subst f1_zeroed, substitute f2_subst f2_zeroed in

      (* then, for each original free variable var(n_i) that they have
         in common, add an equality between var(n_i) shifted by (2c)
         and the sum of the vars corresponding to n in f1' and f2' *)
      let rec mk_sums acc = function
          [] -> Safelist.rev acc 
        | (n,Some m1, Some m2)::rest ->
            let e = mkEq (mkVar (n+2*c)) (mkSum (mkVar m1) (mkVar m2)) in
              mk_sums (e::acc) rest 
        | _::rest -> mk_sums acc rest in
      let var_sums = mk_sums [] assoc in

      (* finally, existentially the first c1+c2 variables *)
      let rec mk_exists num f = if num=0 then f else mk_exists (num-1) (mkExists f) in

        (* final result *)
        mk_exists (2*c) (mkAnd (f1_fresh::f2_fresh::var_sums))

let add ts0 = match ts0 with 
    [] -> Error.simple_error "P.add: zero-length addition"
  | t::ts -> Safelist.fold_left (fun s ti -> add2 s ti) t ts 

(* ---- BIT VECTOR ---- *)
module BitVector = struct

  let bpi = Sys.word_size - 2

  type t = { data : int array;
             length : int }

  let length bv = bv.length

  let create n = 
    let data_length = if n=0 then 0 else n/bpi+1 in
      { data=Array.create data_length 0;
        length=n; }

  let pos n = (n / bpi, n mod bpi) 

  let masks = Array.init bpi (fun j -> 1 lsl j)

  let neg_masks = Array.init bpi (fun j -> max_int - masks.(j))

  let get v n =         
    let (idx,j) = pos n in 
      min 1 ((Array.unsafe_get v.data idx) land (Array.unsafe_get masks j))

  let set v n x =
    if x < 0 or x > 1 then 
      raise (Invalid_argument 
               (Printf.sprintf 
                  "Valuation.set: cannot set bit %d to %d" n x));
    let (idx,j) = pos n in
    let new_vn =   
      if x=0 then (Array.unsafe_get v.data idx) land (Array.unsafe_get neg_masks j) 
      else (Array.unsafe_get v.data idx) lor (Array.unsafe_get masks j) in
      Array.unsafe_set v.data idx new_vn

  (* same as (set v n (succ (get v n))), but inlined and w/o error checking *)
  let bump v n = 
    let (idx,j) = pos n in
    let v_idx = Array.unsafe_get v.data idx in        
      Array.unsafe_set v.data idx (v_idx lor (Array.unsafe_get masks j))

  let format_t v = 
    Format.printf "[";
    for i=0 to v.length-1 do       
      Format.printf "%d%s" (get v i) (if i=v.length-1 then "" else ",")
    done;
    Format.printf "]"

end

(* ----- VALUATION ----- *)
module Valuation = struct  
  type t = { bit_data : BitVector.t;
             int_data : int array;
             mem_data : BitVector.t option;
             num_ints : int;
           }

  let length v = BitVector.length v.bit_data + v.num_ints

  let create nbits nints do_mems =
    { bit_data = BitVector.create nbits;
      int_data = Array.create nints 0;    
      mem_data = if do_mems then Some (BitVector.create (nbits + nints)) else None;
      num_ints = nints; }

(*   let get v n =  *)
(*     let blength = BitVector.length v.bit_data in  *)
(*       if n < blength then BitVector.get v.bit_data n  *)
(*       else Array.unsafe_get v.int_data (n - blength) *)

(*   let set v n x =  *)
(*     let blength = BitVector.length v.bit_data in  *)
(*       if n < blength then BitVector.set v.bit_data n x *)
(*       else Array.unsafe_set v.int_data (n - blength) x *)

  let zonk v n = 
    match v.mem_data with 
      None -> ()
    | Some mems -> BitVector.set mems n 1


  let bump v n = 
    zonk v n;
    let blength = BitVector.length v.bit_data in 
      if n < blength then BitVector.bump v.bit_data n
      else 
        let idx = n-blength in 
          Array.unsafe_set v.int_data idx ((Array.unsafe_get v.int_data idx)+1)

  let format_t v = 
    let concat l = List.fold_left (fun a e -> if a="" then e else Printf.sprintf "%s,%s" e a) "" l in
      Format.printf "{ bit_data="; 
      BitVector.format_t v.bit_data;
      Format.printf ", int_data=[%s], mem_data="
        (concat (List.map string_of_int (Array.to_list v.int_data)));
      (match v.mem_data with
           None -> Format.printf "None";
         | Some mems -> BitVector.format_t mems);
      Format.printf  " }@\n%!" 

let _ = Callback.register "Valuation.get_bpi" (fun () -> BitVector.bpi)
end

(* ---- OMEGA ---- *)    
let rec f2omega g o f = match f with 
    EqZ(ps_xs,c) ->
      let ps_vs = Safelist.map (fun (pi,xi) -> (pi, (snd g) xi)) ps_xs in
        OmegaLibrary.add_eq o ps_vs c

  | GeqZ(ps_xs,c) ->
      let ps_vs = Safelist.map (fun (pi,xi) -> (pi, (snd g) xi)) ps_xs in
        OmegaLibrary.add_geq o ps_vs c

  | Not(f1) -> 
      let not_o = OmegaLibrary.add_not o in
        f2omega g not_o f1;
        OmegaLibrary.finalize not_o

  | Or(fs) ->
      let or_o = OmegaLibrary.add_or o in 
        Safelist.iter (f2omega g or_o) fs;
        OmegaLibrary.finalize or_o

  | And(fs) ->
      let and_o = OmegaLibrary.add_and o in 
        Safelist.iter (f2omega g and_o) fs;
        OmegaLibrary.finalize and_o

  | Exists(f1) -> 
      let (qd,g') = g in    
      let exists_o, var_x = OmegaLibrary.add_exists o (sprintf "x%d" qd) in 
      let g' = (qd+1, fun n -> if n=0 then var_x else g' (n-1)) in
        f2omega g' exists_o f1;
        OmegaLibrary.finalize exists_o

let rec wrap n f = if n < 0 then f else wrap (n-1) (mkExists f)

let close f = 
  let xs = fvs_t f in 
    if IntSet.is_empty xs then f 
    else wrap (IntSet.max_elt xs) f

let empty_env n = raise 
  (Error.Harmony_error 
     (fun () ->
        Format.printf "Presburger.satisfiable: unknown variable: n%d" n))

let satisfiable f = 
  let omega_init_g = (0, empty_env) in    
  let f' = close f in    
  let o = OmegaLibrary.empty (0) in
    f2omega omega_init_g o f';
    OmegaLibrary.finalize o;
    OmegaLibrary.satisfiable o

type this_t = t
module TCache = Hashtbl.Make(
  struct
    type t = this_t
    let equal = (==)
    let hash o = Hashtbl.hash (Obj.magic o : int)
  end)

module VCache = Hashtbl.Make(
  struct
    type t = Valuation.t
    let equal f1 f2 = compare f1 f2 = 0
    let hash = Hashtbl.hash 
  end)  

let omega_cache : ((OmegaLibrary.t * bool VCache.t) TCache.t) = TCache.create 101
let l1_hits, l1_misses = ref 0, ref (-1)
let l2_hits, l2_misses = ref 0, ref 0

let fast_sat f vs = 
  let (o,l2) = 
    try 
      let res = TCache.find omega_cache f in
        incr l1_hits;
        res
    with Not_found ->
      incr l1_misses;
      decr l2_misses; (* this one doesn't count *)
      let n = Valuation.length vs in
      let o = OmegaLibrary.empty n in
      let vcache = VCache.create 3 in
      let res = (o,vcache) in
        f2omega (0, fun k -> 
                   if k <= n-1 then OmegaLibrary.get_var o k 
                   else 
                     raise 
                       (Error.Harmony_error 
                          (fun () ->
                             Format.printf "Presburger.fast_sat: unknown variable: n%d" k))                     
                ) o f;
        OmegaLibrary.finalize o;
        TCache.add omega_cache f res;
        res in
    try 
      let sat = VCache.find l2 vs in
        incr l2_hits;
        sat
    with Not_found ->
      incr l2_misses;
      let sat = OmegaLibrary.fast_sat o vs in        
        (* Format.printf "res=%b@\n%!" sat; *)
        VCache.add l2 vs sat;
        sat

let print_stats () = 
  let mk_pct hits misses =
    100.0 *. (float_of_int (!hits) /.
                float_of_int ((!hits + !misses))) in
  let l1_entries,l2_total_entries,l2_number = TCache.fold
    (fun _ (_,l2) (l1, l2e, l2n) ->
       (l1 + 1, l2e + (VCache.length l2), l2n + 1))
    omega_cache
    (0,0,0) in
    Trace.debug "cache"
      (fun () ->           
           Format.printf "--- Presburger cache stats ---@\n";
           Format.printf "total queries: %d@\n" (!l2_hits + !l2_misses);
           Format.printf "l1_cache@\n";
           Format.printf "\tentries\t\t: %d@\n" l1_entries;
           Format.printf "\thit rate\t: %.3f%s@\n" (mk_pct l1_hits l1_misses) "%";
           Format.printf "l2_cache@\n";
           Format.printf "\taverage entries\t: %.3f@\n" ((float_of_int l2_total_entries) /. (float_of_int l2_number));
           Format.printf "\thit rate\t: %.2f%s" (mk_pct l2_hits l2_misses) "%";
           Format.print_newline ())

(*          let bytes2str b = 
           let thresh = 1024 in
           let fthresh = float_of_int thresh in
             if b < thresh then sprintf "%dB" b
             else let kb = (float_of_int b /. fthresh) in 
               if kb < fthresh then sprintf "%.1fKB" kb
               else let mb = kb /. fthresh in 
                 if mb < fthresh then sprintf "%.1fMB" mb 
                 else sprintf "%.1fGB" (mb /. fthresh) in
*)

(*
  let (_,_,n,bv,l1),l2_rev = Array.fold_left 
    (fun ((fill_bv,pos,n,bv,l1),l2) ai ->
       let acc1 = 
         if fill_bv && ai <= 1 then 
           let fill_bv' = n+1 < Sys.word_size in
           let bv' = bv lor (ai lsl pos) in
             (fill_bv', pos+1, n+1, bv',l1)
         else
           (false,0,n,bv,ai::l1) in
         (acc1,ai::l2))
    ((true,0,0,0,[]),[])
    a in
(*     Format.printf "@\n --- array2fingerprint ---@\n";
    Format.printf "a=[%s]@\n"
      (Misc.concat_list "," (Safelist.map string_of_int (Array.to_list a)));
    Format.printf "fingerprint=((%d,%d),[%s])@\n" 
      n bv (Misc.concat_list "," (Safelist.map string_of_int l1));
    Format.printf "to_list=[%s]@\n"
      (Misc.concat_list "," (Safelist.map string_of_int (Safelist.rev l2_rev)));
*)
    ((n::bv::l1),(Safelist.rev l2_rev))
*) 

