(*********************************************************)
(* The Harmony Project                                   *)
(* harmony@lists.seas.upenn.edu                          *)
(*                                                       *)
(* presburger.ml - presburger arithmetic constraints     *)
(*********************************************************)
(* $Id$ *)

(* Ensure that the C library is initialized *)
let _ = GenepiLibrary.init ()

let sprintf = Printf.sprintf

(* ------------------------ abstract syntax ------------------------ *)
(* exps: used only for easy construction of EqZs. *)
type exp = 
    Const of int
    | Var of int 
    | Sum of exp list 
        
type g = { set : GenepiLibrary.t; empty : unit -> bool }

(* formulas, represented in de Bruijn notation *)

type possible_value = Definite of int | Unknown | Nothing

type e = 
    EqZ of int Int.Map.t * int
    | Not of t
    | Or of t list
    | And of t list
    | Ex of t
  and f = { e:e; hash:int } 
  and t = { def:f; width:int; zeros: unit -> Int.Set.t; values: int -> possible_value; comp:int -> g }

(* --- formatter --- *)
let format_possible_value = function
    Definite(c) -> Util.format "Definite(%d)" c
  | Unknown     -> Util.format "Unknown"
  | Nothing     -> Util.format "Nothing"

let rec format_exp = function
    Const(n) -> Util.format "%d" n
  | Var(x) -> Util.format "n%d" x
  | Sum(es) -> Misc.format_list "+" format_exp es
  
let init_g = (0,sprintf "n%d")

let format_constraint_aux g (vs,c) = 
  let non_neg, neg = Int.Map.partition (fun _ wi -> wi >= 0) vs in 
  let pos, zero = Int.Map.partition (fun _ wi -> wi > 0) non_neg in 
  let pos_empty = Int.Set.is_empty (Int.Map.domain pos) in 
  let neg_empty = Int.Set.is_empty (Int.Map.domain neg) in 
    if pos_empty && neg_empty then 
      Util.format "%d=%d" 
        (if c >=0 then c else 0)
        (if c < 0 then -c else 0)
    else if pos_empty && c=0 then 
      (Int.Map.iter_with_sep (fun xi wi -> Util.format "%s%s" (if wi=(-1) then "" else sprintf "%d*" (-wi)) ((snd g) xi))
          (fun () -> Util.format "+")
          neg;
       Util.format "=0")
    else
      begin if pos_empty then Util.format "%d" (if c >= 0 then c else 0)
        else (Int.Map.iter_with_sep 
                 (fun xi wi -> Util.format "%s%s" 
                   (if wi=1 then "" else sprintf "%d*" wi) ((snd g) xi))
                 (fun () -> Util.format "+")
                 pos;
              if c > 0 then Util.format "+%d" c);
        Util.format "=";
        if neg_empty then Util.format "%d" (if c < 0 then -c else 0) 
        else 
          (Int.Map.iter_with_sep 
              (fun xi wi -> Util.format "%s%s" 
                (if wi=(-1) then "" else sprintf "%d*" (-wi)) ((snd g) xi))
              (fun () -> Util.format "+")
              neg;
           if c < 0 then Util.format "+%d" (-c))
      end 

let format_constraint = format_constraint_aux init_g

let rec format_e_aux g e0 = match e0 with
    EqZ(vs,c) -> format_constraint_aux g (vs,c)
  | Not(t1) -> 
      Util.format "!("; 
      format_t_aux g t1;
      Util.format ")"
  | Or(ts) ->
      Util.format "(";
      Misc.format_list " | " (format_t_aux g) ts;
      Util.format ")";
  | And(ts) ->
      Util.format "(";
      Misc.format_list " & " (format_t_aux g) ts;
      Util.format ")"
  | Ex(t1) -> 
      let next (qd,gm) = (qd+1,fun n->if n=0 then sprintf "x%d" qd else gm (n-1)) in
      let rec format_exs g t = match t.def.e with
          Ex(t1) -> 
            Util.format ",x%d" (fst g);
            format_exs (next g) t1
        | _ -> 
            Util.format ".";
            format_t_aux g t in
        Util.format "EX x%d" (fst g);
        format_exs (next g) t1
and format_t_aux g t0 = format_e_aux g t0.def.e

let format_e = format_e_aux init_g

let rec format_e_bare = function
    EqZ(vs,c) -> Util.format "("; Int.Map.iter (fun x w -> Util.format "%d*x%d" w x) vs; Util.format "+ %d = 0)" c
  | Not(t1) -> Util.format "~("; format_t_bare t1; Util.format ")";
  | Or(ts) -> Util.format "("; Misc.format_list "|" format_t_bare ts; Util.format ")"
  | And(ts) -> Util.format "("; Misc.format_list "&" format_t_bare ts; Util.format ")"
  | Ex(t1) -> Util.format "EX("; format_t_bare t1; Util.format ")"

and format_t_bare t0 = format_e_bare t0.def.e

let format_t = format_t_aux init_g

let hash_list = Safelist.fold_left (fun a ti -> 883 * ti.def.hash + a) 0 
let hash_map m = Int.Map.fold (fun x w a -> 757 * (x+1) + a) m 0 
let hash_const c = 563 * c
let hash_constraint (vs,c) = hash_map vs + hash_const c
let hash_tag = function
    Not(_)    -> 71
  | Or(_)     -> 199
  | EqZ(_)    -> 1
  | And(_)    -> 821
  | Ex(_)     -> 379
let hash_e e0 = 
  let tag_code = hash_tag e0 in 
  let subelt_code = match e0 with
      Not(t) | Ex (t) -> t.def.hash
    | Or(ts) | And(ts) -> hash_list ts
    | EqZ(m,c) -> hash_constraint(m,c) in 
  tag_code * subelt_code
      
let equal_constraint (m1,c1) (m2,c2) = 
  ((c1=c2)
  && (Int.Set.equal (Int.Map.domain m1) (Int.Map.domain m2))
  && (Int.Map.fold
         (fun k1 v1 a -> 
           a && (try v1 = Int.Map.find k1 m2 with Not_found -> false))
         m1 true))
    
let rec equal_list ts1 ts2 = 
  let rec loop acc l1 l2 = match acc,l1,l2 with 
    | true,[],[]      -> true
    | false,_,_       -> false            
    | _,[],_ | _,_,[] -> false
    | _,t1::r1,t2::r2 -> loop (equal_e t1.def.e t2.def.e) r1 r2 in 
    loop true ts1 ts2 

and equal_e e1 e2 = match e1,e2 with
    EqZ(vs1,c1),EqZ(vs2,c2) -> equal_constraint (vs1,c1) (vs2,c2)
  | Or(ts1), Or(ts2) | And(ts1), And(ts2) -> equal_list ts1 ts2
  | Not(t1),Not(t2) | Ex(t1),Ex(t2) -> equal_e t1.def.e t2.def.e 
  | _ -> false

let rec compare_t t1 t2 = match t1.def.e,t2.def.e with
    EqZ(m1,c1),EqZ(m2,c2) ->
      let cmp1 = compare c1 c2 in 
        if cmp1 <> 0 then cmp1
        else 
          let cmp2 = Int.Set.compare (Int.Map.domain m1) (Int.Map.domain m2) in 
            if cmp2 <> 0 then cmp2 
            else (Int.Map.fold
                     (fun k1 v1 cmpa -> 
                       if cmpa <> 0 then cmpa 
                       else try compare v1 (Int.Map.find k1 m2) with Not_found -> 1)
                     m1 0)
  | Or(ts1),Or(ts2) | And(ts1), And(ts2) ->
      Misc.dict_cmp compare_t ts1 ts2
  | Not(t1),Not(t2) | Ex(t1),Ex(t2) -> compare_t t1 t2
  | EqZ(_),_ -> -1
  | _,EqZ(_) -> 1
  | Or(_),_ -> -1
  | _,Or(_) -> 1
  | And(_),_ -> -1
  | _,And(_) -> 1
  | Not(_),_ -> -1
  | _,Not(_) -> 1
      
(* --- stats --- *)
let width_e e0 = match e0 with
    EqZ(m,_) -> 
      Int.Map.fold 
        (fun xi _ w -> max ((if xi<0 then (-xi) else xi)+1) w)
        m 0
  | Not(t) -> t.width
  | Ex(t) -> t.width-1
  | Or(ts) | And(ts)   -> Safelist.fold_left (fun w ti -> max w ti.width) 0 ts 
          
let width t = t.width

let rec add_to_list n x l = if n < 1 then l else add_to_list (n-1) x (x::l)
  
(* --- easy_zeros: calculate a set of "obviously zero" variables from a
   formula. NOT semantically complete--i.e., it's not the case that 
   n in get_zeros(f) <=> every satisfying valuation of f has n=0 *)
let easy_zeros_e = function
    EqZ(vs,c) -> 
      if c = 0 then match Int.Map.fold 
          (fun xi wi acc -> match acc with 
              (false,_)         -> acc
            | (true,None)       -> (true, Some(Int.Set.add xi Int.Set.empty, wi > 0))
            | (true,Some(zs,p)) -> ((wi > 0)=p, Some (Int.Set.add xi zs, p)))
          vs (true,None)  
        with false,_ | _,None -> Int.Set.empty
          | true,Some(zs,_) -> zs
      else Int.Set.empty
  | Not(t1) -> begin match t1.def.e with 
        Not(t2) -> t2.zeros ()
      | _ -> Int.Set.empty
    end
  | Or(ts) -> begin match ts with
        [] -> Int.Set.empty
      | t1::trest -> Safelist.fold_left (fun a ti -> Int.Set.inter (ti.zeros ()) a) (t1.zeros ()) trest     
    end
  | And(ts) -> Safelist.fold_left (fun a ti -> Int.Set.union (ti.zeros ()) a) Int.Set.empty ts
  | Ex(t1) -> 
      Int.Set.fold 
        (fun xi a -> Int.Set.add (xi-1) a)
        (Int.Set.remove 0 (t1.zeros ()))
        Int.Set.empty

let easy_zeros t = t.zeros ()

let rec easy_var_value_e e0 x = match e0 with
    EqZ(vs,c)  ->       
      (try 
          let cx = Int.Map.find x vs in 
            (match Int.Set.cardinal (Int.Map.domain vs), cx with
                1,1  -> Definite (-c)
              | 1,-1 -> Definite c
              | _    -> Unknown)
        with Not_found -> Nothing)

  | Not(t1) -> (match t1.def.e with 
        Not(t2) -> t2.values x
      | _      -> (match t1.values x with 
            Nothing -> Nothing
          | _       -> Unknown))

  | Or(ts) | And(ts) -> Safelist.fold_left 
      (fun acc ti -> 
        match acc with
            Nothing    -> ti.values x
          | Unknown  -> Unknown
          | Definite i -> 
              (match ti.values x with 
                  Nothing -> acc
                | Unknown -> Unknown
                | Definite j -> if i=j then acc else Unknown))
        Nothing ts
        
  | Ex(t1) -> t1.values (x+1)

(* ---- translation to C library structure ---- *)    
let rec s_of_e e0 width = 
  Trace.debug "compile+" (fun () -> Util.format "COMPILING %d " width; format_e e0; Util.format "@\n");
  match e0 with
    EqZ(vs,c) -> 
      let rec loop i acc =           
        if i < 0 then acc 
        else loop (i-1) ((Int.Map.safe_find i vs 0)::acc) in
        GenepiLibrary.linear_constraint (loop (width-1) []) (-c)
          
  | Not(f1) -> GenepiLibrary.complement(genepi_set_of_t f1 width)
      
  | Or(fs) -> 
      Safelist.fold_left 
        (fun gs fi -> GenepiLibrary.union (genepi_set_of_t fi width) gs)
        (GenepiLibrary.bot width)
        fs 
        
  | And(fs) ->
      Safelist.fold_left
        (fun gs fi -> GenepiLibrary.intersection (genepi_set_of_t fi width) gs)
        (GenepiLibrary.top width)
        fs 
        
  | Ex(f1) ->       
      let x0_sel = 1::(add_to_list width 0 []) in
      let f1 = genepi_set_of_t f1 (width+1) in 
        GenepiLibrary.project f1 x0_sel 
  
and genepi_set_of_t t width = (t.comp width).set
  
let g_of_s s0 =   
  { set=s0; 
    empty=
      let module M = Memo.Make(struct
        type arg = unit
        type res = bool
        let name = "Presburger.empty"
        let f () = not (GenepiLibrary.is_empty s0)
        let init_size = 1
        let format_arg () = Util.format "()"
        let format_res = Util.format "%b"
        let hash () = 1
        let equal () () = true
      end) in 
      M.memoized }

let find_best w = 
  let rec loop acc = function
      [] -> acc 
    | (wh,_) as h::t -> 
        let acc' = match acc with 
            None -> Some h
          | Some (wa,_) -> 
              if wa < w then 
                if wh < w && wh > wa then Some h 
                else acc
              else if wh < w || wh > wa then Some h 
              else acc in
        loop acc' t in 
  loop None 
      
let t_of_f f0 = 
  let compiled = ref [] in 
    { def = f0; 
      width=width_e f0.e; 
      zeros=
        (let module M = Memo.Make(struct
          type arg = unit
          type res = Int.Set.t
          let name = "Presburger.easy_zeros"
          let f () = easy_zeros_e f0.e
          let init_size = 1
          let format_arg () = Util.format "()"
          let format_res s = 
            Util.format "{"; 
            Misc.format_list "," (Util.format "%d") (Int.Set.elements s);
            Util.format "}"
          let hash () = 1
          let equal () () = true
        end) in 
        M.memoized);            
      values=
        (let module M = Memo.Make(struct
          type arg = int
          type res = possible_value
          let name = "Presburger.values"
          let f x = easy_var_value_e f0.e x
          let init_size = 1
          let format_arg = Util.format "%d"
          let format_res = format_possible_value
          let hash = Hashtbl.hash
          let equal = (=)
        end) in 
        M.memoized);            
      comp=
        let module M = Memo.Make(struct
          type arg = int
          type res = g
          let name = "Presburger.compile"
          let f width = 
            let s0 = match find_best width !compiled with 
                None -> s_of_e f0.e width
              | Some (w1,s_w1) ->  
                  Trace.debug "compile+" 
                    (fun () -> 
                      Util.format "RESIZING from %d to %d " w1 width; 
                      format_e f0.e; 
                      Util.format "@\n");
                  if w1 < width then 
                    let sel = add_to_list w1 0 (add_to_list (width-w1) 1 []) in 
                      GenepiLibrary.inv_project s_w1 sel 
                  else
                    let sel = add_to_list width 0 (add_to_list (w1-width) 1 []) in 
                      GenepiLibrary.project s_w1 sel in
              compiled := (width,s0)::!compiled;            
              g_of_s s0
          let init_size = 5
          let format_res g = 
            Util.format "<genepi_set : "; 
            format_e f0.e; 
            Util.format ">"
          let format_arg = Util.format "%d"
          let hash = Hashtbl.hash
          let equal = (=)
        end) in 
        M.memoized }
    
let t_of_e e = t_of_f { e=e; hash=hash_e e }

let map_int_map f m = 
  Int.Map.fold 
    (fun k v a -> let k',v' = f k v in Int.Map.add k' v' a) 
    m Int.Map.empty

(* free variables *)
let rec fvs_t t = match t.def.e with
    EqZ(vs,_) -> Int.Map.domain vs 
          
  | Not(t)    -> fvs_t t

  | Or(ts) | And(ts) -> 
      Safelist.fold_left 
        (fun fvs ti -> Int.Set.union (fvs_t ti) fvs)
        Int.Set.empty
        ts
        
  | Ex(t)    -> 
      Int.Set.fold 
        (fun x s -> if (x <> 0) then (Int.Set.add (x-1) s) else s) 
        (fvs_t t)
        Int.Set.empty 
    
(* -------------- constraints -------------- *)
let combine_constraints (vs1,c1) (vs2,c2) = 
  let pre_c = c1+c2 in 
  let c',neg = if pre_c > 0 then (-pre_c,true) else (pre_c,false) in 
  let vs' = Int.Set.fold 
    (fun xi vs' -> 
       let we1 = Int.Map.safe_find xi vs1 0 in 
       let we2 = Int.Map.safe_find xi vs2 0 in
       let w' = if neg then -1 * (we1 + we2) else we1+we2 in 
         Int.Map.add xi w' vs')
    (Int.Set.union (Int.Map.domain vs1) (Int.Map.domain vs2))
    Int.Map.empty in 
    (vs',c')

let rec constraint_of_exp is_lhs = function 
    Const(n) -> 
      (Int.Map.empty,if is_lhs then n else -n)
  | Var(x) -> 
      (Int.Map.add x (if is_lhs then 1 else -1) Int.Map.empty, 0)
  | Sum([]) -> (Int.Map.empty,0)
  | Sum(eh::es) -> 
      Safelist.fold_left 
        (fun c ei -> combine_constraints c (constraint_of_exp is_lhs ei))
        (constraint_of_exp is_lhs eh)
        es
            
(* --------------- constructors --------------- *)

(* --- expressions --- *)
let mkConst n = Const(n)

let mkVar x = Var(x)

let mkSum es = 
  match Safelist.fold_left 
    (fun a ei -> match ei with
        Const(0) -> a
      | Sum(es) -> es@a
      | _ -> ei::a)
    [] es 
  with [] -> Const(0)
    | es' -> Sum(es')


(* hash consing infrastructure *)
type this_t = t
module HC_BASE = struct
  type res = this_t
  let format_res = format_t 
end

module HC_T_BASE = struct
  include HC_BASE
  type arg = this_t
  let hash t1 = t1.def.hash
  let equal = (==)
  let format_arg = format_t 
  let init_size = 137
end

module HC_TS_BASE = struct
  include HC_BASE
  type arg = this_t list
  let hash = hash_list
  let equal = equal_list
  let format_arg ts = 
    Util.format "[";
    Misc.format_list "," format_t ts;
    Util.format "]"      
  let init_size = 137
end

let mkEqZ_from_constraint = 
  let module M = Memo.Make(struct
    include HC_BASE
    type arg = (int Int.Map.t * int)
    let format_arg = format_constraint
    let hash = hash_constraint
    let equal = equal_constraint
    let name = "Presburger.mk_EqZ"
    let f (vs,c) = t_of_e (EqZ(vs,c))
    let init_size = 137
  end) in 
  M.memoized

let mkEq e1 e2 = 
  mkEqZ_from_constraint 
    (combine_constraints 
        (constraint_of_exp true e1)
        (constraint_of_exp false e2))

(* --------------- constants --------------- *)

let zero = Const(0)
let one = Const(1)
let tru = mkEq zero zero
let fls = mkEq zero one

let mkNot = 
  let module M = Memo.Make(struct
    include HC_T_BASE
    let name = "Presburger.mkNot"
    let f t1 = t_of_e (Not(t1))
  end) in 
  M.memoized

let mkOr = 
  let module M = Memo.Make(struct
    include HC_TS_BASE
    let name = "Presburger.mkOr"
    let f ts = 
      let rec loop acc = function
          [] -> Safelist.rev acc 
        | h::t -> begin match h.def.e with 
              Or(hs) -> loop acc (hs@t)
            | _ ->
                if h==fls then loop acc t 
                else if h==tru then [tru] 
                else loop (h::acc) t
          end in 
        match loop [] ts with
            []  -> fls
          | [h] -> h
          | ts' -> t_of_e(Or(ts'))
  end) in
  M.memoized 

let mkAnd = 
  let module M = Memo.Make(struct
    include HC_TS_BASE
    let name = "Presburger.mkAnd"
    let f ts = 
      let rec loop acc = function
          [] -> Safelist.rev acc 
        | h::t -> begin match h.def.e with 
              And(hs) -> loop acc (hs@t)
            | _ -> 
                if h==tru then loop acc t
                else if h==fls then [fls] 
                else loop (h::acc) t
          end in 
        match loop [] ts with 
            [] -> tru
          | [h] -> h
          | ts'   -> t_of_e(And(ts'))
  end) in 
  M.memoized

let mkEx = 
  let module M = Memo.Make(struct
    include HC_T_BASE
    let name = "Presburger.mkEx"
    let f t1 = t_of_e (Ex(t1)) 
  end) in 
  M.memoized 
  
let rec wrap n f = if n <= 0 then f else wrap (n-1) (mkEx f)

let close f = wrap (width f) f

(* --- helper function for constructing formulas from exps --- *)
       
let mkGe e1 e2 = 
  let shift_map m = Int.Map.fold (fun xi wi a -> Int.Map.add (xi+1) wi a) m Int.Map.empty in
  let vs1,c1 = constraint_of_exp true e1 in 
  let vs2,c2 = constraint_of_exp false e2 in 
  let vs1' = Int.Map.add 0 0 (shift_map vs1) in 
  let vs2' = Int.Map.add 0 (-1) (shift_map vs2) in 
  let (vs,c) = combine_constraints (vs1',c1) (vs2',c2) in 
  let t1 = mkEqZ_from_constraint (vs,c) in 
    mkEx t1
      
let mkGt e1 e2 = mkGe e1 (mkSum [e2; mkConst 1])
let mkLe e1 e2 = mkGe e2 e1
let mkLt e1 e2 = mkGt e2 e1

  
(* --------------- de Bruijn shifting / substitution -------------- *)

(* --- shift with cutoff --- *)
let rec shift_exp_aux n c e0 = match e0 with
    Const(n)   -> e0
  | Var(x)     -> if x < c then e0 else Var(x+n)
  | Sum(es) -> Sum(Safelist.map (shift_exp_aux n c) es)

let rec shift_aux n c t0 = 
  let shift_var x wi = 
    let y = if x < 0 then -x else x in 
    let pre_res = if y<c then y else y+n in 
      if x < 0 then (-pre_res,wi) else (pre_res,wi) in 
    t_of_e
      (match t0.def.e with
           EqZ(vs,c)  -> EqZ(map_int_map shift_var vs, c)
         | Not(t1)    -> Not(shift_aux n c t1)
         | Or(ts)     -> Or(Safelist.map (shift_aux n c) ts)
         | And(ts)    -> And(Safelist.map (shift_aux n c) ts)
         | Ex(t1) -> Ex(shift_aux n (c+1) t1))
             
(* --- top-level shift operator --- *)
let shift_exp n = shift_exp_aux n 0
let shift_t n t = 
  Trace.debug "presburger+" (fun() -> Util.format "SHIFT: "; format_t t);
  let res = shift_aux n 0 t in 
    Trace.debug "presburger+" (fun() -> Util.format " -> "; format_t res); 
    res

let rec substitute_exp s e0 = match e0 with 
    Const(_) -> e0
  | Var(x) -> (try Safelist.assoc x s with Not_found -> e0)
  | Sum(es) -> Sum(Safelist.map (substitute_exp s) es)
        
let rec substitute es t0 = 
  let substitute_constraint (vs,c) = 
    let mult (vs,c) n = (map_int_map (fun xi wi -> (xi,n*wi)) vs, c*n) in 
      Int.Map.fold 
        (fun xi wi (vs,c) ->
           try 
             combine_constraints
               (mult (constraint_of_exp true (Safelist.assoc xi es)) wi)
               (vs,c)
           with Not_found -> 
             (Int.Map.add xi wi vs,c))
        vs 
        (Int.Map.empty,c) in
    match t0.def.e with
        EqZ(vs,c)  -> mkEqZ_from_constraint (substitute_constraint (vs,c))
      | Not(t1) -> mkNot(substitute es t1)
      | Or(ts)  -> mkOr(Safelist.map (substitute es) ts)
      | And(ts) -> mkAnd(Safelist.map (substitute es) ts)
      | Ex(t1) ->
          let shifted_es = Safelist.map 
            (fun (x,e) -> (x+1, shift_exp 1 e)) es in
            mkEx(substitute shifted_es t1)
              
(* --------------- addition -------------- *)

(* given two formulas, f1 and f2, a formula representing (f1+f2) can
   be constructed by: 
   
   (1) instantiating each with fresh existentially-quantified
   variables and
   
   (2) adding constraints that each original variable is equal to the
   sum of the corresponding fresh variables. 

   however, omega is limited to a fixed number of variables, so
   whenever possible we try to avoid introducing unecessary
   variables. for add, we can avoid introducing fresh variables for
   variables that only appear in one formula, or for variables that
   are "obviously zero" (adding to zero is the identity. *)
let add2 t1 t2 =
  (* calculate free variables *)
  let fv1,fv2 = fvs_t t1, fvs_t t2 in
  let common_fvs = Int.Set.inter fv1 fv2 in 

  (* calculate the "obviously zero" variables *)
  let zs1,zs2 = easy_zeros t1, easy_zeros t2 in
  
  (* construct a formula equisatisfiable with f1 and f2 where some
     "obviously zero" variables is instantiated with the constant
     zero. we need to be careful: blindly substituting for ALL
     obviously zero variables in both formulas would lose the
     constraint entirely. thus, we only instantiate obviously zero
     variables that (a) are free in both formulas, and (b) we
     instantiate each obviously zero variable in at most one of f1 and
     f2. *)
  let f1_zeros = Int.Set.inter common_fvs zs1 in 
  let f1_zeroed = substitute (Safelist.map (fun z -> (z,zero)) (Int.Set.elements f1_zeros)) t1 in
  let f2_zeros = Int.Set.inter common_fvs (Int.Set.diff zs2 zs1) in 
  let f2_zeroed = substitute (Safelist.map (fun z -> (z,zero)) (Int.Set.elements f2_zeros)) t2 in   
  let remaining_common_vars = Int.Set.diff common_fvs (Int.Set.union f1_zeros f2_zeros) in
    Trace.debug "add+" 
      (fun () -> 
         Util.format " --- Presburger.add --- @\nf1="; 
         format_t t1; 
         Util.format "@\nf2="; 
         format_t t2;
         Util.format "@\nZS1={%s} (ALL={%s})"  
           (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements f1_zeros)))
           (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements zs1)));
         Util.format "@\nZS2={%s} (ALL={%s})" 
           (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements f2_zeros))) 
           (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements zs2)));
         Util.format "@\nf1_zeroed: "; format_t f1_zeroed;
         Util.format "@\nf2_zeroed: "; format_t f2_zeroed;
         Util.format "@\nREMAINING_COMMON: {%s}" (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements remaining_common_vars)));
         Util.format "@\n");
    
    (* next we recognize an easy common case: if f1_zeroed and
       f2_zeroed have no remaining free variables in common, then
       their sum is equivalent to their conjunction. *)
    if Int.Set.is_empty remaining_common_vars then 
      let res = mkAnd[f1_zeroed;f2_zeroed] in 
        (Trace.debug "add+" (fun () -> Util.format "QUICK RES="; format_t res; Util.format "@\n"); 
         res)
    else
      (* otherwise, we fall back and introduce fresh quantified
         variables for the remaining common variables as described
         above. 
         
         because we use a debruijn encoding, calcuating the correct
         instantiation requires a little arithmetic. variables
         {0..c_common-1} are used to instantiate the common variables
         in f1 and {c_common..2*c_common-1} for the common variables
         in f2.  {2*c_common..2*c_common+c_1} are used to instantiate
         the remaining non-shared variables in f1--at the end, when we
         wrap up the formula with 2*c_common existentials, they will
         point to the same variables; similarly
         {2*c_common+c_1..2*c_common+c_1+(Int.Set.cardinal
         remaining_fv2)-1} are used to instantiate the remaining
         non-shared variables in f2. *)
      let c_common = Int.Set.cardinal remaining_common_vars in
      let remaining_fv1 = Int.Set.diff fv1 f1_zeros in 
      let c_1 = Int.Set.cardinal remaining_fv1 in 
      let remaining_fv2 = Int.Set.diff fv2 f2_zeros in 
      let remaining_fvs = Int.Set.union remaining_fv1 remaining_fv2 in
        Trace.debug "add+" 
          (fun () -> 
             Util.format "REMAINING_FV1 {%s}@\n" (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements remaining_fv1)));
             Util.format "REMAINING_FV2 {%s}@\n" (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements remaining_fv2)));
             Util.format "REMAINING_FVS {%s}@\n" (Misc.concat_list "," (Safelist.map string_of_int (Int.Set.elements remaining_fvs)));
             Util.format "C_COMMON=%d, C_1=%d@\n" c_common c_1);        
        let init_i1 = 0 in 
        let init_o1 = 2 * c_common in 
        let init_o2 = c_1 + (2 * c_common) in 

        (* calculate instantiions for each formula and equalities, the
           equality constraints between (shifted up values of)
           variables common to both and corresponding pairs of fresh
           existential variables *)
        let _,_,f1_subst_rev,_,f2_subst_rev,equalities_rev = Safelist.fold_left 
          (fun (i_pos,o1,s1,o2,s2,e) n -> 
             match Int.Set.mem n remaining_fv1, Int.Set.mem n remaining_fv2 with 
                 true,true -> 
                   let x1 = mkVar i_pos in 
                   let x2 = mkVar (i_pos+c_common) in 
                   let n_shifted = mkVar (n+2*c_common) in 
                   let i_pos' = i_pos + 1 in 
                   let s1' = (n,x1)::s1 in 
                   let s2' = (n,x2)::s2 in 
                   let e' = (mkEq n_shifted (mkSum [x1;x2]))::e in
                     (i_pos',o1,s1',o2,s2',e')
               | true,false -> 
                   let o1' = o1 + 1 in                  
                   let s1' = (n,Var(o1))::s1 in 
                     (i_pos,o1',s1',o2,s2,e)
               | false,true -> 
                   let o2' = o2 + 1 in 
                   let s2' = (n,Var(o2))::s2 in 
                     (i_pos,o1,s1,o2',s2',e)
               | false,false -> assert false)
          (init_i1,init_o1,[],init_o2,[],[])
          (Int.Set.elements remaining_fvs) in 
          
        (* instantiate f1 and f2 with vars generated by mk_assoc *)
        let f1_subst = Safelist.rev f1_subst_rev in 
        let f1_fresh = substitute f1_subst f1_zeroed in 
        let f2_subst = Safelist.rev f2_subst_rev in 
        let f2_fresh = substitute f2_subst f2_zeroed in
        let equalities = Safelist.rev equalities_rev in 
          
          Trace.debug "add+"
            (fun () -> 
               Util.format "F1_FRESH: ";
               format_t f1_fresh;
               Util.format "@\nF2_FRESH: ";
               format_t f2_fresh;
               Util.format "@\nEQUALITIES: "; 
               Misc.format_list " & " format_t equalities;
               Util.format "@\n");
          
      (* final result *)
          let res = wrap (2*c_common) (mkAnd (f1_fresh::f2_fresh::equalities)) in 
            Trace.debug "add+" (fun () -> Util.format "RES="; format_t res; Util.format "@\n");
            res

let add ts0 = match ts0 with 
    [] -> Error.simple_error "P.add: zero-length addition"
  | t::ts -> Safelist.fold_left (fun s ti -> add2 s ti) t ts 

(* oracle infrastructure *)
let oracle_file = Prefs.createString "oracle" "" "Use xxx as an oracle" ""
let oracle_dump_file = Prefs.createString "oracle-dump" "" "Create an oracle in xxx" ""
let oracle_compile = Prefs.createBool "oracle-compile" false "Consult oracle and compile Presburger formulas to Genepi" ""

(* oracle state *)
module Oracle = Hashtbl.Make(struct
  type t = string
  let hash = Hashtbl.hash
  let equal = (=)
end)
let oracle = Oracle.create 267
let oracle_initialized = ref false  
let oracle_outc_opt = ref None

let initialize_oracle () = 
  let inc = open_in_bin (Prefs.read oracle_file) in 
    try 
      while true do       
        let s = (input_value inc : string) in 
        let b = (input_value inc : bool) in 
          Oracle.add oracle s b
      done
    with End_of_file -> 
      close_in inc;
      oracle_initialized := true 
      
let sat_oracle t = 
  if not !oracle_initialized then initialize_oracle ();
  Oracle.find oracle (Util.format_to_string (fun () -> format_t t))
    
let sat_genepi t = 
  let g = t.comp t.width in 
    g.empty ()
    
let satisfiable t =   
  let res = 
    if Prefs.read oracle_file <> "" then 
      begin 
        if Prefs.read oracle_compile then ignore (t.comp t.width);      
        try sat_oracle t with Not_found -> sat_genepi t
      end
    else sat_genepi t in 
  let dump_file = Prefs.read oracle_dump_file in 
    if dump_file <> "" then 
      begin
        let outc = match !oracle_outc_opt with 
            Some outc -> outc 
          | _ -> 
              let outc = open_out_bin dump_file in 
                oracle_outc_opt := Some outc;
                outc in 
          output_value outc (Util.format_to_string (fun () -> format_t t));
          output_value outc res 
      end;
    Trace.debug "satisfiable+"
      (fun () ->
        format_t t; 
        Util.format "@\n";
        Util.format " = %b@\n" res);
    res

let is_non_zero t0 x = 
  not (Int.Set.mem x (easy_zeros t0))
  && match t0.values x with
      Definite c -> c > 0
    | Nothing    -> false
    | Unknown  -> satisfiable (mkAnd [t0; mkGt (mkVar x) zero])
    
(* called from Toplevel after all processing has completed *)
let finish () = 
  match !oracle_outc_opt with 
    | Some outc -> close_out outc
    | None -> ()
        
