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
(* /boomerang/src/bsyntax.ml                                                   *)
(* Boomerang abstract syntax                                                   *)
(* $Id$ *)
(*******************************************************************************)

(* ----- imports and abbreviations ----- *)
let (@) = Safelist.append 
let sprintf = Printf.sprintf
let msg = Util.format

(* ------ identifiers ------ *)
module Id = struct
  (* identifiers: parsing information and a string *)
  type t = Info.t * string 
  (* constructor *)
  let mk i s = (i,s)
  (* accessors *)
  let info_of_t (i,_) = i
  let string_of_t (_,s) = s
  (* comparisons *)
  let compare (_,x1) (_,x2) = compare x1 x2
  let equal i1 i2 = compare i1 i2 = 0
  (* modifiers *)
  let prime (i,x) = (i,x ^ "'")
  (* constants *)
  let wild = (Info.M "_", "_")
  (* modules *)
  type this_t = t
  module Set = 
    Set.Make(
      struct
        type t = this_t 
        let compare = compare 
      end)
end

(* ------ qualified identifiers ------ *)
module Qid = struct
  (* qualified identifiers: list of qualifiers, and an identifier *)
  type t = Id.t list * Id.t  
  (* constructor *)
  let mk qs x = (qs,x)
  let t_of_id x = mk [] x
  (* accessors *)
  let info_of_t q = match q with
    | ([],x) -> Id.info_of_t x
    | (x1::_,x) -> Info.merge_inc (Id.info_of_t x1) (Id.info_of_t x)
  let qs_of_t (qs,_) = qs
  let id_of_t (_,x) = x                 
  let string_of_t (qs,x) = 
    Printf.sprintf "%s%s"
      (Safelist.fold_left 
         (fun acc qi -> Printf.sprintf "%s%s." acc (Id.string_of_t qi)) 
         ""
         qs)    
      (Id.string_of_t x)
  (* comparisons *)
  let compare (qs1,x1) (qs2,x2) = 
    let rec ids_compare xs1 xs2 = match xs1,xs2 with
      | [],[] -> 0
      | _,[]  -> 1
      | [],_  -> -1
      | (x1::t1),(x2::t2) -> 
          let hd_compare = Id.compare x1 x2 in
          if (hd_compare <> 0) then hd_compare 
          else ids_compare t1 t2 in
    ids_compare (qs1@[x1]) (qs2@[x2])
  let equal q1 q2 = (compare q1 q2 = 0)
  (* modifiers *)
  let prime (qs,x) = (qs,Id.prime x)
  (* operations *)      
  let id_dot x1 (qs2,x2) = (x1::qs2,x2)
  let splice_id_dot x1 (qs2,x2) = (qs2@[x1],x2)
  let t_dot_id (qs1,x1) x2 = (qs1@[x1],x2)
  let t_dot_t (qs1,x1) (qs2,x2) = (qs1@x1::qs2,x2)
  let id_prefix q1 il2 = 
    let (is1,i1) = q1 in 
    let il1 = is1 @ [i1] in
      ((Safelist.length il1) <= (Safelist.length il2)) 
      && (Safelist.for_all 
            (fun (i1,i2) -> Id.equal i1 i2)
            (Safelist.combine il1 (Misc.take (Safelist.length il1) il2)))
  (* constants *)
  let mk_mod_t ml x = 
    let i = Info.M (sprintf "%s built-in" x) in 
    let qs = Safelist.map (Id.mk i) ml in 
    (qs, Id.mk i x)
  let mk_native_prelude_t = mk_mod_t ["Native"; "Prelude"] 
  let mk_prelude_t = mk_mod_t ["Prelude"]
  let mk_core_t = mk_mod_t ["Core"]
  let mk_list_t = mk_mod_t ["List"]    
  (* modules *)
  type this_t = t
  module Env = Env.Make(
    struct
      type t = this_t
      let compare = compare
      let to_string = string_of_t
    end) 
  module Set = 
    Set.Make(
      struct
        type t = this_t 
        let compare = compare 
      end)
end

type blame = Blame of Info.t * bool 

let mk_blame i = Blame (i,true)

let invert_blame (Blame(i,b)) = Blame (i,not b)

let string_of_blame (Blame(i,b)) = sprintf "<<%s:%b>>" (Info.string_of_t i) b

(* ----- sorts, parameters, expressions ----- *)
type sort = 
    (* base sorts *)
    | SUnit                           (* unit *)
    | SBool                           (* booleans *)
    | SInteger                        (* integers *)
    | SChar                           (* chars *)
    | SString                         (* strings *)
    | SRegexp                         (* regular expressions *)
    | SLens                           (* lenses *)
    | SCanonizer                      (* canonizers *)

    (* products and datatypes (sums) *)
    | SProduct of sort * sort         (* products *)
    | SData of sort list * Qid.t      (* data types *)

    (* dependent and refinement sorts *)
    | SFunction of Id.t * sort * sort (* dependent functions *)
    | SRefine of Id.t * sort * exp    (* refinements *)

    (* variables and universals *)
    | SVar of Id.t                    (* variables *)
    | SForall of Id.t * sort          (* universals *)
 
(* parameters *)
and param = Param of Info.t * Id.t * sort

(* variable bindings *)
and binding = Bind of Info.t * pat * sort option * exp 

(* expression syntax *)
and exp = 
    (* lambda calculus *)
    | EApp  of Info.t * exp * exp 
    | EVar  of Info.t * Qid.t 
    | EOver of Info.t * op * exp list 
    | EFun  of Info.t * param * sort option * exp 
    | ELet  of Info.t * binding * exp 

    (* err... System F rather *)
    | ETyFun of Info.t * Id.t * exp 
    | ETyApp of Info.t * exp * sort

    (* with products, case *)
    | EPair of Info.t * exp * exp 
    | ECase of Info.t * exp * (pat * exp) list * sort

    (* coercions *)
    | ECast of Info.t * sort * sort * blame * exp 
        
    (* unit, strings, ints, character sets *)
    | EUnit    of Info.t  
    | EBoolean of Info.t * bool
    | EInteger of Info.t * int    
    | EChar    of Info.t * Bstring.sym
    | EString  of Info.t * Bstring.t 
    | ECSet    of Info.t * bool * (Bstring.sym * Bstring.sym) list 

(* overloaded operators *)
and op = 
  | OIter of int * int
  | ODot
  | OTilde
  | OMinus
  | OBar
  | OAmp
  | OBarBar
  | OAmpAmp
  | ODarrow
  | ODeqarrow
  | OEqual
  | OLt
  | OLeq
  | OGt
  | OGeq

(* patterns *)
and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t * sort option
  | PVnt of Info.t * Qid.t * pat option 
  | PPar of Info.t * pat * pat

let sort_of_param p0 = match p0 with
  | Param(_,_,s) -> s

let id_of_param p0 = match p0 with
  | Param(_,x,_) -> x

let pat_of_binding b0 = match b0 with 
  | Bind(_,p,_,_) -> p

let exp_of_binding b0 = match b0 with 
  | Bind(_,_,_,e) -> e
  
(* test results *)
type test_result =
    | TestError
    | TestPrint
    | TestEqual of exp
    | TestSortPrint of sort option
    | TestSortEqual of sort 

(* declarations *)
type decl = 
    | DLet  of Info.t * binding 
    | DType of Info.t * Id.t list * Qid.t * (Id.t * sort option) list 
    | DMod  of Info.t * Id.t * decl list 
    | DTest of Info.t * exp * test_result

(* modules *)
type modl = Mod of Info.t * Id.t * Qid.t list * decl list

(* infix constructor for non-dependent functions *)
let (^>) s1 s2 = SFunction(Id.wild,s1,s2)

(* accessors *)
let info_of_exp = function
  | EApp     (i,_,_)     -> i
  | EVar     (i,_)       -> i
  | EOver    (i,_,_)     -> i
  | EFun     (i,_,_,_)   -> i
  | ELet     (i,_,_)     -> i 
  | ETyFun   (i,_,_)     -> i
  | ETyApp   (i,_,_)     -> i
  | EPair    (i,_,_)     -> i
  | ECase    (i,_,_,_)   -> i
  | ECast    (i,_,_,_,_) -> i 
  | EUnit    (i)         -> i
  | EBoolean (i,_)       -> i
  | EInteger (i,_)       -> i    
  | EChar    (i,_)       -> i 
  | EString  (i,_)       -> i
  | ECSet    (i,_,_)     -> i
      
let info_of_pat = function
  | PWld (i)     -> i
  | PUnt (i)     -> i
  | PBol (i,_)   -> i
  | PInt (i,_)   -> i
  | PStr (i,_)   -> i
  | PVar (i,_,_)   -> i 
  | PVnt (i,_,_) -> i
  | PPar (i,_,_) -> i

let info_of_module = function
  | Mod(i,_,_,_) -> i

let id_of_module = function
  | Mod(_,x,_,_) -> x

let pat_vars = 
  let rec aux acc = function
  | PWld (_)     
  | PUnt (_)     
  | PBol (_,_)   
  | PInt (_,_)   
  | PStr (_,_)    -> acc
  | PVar (_,x,so)  -> Id.Set.add x acc 
  | PVnt (_,_,None) -> acc
  | PVnt (_,_,Some p) -> aux acc p
  | PPar (_,p1,p2) -> aux (aux acc p1) p2 in 
  aux Id.Set.empty

(* These are big and ugly, but they're simple... and substitution is
tricky enough that I think it's better to have them written out
explicitly and (I hope) carefully. *)

(* generic Safelist.assoc stuff *)
let rec gen_assoc eq x = function
  | [] -> raise Not_found
  | (y,s)::rest -> if eq x y then s else gen_assoc eq x rest

(* FREE SORT VARIABLES *)
let rec free_svars_pat acc = function
  | PVar(_,_,Some s) -> free_svars_sort acc s
  | PVar(_,_,None)   -> acc
  | PVnt(_,_,Some p) -> free_svars_pat acc p
  | PVnt(_,_,None)   -> acc
  | PPar(_,p1,p2)    -> free_svars_pat (free_svars_pat acc p1) p2
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> acc
and free_svars_sort acc = function
  | SVar a -> Id.Set.add a acc 
  | SFunction(x,s1,s2) -> 
      free_svars_sort (free_svars_sort acc s1) s2 
  | SProduct(s1,s2) -> 
      free_svars_sort (free_svars_sort acc s1) s2
  | SData(sl,qx)       -> 
      Safelist.fold_left free_svars_sort acc sl
  | SRefine(x,s1,e2) -> 
      free_svars_exp (free_svars_sort acc s1) e2
  | SForall(a,s1) -> 
      Id.Set.remove a (free_svars_sort acc s1)
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer ->
      acc
and free_svars_exp acc = function
  | EApp(i,e1,e2) -> 
      free_svars_exp (free_svars_exp acc e1) e2
  | EOver(i,o,el) -> 
      Safelist.fold_left free_svars_exp acc el
  | EFun(i,Param(ip,x1,s2),so3,e4) -> 
      let acc2 = free_svars_sort acc s2 in 
      let acc3 = match so3 with None -> acc2 | Some s3 -> free_svars_sort acc s3 in 
      let acc4 = free_svars_exp acc3 e4 in 
      acc4
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let acc1 = free_svars_pat acc p1 in
      let acc2 = match so2 with None -> acc1 | Some s2 -> free_svars_sort acc1 s2 in 
      let acc3 = free_svars_exp acc2 e3 in 
      let acc4 = free_svars_exp acc3 e4 in 
      acc4
  | ETyFun(i,a,e1) -> 
      Id.Set.remove a (free_svars_exp acc e1)
  | ETyApp(i,e1,s1) -> 
      let acc2 = free_svars_sort acc s1 in 
      let acc1 = free_svars_exp acc2 e1 in 
      acc1
  | ECast(i,f1,t2,b,e3) ->
      let acc1 = free_svars_sort acc f1 in 
      let acc2 = free_svars_sort acc1 t2 in 
      let acc3 = free_svars_exp acc2 e3 in 
      acc3 
  | EPair(i,e1,e2) -> 
      free_svars_exp (free_svars_exp acc e1) e2
  | ECase(i,e1,cl,s3) -> 
      let accl = 
      Safelist.fold_left 
        (fun accj (pi,ei) -> free_svars_exp (free_svars_pat accj pi) ei)
        acc cl in 
      let acc1 = free_svars_exp accl e1 in 
      let acc3 = free_svars_sort acc1 s3 in 
      acc3
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ | EVar _ -> acc

(* SORT SUBSTITUTION *)
let free_svars_in_subst subst = 
  Safelist.fold_left 
    (fun acc (x,s) -> free_svars_sort (Id.Set.add x acc) s)
    Id.Set.empty subst

let rec fresh_svar subst a = 
  let clashes = free_svars_in_subst subst in 
  let rec aux a = 
    if Id.Set.mem a clashes then aux (Id.prime a)
    else a in 
  aux a 

let rec subst_svars_pat subst p0 = match p0 with
  | PVar(i,x,Some s) ->       
      let new_s = subst_svars_sort subst s in 
      PVar(i,x,Some new_s)
  | PVar(i,x,None) -> p0
  | PVnt(i,qx,Some p) -> 
      let new_p = subst_svars_pat subst p in       
      PVnt(i,qx,Some new_p)
  | PVnt(i,qx,None) -> p0
  | PPar(i,p1,p2) -> 
      let new_p1 = subst_svars_pat subst p1 in  
      let new_p2 = subst_svars_pat subst p2 in  
      PPar(i,new_p1,new_p2) 
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> p0
and subst_svars_sort subst s0 = match s0 with 
  | SVar a -> 
      (try gen_assoc Id.equal a subst with Not_found -> s0)
  | SFunction(x,s1,s2) -> 
      let new_s1 = subst_svars_sort subst s1 in 
      let new_s2 = subst_svars_sort subst s2 in 
      SFunction(x,new_s1,new_s2)
  | SProduct(s1,s2) -> 
      let new_s1 = subst_svars_sort subst s1 in 
      let new_s2 = subst_svars_sort subst s2 in 
      SProduct(new_s1,new_s2)
  | SData(sl,qx) -> 
      let new_sl = Safelist.map (subst_svars_sort subst) sl in 
      SData(new_sl,qx)
  | SRefine(x,s1,e2) -> 
      let new_s1 = subst_svars_sort subst s1 in 
      let new_e2 = subst_svars_exp subst e2 in 
      SRefine(x,new_s1,new_e2)
  | SForall(a,s1) -> 
      let fresh_a = fresh_svar subst a in 
      let safe_s1 = if fresh_a = a then s1 else subst_svars_sort [(a,SVar fresh_a)] s1 in 
      let new_s1 = subst_svars_sort subst safe_s1 in
      SForall(fresh_a,new_s1)
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer -> 
      s0
and subst_svars_exp subst e0 = match e0 with 
  | EApp(i,e1,e2) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_e2 = subst_svars_exp subst e2 in 
      EApp(i,new_e1,new_e2) 
  | EOver(i,o,el) -> 
      let new_el = Safelist.map (subst_svars_exp subst) el in 
      EOver(i,o,new_el)
  | EFun(i,Param(ip,x1,s2),so3,e4) -> 
      let new_s2 = subst_svars_sort subst s2 in 
      let new_so3 = Misc.map_option (fun s3 -> subst_svars_sort subst s3) so3 in 
      let new_e4 = subst_svars_exp subst e4 in 
      EFun(i,Param(ip,x1,new_s2),new_so3,new_e4) 
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let new_p1 = subst_svars_pat subst p1 in 
      let new_so2 = Misc.map_option (fun s2 -> subst_svars_sort subst s2) so2 in 
      let new_e3 = subst_svars_exp subst e3 in 
      let new_e4 = subst_svars_exp subst e4 in 
      ELet(i,Bind(ib,new_p1,new_so2,new_e3),new_e4)
  | ETyFun(i,a,e1) -> 
      let fresh_a = fresh_svar subst a in 
      let safe_e1 = if fresh_a = fresh_a then e1 else subst_svars_exp [(a,SVar fresh_a)] e1 in 
      let new_e1 = subst_svars_exp subst safe_e1 in
      ETyFun(i,fresh_a,new_e1) 
  | ETyApp(i,e1,s2) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_s2 = subst_svars_sort subst s2 in 
      ETyApp(i,new_e1,new_s2) 
  | ECast(i,f1,t2,b,e3) ->
      let new_f1 = subst_svars_sort subst f1 in 
      let new_t2 = subst_svars_sort subst t2 in 
      let new_e3 = subst_svars_exp subst e3 in 
      ECast(i,new_f1,new_t2,b,new_e3)
  | EPair(i,e1,e2) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_e2 = subst_svars_exp subst e2 in 
      EPair(i,new_e1,new_e2)
  | ECase(i,e1,cl,s3) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_cl = 
        Safelist.map 
          (fun (pi,ei) -> 
             let new_pi = subst_svars_pat subst pi in 
             let new_ei = subst_svars_exp subst ei in 
             (new_pi,new_ei))
          cl in 
      let new_s3 = subst_svars_sort subst s3 in 
      ECase(i,new_e1,new_cl,new_s3)
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ | EVar _ -> e0

(* FREE EXPRESSION VARIABLES *)
let qvs_of_is s = Id.Set.fold (fun xi acc -> Qid.Set.add (Qid.t_of_id xi) acc) s Qid.Set.empty 

let rec free_evars_pat acc = function
  | PVar(_,_,Some s) -> free_evars_sort acc s
  | PVar(_,_,None)   -> acc
  | PVnt(_,_,Some p) -> free_evars_pat acc p
  | PVnt(_,_,None)   -> acc
  | PPar(_,p1,p2)    -> free_evars_pat (free_evars_pat acc p1) p2
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> acc
and bound_evars_pat acc = function
  | PVar(_,x,_)      -> Id.Set.add x acc
  | PVnt(_,_,Some p) -> bound_evars_pat acc p
  | PVnt(_,_,None)   -> acc
  | PPar(_,p1,p2)    -> bound_evars_pat (bound_evars_pat acc p1) p2
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> acc
and free_evars_sort acc = function
  | SFunction(x,s1,s2) -> 
      let acc2 = free_evars_sort acc s2 in 
      let acc1 = free_evars_sort (Qid.Set.remove (Qid.t_of_id x) acc2) s1 in 
      acc1 
  | SProduct(s1,s2) -> 
      free_evars_sort (free_evars_sort acc s1) s2
  | SData(sl,qx)       -> 
      Safelist.fold_left free_evars_sort acc sl
  | SRefine(x,s1,e2) -> 
      let acc2 = free_evars_exp acc e2 in 
      let acc1 = free_evars_sort (Qid.Set.remove (Qid.t_of_id x) acc2) s1 in 
      acc1 
  | SForall(a,s1) -> 
      free_evars_sort acc s1
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer | SVar _ -> 
      acc
and free_evars_exp acc = function
  | EVar(i,q) -> 
      Qid.Set.add q acc
  | EApp(i,e1,e2) -> 
      free_evars_exp (free_evars_exp acc e1) e2
  | EOver(i,o,el) -> 
      Safelist.fold_left free_evars_exp acc el
  | EFun(i,Param(ip,x1,s2),so3,e4) -> 
      let acc3 = match so3 with None -> acc | Some s3 -> free_evars_sort acc s3 in 
      let acc4 = free_evars_exp acc3 e4 in 
      free_evars_sort (Qid.Set.remove (Qid.t_of_id x1) acc4) s2
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let acc1 = free_evars_pat acc p1 in
      let acc2 = match so2 with None -> acc1 | Some s2 -> free_evars_sort acc1 s2 in 
      let acc3 = free_evars_exp acc2 e3 in 
      let p1_bvars = qvs_of_is (bound_evars_pat Id.Set.empty p1) in 
      let acc4 = free_evars_exp (Qid.Set.diff acc3 p1_bvars) e4 in 
      acc4
  | ETyFun(i,a,e1) -> 
      free_evars_exp acc e1
  | ETyApp(i,e1,s1) -> 
      let acc2 = free_evars_sort acc s1 in 
      let acc1 = free_evars_exp acc2 e1 in 
      acc1
  | ECast(i,f1,t2,b,e3) ->
      let acc1 = free_evars_sort acc f1 in 
      let acc2 = free_evars_sort acc1 t2 in 
      let acc3 = free_evars_exp acc2 e3 in 
      acc3 
  | EPair(i,e1,e2) -> 
      free_evars_exp (free_evars_exp acc e1) e2
  | ECase(i,e1,cl,s3) -> 
      let accl = 
      Safelist.fold_left 
        (fun accj (pi,ei) -> 
           let acci = free_evars_pat accj pi in            
           let pi_bvars = qvs_of_is (bound_evars_pat Id.Set.empty pi) in 
           free_evars_exp (Qid.Set.diff acci pi_bvars) ei)
        acc cl in 
      let acc1 = free_evars_exp accl e1 in 
      let acc3 = free_evars_sort acc1 s3 in 
      acc3
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ -> acc

(* EXPRESSION SUBSTITUTION *)
let free_evars_in_subst subst = 
  Safelist.fold_left 
    (fun acc (x,e) -> free_evars_exp (Qid.Set.add x acc) e)
    Qid.Set.empty subst

let fresh_evar subst a = 
  let clashes = free_evars_in_subst subst in 
  let rec aux a =     
    if Qid.Set.mem a clashes then aux (Qid.prime a)
    else a in 
  aux a 

let fresh_evar_id subst a = 
  let clashes = free_evars_in_subst subst in 
  let rec aux a =     
    if Qid.Set.mem (Qid.t_of_id a) clashes then aux (Id.prime a)
    else a in 
  aux a 

let fresh_evar_ids subst xs = 
  let clashes = free_evars_in_subst subst in 
  let rec aux a = 
    if Qid.Set.mem (Qid.t_of_id a) clashes then aux (Id.prime a)
    else a in 
  Id.Set.fold (fun xi acc -> (xi,aux xi)::acc) xs [] 

let rec rename_evars_pat subst p0 = match p0 with 
  | PVar(i,x,so) ->  
      (try 
         let new_x = gen_assoc Id.equal x subst in 
         PVar(i,new_x,so) 
       with Not_found -> p0)
  | PVnt(i,qx,Some p) -> 
      let new_p = rename_evars_pat subst p in       
      PVnt(i,qx,Some new_p)
  | PVnt(i,qx,None) -> p0
  | PPar(i,p1,p2) -> 
      let new_p1 = rename_evars_pat subst p1 in  
      let new_p2 = rename_evars_pat subst p2 in  
      PPar(i,new_p1,new_p2) 
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> p0

let rec subst_evars_pat subst p0 = match p0 with
  | PVar(i,x,Some s) ->       
      let new_s = subst_evars_sort subst s in 
      PVar(i,x,Some new_s)
  | PVar(i,x,None) -> p0
  | PVnt(i,qx,Some p) -> 
      let new_p = subst_evars_pat subst p in       
      PVnt(i,qx,Some new_p)
  | PVnt(i,qx,None) -> p0
  | PPar(i,p1,p2) -> 
      let new_p1 = subst_evars_pat subst p1 in  
      let new_p2 = subst_evars_pat subst p2 in  
      PPar(i,new_p1,new_p2) 
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> p0
and subst_evars_sort subst s0 = match s0 with 
  | SFunction(x,s1,s2) ->      
      let new_s1 = subst_evars_sort subst s1 in       
      let fresh_x = fresh_evar_id subst x in 
      let safe_s2 = 
        if x = fresh_x then s2 
        else 
          let qx = Qid.t_of_id x in 
          let subst_x = [(qx,EVar(Id.info_of_t fresh_x,qx))] in 
          subst_evars_sort subst_x s2 in       
      let new_s2 = subst_evars_sort subst safe_s2 in       
      SFunction(fresh_x,new_s1,new_s2)
  | SProduct(s1,s2) -> 
      let new_s1 = subst_evars_sort subst s1 in 
      let new_s2 = subst_evars_sort subst s2 in 
      SProduct(new_s1,new_s2)
  | SData(sl,qx) -> 
      let new_sl = Safelist.map (subst_evars_sort subst) sl in 
      SData(new_sl,qx)
  | SRefine(x,s1,e2) -> 
      let fresh_x = fresh_evar_id subst x in 
      let safe_e2 = 
        if fresh_x = x then e2 
        else 
          let i = Id.info_of_t x in 
          let qx = Qid.t_of_id x in 
          let fresh_qx = Qid.t_of_id fresh_x in 
          let subst_x = [(qx,EVar(i,fresh_qx))] in 
          subst_evars_exp subst_x e2 in                 
      let new_s1 = subst_evars_sort subst s1 in 
      let new_e2 = subst_evars_exp subst safe_e2 in 
      SRefine(fresh_x,new_s1,new_e2)
  | SForall(a,s1) -> 
      let new_s1 = subst_evars_sort subst s1 in 
      SForall(a,new_s1)
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer | SVar _ -> 
      s0
and subst_evars_exp subst e0 = match e0 with 
  | EVar(_,x) -> 
      (try gen_assoc Qid.equal x subst with Not_found -> e0)
  | EApp(i,e1,e2) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_e2 = subst_evars_exp subst e2 in 
      EApp(i,new_e1,new_e2) 
  | EOver(i,o,el) -> 
      let new_el = Safelist.map (subst_evars_exp subst) el in 
      EOver(i,o,new_el)
  | EFun(i,Param(ip,x1,s2),so3,e4) -> 
      let fresh_x1 = fresh_evar_id subst x1 in 
      let new_s2 = subst_evars_sort subst s2 in 
      let safe_so3,safe_e4 = 
        if fresh_x1 = x1 then (so3,e4)
        else
          let i = Id.info_of_t x1 in 
          let qx1 = Qid.t_of_id x1 in 
          let fresh_qx1 = Qid.t_of_id fresh_x1 in 
          let subst_x1 = [(qx1,EVar(i,fresh_qx1))] in 
          (Misc.map_option (subst_evars_sort subst_x1) so3,
           subst_evars_exp subst_x1 e4) in 
      let new_so3 = Misc.map_option (subst_evars_sort subst) safe_so3 in 
      let new_e4 = subst_evars_exp subst safe_e4 in 
      EFun(i,Param(ip,fresh_x1,new_s2),new_so3,new_e4) 
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let p1_vars = bound_evars_pat Id.Set.empty p1 in 
      let p1_fresh_vars = fresh_evar_ids subst p1_vars in 
      let safe_p1,safe_so2,safe_e3,safe_e4 = 
        if Safelist.for_all (fun (xi,fresh_xi) -> xi = fresh_xi) p1_fresh_vars then 
          (p1,so2,e3,e4)
        else
          let subst_pvars = 
            Safelist.map 
              (fun (xi,fresh_xi) -> 
                 (Qid.t_of_id xi, EVar(i,Qid.t_of_id fresh_xi))) 
              p1_fresh_vars in
          (rename_evars_pat p1_fresh_vars p1,
           Misc.map_option (subst_evars_sort subst_pvars) so2,
           subst_evars_exp subst_pvars e3,
           subst_evars_exp subst_pvars e4) in 
      let new_so2 = Misc.map_option (fun s2 -> subst_evars_sort subst s2) safe_so2 in 
      let new_e3 = subst_evars_exp subst safe_e3 in 
      let new_e4 = subst_evars_exp subst safe_e4 in 
      ELet(i,Bind(ib,safe_p1,new_so2,new_e3),new_e4)
  | ETyFun(i,a,e1) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      ETyFun(i,a,new_e1)
  | ETyApp(i,e1,s2) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_s2 = subst_evars_sort subst s2 in 
      ETyApp(i,new_e1,new_s2) 
  | ECast(i,f1,t2,b,e3) ->
      let new_f1 = subst_evars_sort subst f1 in 
      let new_t2 = subst_evars_sort subst t2 in 
      let new_e3 = subst_evars_exp subst e3 in 
      ECast(i,new_f1,new_t2,b,new_e3)
  | EPair(i,e1,e2) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_e2 = subst_evars_exp subst e2 in 
      EPair(i,new_e1,new_e2)
  | ECase(i,e1,cl,s3) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_cl = 
        Safelist.map 
          (fun (pi,ei) -> 
             let pi_vars = bound_evars_pat Id.Set.empty pi in 
             let pi_fresh_vars = fresh_evar_ids subst pi_vars in 
             let safe_pi,safe_ei = 
               if Safelist.for_all (fun (xi,fresh_xi) -> xi = fresh_xi) pi_fresh_vars then 
               (pi,ei)
               else
                 let subst_pvars = 
                   Safelist.map 
                     (fun (xi,fresh_xi) -> 
                        (Qid.t_of_id xi, EVar(i,Qid.t_of_id fresh_xi))) 
                     pi_fresh_vars in
                 (rename_evars_pat pi_fresh_vars pi,
                  subst_evars_exp subst_pvars ei) in                           
             let new_ei = subst_evars_exp subst safe_ei in 
             (safe_pi,new_ei))
          cl in 
      let new_s3 = subst_evars_sort subst s3 in 
      ECase(i,new_e1,new_cl,new_s3)
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _  -> e0

let subst_sort subst s0 = subst_svars_sort subst s0

let subst_exp_in_sort subst s0 = subst_evars_sort subst s0

let free_sort_vars s0 = free_svars_sort Id.Set.empty s0

let free_exp_vars_in_sort s0 = free_evars_sort Qid.Set.empty s0

let rec erase_sort = function
  | SFunction(x,s1,s2) ->       
      SFunction(x,erase_sort s1,erase_sort s2)
  | SProduct(s1,s2) -> 
      SProduct(erase_sort s1, erase_sort s2)
  | SData(sl,qx) ->
      SData(Safelist.map erase_sort sl,qx)
  | SRefine(x,s1,e1) -> 
      erase_sort s1
  | SForall(x,s1) -> 
      SForall(x,erase_sort s1)
  | SUnit | SBool | SInteger | SChar | SString 
  | SRegexp | SLens | SCanonizer | SVar _ as s0 -> 
      s0
