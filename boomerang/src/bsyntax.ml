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
and binding = Bind of Info.t * Id.t * sort option * exp 

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
  | OBar
  | OTilde

(* patterns *)
and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t 
  | PVnt of Info.t * Qid.t * pat option 
  | PPar of Info.t * pat * pat

let sort_of_param p0 = match p0 with
  | Param(_,_,s) -> s

let id_of_param p0 = match p0 with
  | Param(_,x,_) -> x

let id_of_binding b0 = match b0 with 
  | Bind(_,x,_,_) -> x

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
  | PVar (i,_)   -> i 
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
  | PVar (_,x)    -> Id.Set.add x acc 
  | PVnt (_,_,None) -> acc
  | PVnt (_,_,Some p) -> aux acc p
  | PPar (_,p1,p2) -> aux (aux acc p1) p2 in 
  aux Id.Set.empty

let rec sort_walk 
    (eacc:Qid.Set.t) (sacc:Id.Set.t) 
    (on_evar:Qid.Set.t -> 'a -> Info.t -> Qid.t -> exp * 'a) 
    (on_svar:Id.Set.t -> 'a -> Id.t -> sort * 'a)
    (acc:'a) (s0:sort) : (sort * 'a) = 
  let go = sort_walk eacc sacc on_evar on_svar in
  let add_evar x = 
    let eacc' = Qid.Set.add x eacc in 
    let go' = sort_walk eacc' sacc on_evar on_svar in 
    let go_exp' = exp_walk eacc' sacc on_evar on_svar in 
    (go',go_exp') in 
  let add_svar x = 
    let sacc' = Id.Set.add x sacc in 
    let go' = sort_walk eacc sacc' on_evar on_svar in 
    let go_exp' = exp_walk eacc sacc' on_evar on_svar in 
    (go',go_exp') in 
  match s0 with 
    | SVar(x) -> 
        on_svar sacc acc x
    | SFunction(x,s1,s2) -> 
        let go',_ = add_evar (Qid.t_of_id x) in 
        let new_s1,acc1 = go' acc s1 in 
        let new_s2,acc2 = go' acc1 s2 in 
        let new_s0 = SFunction(x,new_s1,new_s2) in 
        (new_s0,acc2)
    | SProduct(s1,s2) -> 
        let new_s1,acc1 = go acc s1 in 
        let new_s2,acc2 = go acc1 s2 in 
        let new_s0 = SProduct(new_s1,new_s2) in 
        (new_s0,acc2)
    | SData(sl,qx) ->
        let new_sl,new_acc = Safelist.fold_right 
          (fun si (sli,acci) -> 
             let new_si,new_acci = go acci si in 
             (new_si::sli,new_acci)) sl ([],acc) in 
        let new_s0 = SData(new_sl,qx) in 
        (new_s0,new_acc)
    | SRefine(x,s1,e1) -> 
        let go',go_exp' = add_evar (Qid.t_of_id x) in
        let new_s1,acc1 = go' acc s1 in 
        let new_e1,acc2 = go_exp' acc1 e1 in 
        let new_s0 = SRefine(x,new_s1,new_e1) in 
        (new_s0,acc2)
    | SForall(x,s1) -> 
        let go',_ = add_svar x in 
        let new_s1,acc1 = go' acc s1 in 
        let new_s0 = SForall(x,new_s1) in 
        (new_s0,acc1)
    | SUnit | SBool | SInteger | SChar | SString 
    | SRegexp | SLens | SCanonizer -> 
        (s0,acc)

and exp_walk 
    (eacc:Qid.Set.t) (sacc:Id.Set.t) 
    (on_evar:Qid.Set.t -> 'a -> Info.t -> Qid.t -> exp * 'a) 
    (on_svar:Id.Set.t -> 'a -> Id.t -> sort * 'a)
    (acc:'a) (e0:exp) : (exp * 'a) = 
  let add_evar x = 
    let eacc' = Qid.Set.add x eacc in 
    let go' = exp_walk eacc' sacc on_evar on_svar in 
    let go_sort' = sort_walk eacc' sacc on_evar on_svar in 
    (go',go_sort') in 
  let add_svar x = 
    let sacc' = Id.Set.add x sacc in 
    let go' = exp_walk eacc sacc' on_evar on_svar in 
    let go_sort' = sort_walk eacc sacc' on_evar on_svar in 
    (go',go_sort') in 
  let go = exp_walk eacc sacc on_evar on_svar in 
  let go_sort = sort_walk eacc sacc on_evar on_svar in 
    match e0 with 
      | EVar(i,q) -> 
          on_evar eacc acc i q
      | EApp(i,e1,e2) -> 
          let new_e1,acc1 = go acc e1 in 
          let new_e2,acc2 = go acc1 e2 in 
          let new_e0 = EApp(i,new_e1,new_e2) in 
          (new_e0,acc2)
      | EOver(i,o,el) -> 
          let new_el,new_acc = Safelist.fold_right 
          (fun ei (eli,acci) -> 
             let new_ei,new_acci = go acci ei in 
             (new_ei::eli,new_acci)) el ([],acc) in 
          let new_e0 = EOver(i,o,new_el) in 
          (new_e0,new_acc)
      | EFun(i,Param(pi,x1,s1),so2,e1) ->  
          let go',go_sort' = add_evar (Qid.t_of_id x1) in 
          let new_s1,acc1 = go_sort' acc s1 in 
          let new_so2,acc2 = match so2 with 
            | None -> (so2,acc1)
            | Some s2 -> 
                let new_s2,acc2 = go_sort' acc1 s2 in
                (Some new_s2,acc2) in 
          let new_e1,acc3 = go acc2 e1 in 
          let new_e0 = EFun(i,Param(pi,x1,new_s1),new_so2,new_e1) in 
          (new_e0,acc3)
      | ELet(i,Bind(bi,x1,so1,e1),e2) ->           
          let go',go_sort' = add_evar (Qid.t_of_id x1) in 
          let new_so1,acc1 = match so1 with 
            | None -> (so1,acc)
            | Some s1 -> 
                let new_s1,acc1 = go_sort acc s1 in
                (Some new_s1,acc1) in 
          let new_e1,acc2 = go acc1 e1 in 
          let new_e2,acc3 = go' acc2 e2 in 
          let new_e0 = ELet(i,Bind(bi,x1,new_so1,new_e1),new_e2) in 
          (new_e0,acc3)
      | ETyFun(i,x1,e1) -> 
          let go',go_sort' = add_svar x1 in 
          let new_e1,acc1 = go' acc e1 in 
          let new_e0 = ETyFun(i,x1,new_e1) in 
          (new_e0,acc1)
      | ETyApp(i,e1,s1) -> 
          let new_e1,acc1 = go acc e1 in 
          let new_s1,acc2 = go_sort acc1 s1 in 
          let new_e0 = ETyApp(i,new_e1,new_s1) in 
          (new_e0,acc2)
      | ECast(i,f,t,b,e) -> 
          let new_f,acc1 = go_sort acc f in 
          let new_t,acc2 = go_sort acc1 t in 
          let new_e,acc3 = go acc2 e in 
          let new_e0 = ECast(i,new_f,new_t,b,new_e) in 
          (new_e0,acc3)
      | EPair(i,e1,e2) -> 
          let new_e1,acc1 = go acc e1 in 
          let new_e2,acc2 = go acc1 e2 in 
          let new_e0 = EPair(i,new_e1,new_e2) in 
          (new_e0,acc2)
      | ECase(i,e1,cl,s) -> 
          let new_e1,acc1 = go acc e1 in 
          let new_cl,acc2 = 
            Safelist.fold_right 
              (fun (pi,ei) (cli,acci) -> 
                 let xs = pat_vars pi in 
                 let eacc' = Id.Set.fold (fun xi acc -> Qid.Set.add (Qid.t_of_id xi) acc) xs eacc in 
                 let go' = exp_walk eacc' sacc on_evar on_svar in 
                 let new_ei,new_acci = go' acci ei in
                 ((pi,new_ei)::cli, new_acci))
              cl ([],acc1) in 
          let new_s,acc3 = go_sort acc2 s in 
          let new_e0 = ECase(i,new_e1,new_cl,new_s) in 
          (new_e0,acc3)
      | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ -> 
          (e0,acc)

let rec gen_assoc eq x = function
  | [] -> raise Not_found
  | (y,s)::rest -> if eq x y then s else gen_assoc eq x rest

let do_svar subst sacc () x = 
  let res = 
    if Id.Set.mem x sacc then SVar(x)
    else try gen_assoc Id.equal x subst with Not_found -> SVar(x) in 
  (res,())

let dont_svar subst sacc () x = (SVar(x),())

let do_evar subst eacc () i q = 
  let res = 
    if Qid.Set.mem q eacc then EVar(i,q)
    else try gen_assoc Qid.equal q subst with Not_found -> EVar(i,q) in 
  (res,())

let dont_evar subst eacc () i q = (EVar(i,q),())

let subst_sort subst s0 = 
  let new_s0,_ = 
    sort_walk Qid.Set.empty Id.Set.empty (dont_evar []) (do_svar subst) () s0 in 
  new_s0

let subst_exp subst e0 = 
  let new_s0,_ = exp_walk Qid.Set.empty Id.Set.empty (do_evar subst) (dont_svar []) () e0 in 
  new_s0

let subst_exp_in_sort subst s0 = 
  let new_s0,_ = sort_walk Qid.Set.empty Id.Set.empty (do_evar subst) (dont_svar []) () s0 in 
  new_s0

let add_sort_vars sacc fvs x = 
  let new_fvs = 
    if Id.Set.mem x sacc then fvs 
    else Id.Set.add x fvs in 
  (SVar(x),new_fvs)

let ignore_sort_vars sacc fvs x = 
  (SVar(x), fvs)

let add_exp_vars eacc fvs i q = 
  let new_fvs = 
    if Qid.Set.mem q eacc then fvs 
    else Qid.Set.add q fvs in 
    (EVar(i,q),new_fvs)
   
let ignore_exp_vars eacc fvs i q = 
  (EVar(i,q),fvs)

let free_sort_vars s0 = 
  let _,fvs = 
    sort_walk Qid.Set.empty Id.Set.empty 
      ignore_exp_vars add_sort_vars
      Id.Set.empty s0 in 
  fvs

let free_exp_vars_in_sort s0 = 
  let _,fvs = 
    sort_walk Qid.Set.empty Id.Set.empty
      add_exp_vars ignore_sort_vars
      Qid.Set.empty s0 in 
  fvs

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
