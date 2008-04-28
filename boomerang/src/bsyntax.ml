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
  let mk_list_t = mk_mod_t ["List"]    
end

(* ----- sorts, parameters, expressions ----- *)

(* base sorts: used to constrained type variables *)
type base_sort = Unt | Str | Int | Reg | Lns | Can

(* sets of base sorts *)          
module BSSet = Set.Make(
  struct
    type t = base_sort
    let compare = Pervasives.compare
  end)

type bs = BSSet.t 

(* sorts *)
type sort = 
    | SUnit                             (* unit *)
    | SString                           (* strings *)
    | SInteger                          (* integers *)
    | SRegexp                           (* regular expressions *)
    | SLens                             (* lenses *)
    | SCanonizer                        (* canonizers *)
    | SFunction of Id.t * sort * sort   (* dependent functions *)
    | SData of sort list * Qid.t        (* data types *)
    | SProduct of sort * sort           (* products *)
    | SVar of svar                      (* sort variables *)
    | SRawVar of Id.t                   (* parsed sort variables *)
    | SRefine of Id.t * sort * exp      (* refinements *)

(* sort variables: a unique identifier and a ref cell *)
and svar = int * sbnd ref

(* sort variable: free, bound to sort, or constrained *)
and sbnd = Fre | Bnd of sort | Con of bs

(* parameters *)
and param = Param of Info.t * Id.t * sort

(* variable bindings *)
and binding = Bind of Info.t * pat * sort option * exp

(* expression syntax *)
and exp_desc = 
    (* lambda calculus *)
    | EApp of exp * exp 
    | EVar of Qid.t 
    | EFun of param * sort option * exp 
    | ELet of binding * exp 

    (* with products, case *)
    | EPair of exp * exp 
    | ECase of exp * (pat * exp) list 
        
    (* unit, strings, ints, character sets *)
    | EUnit  
    | EString of Bstring.t 
    | EInteger of int    
    | ECSet of bool * (Bstring.sym * Bstring.sym) list 

(* patterns *)
and pat_desc = 
  | PWld 
  | PUnt 
  | PVar of Qid.t 
  | PVnt of Qid.t * pat option 
  | PPar of pat * pat

(* syntax: carries info, ast, and annotation *) 
and ('a,'b) syntax = 
    { info: Info.t;
      desc: 'a;
      mutable annot: 'b }

and exp = (exp_desc,sort option) syntax

and pat = (pat_desc,sort option) syntax          
          
let mk_exp i d = { info=i; desc=d; annot=None }

let mk_pat i p = { info=i; desc=p; annot=None }
          
let mk_checked_exp i d s = { info=i; desc=d; annot=Some s }

(* variable sets *)
module VSet = Set.Make(
  struct
    type t = Id.t
    let compare = Id.compare 
  end)
  
(* sort variable sets *)
module SVSet = Set.Make(
  struct
    type t = svar 
    (* compare uids *)    
    let compare (x,_) (y,_) = Pervasives.compare x y
  end)

(* sort schemes: used in sort checking environments *)
type scheme = SVSet.t * sort

(* mk_scheme: construct a scheme from a svar list and a sort *)
let mk_scheme svl s = 
  let svs = Safelist.fold_left (fun s svi -> SVSet.add svi s) SVSet.empty svl in 
  (svs,s)

(* scheme_of_sort: convert a sort to a scheme *)
let scheme_of_sort s = mk_scheme [] s

(* sort_or_scheme: used in sort checking environments *)
type sort_or_scheme = Sort of sort | Scheme of scheme 

(* test results *)
type test_result =
    | TestValue of exp
    | TestError
    | TestShow
    | TestSort of sort option
    | TestLensType of (exp option * exp option)

(* declarations *)
type decl_desc = 
    | DLet of binding  
    | DType of sort list * Qid.t * (Id.t * sort option) list 
    | DMod of Id.t * decl list 
    | DTest of exp * test_result

and decl = (decl_desc,sort option) syntax 
          
let mk_decl i d = { info=i; desc=d; annot=None }          
        
let mk_checked_decl i d s = { info=i; desc=d; annot=Some s } 
  
(* modules *)
type modl_desc = Mod of Id.t * Id.t list * decl list

and modl = (modl_desc,unit) syntax
          
let mk_mod i m = { info=i; desc=m; annot=() }
          
(* infix constructor for non-dependent functions *)
let (^>) s1 s2 = SFunction(Id.wild,s1,s2)

(* free sort variables *)
let free_svs i s0 = 
  let rec go acc = function
    | SVar(sv1) -> 
        let (x1,sor1) = sv1 in 
        begin match !sor1 with 
          | Bnd s1 -> go acc s1
          | _    -> SVSet.add sv1 acc 
        end
    | SUnit | SString | SInteger | SRegexp | SLens | SCanonizer -> acc
    | SFunction(_,s1,s2) -> go (go acc s1) s2
    | SProduct(s1,s2)  -> go (go acc s1) s2
    | SData(sl,s1)     -> Safelist.fold_left go acc sl 
    | SRawVar x        -> SVSet.empty
    | SRefine(_,s1,_) -> 
        (* STUB *)
        assert false
  in 
  go SVSet.empty s0

(* free expression variables *)
let free_vars e : VSet.t = 
  (* STUB! *)
  assert false

(* free expression variables in sorts *)
let free_vars_sort s : VSet.t = 
  (* STUB! *)
  VSet.empty

(* accessors *)
let info_of_exp e0 = e0.info
      
let info_of_pat p0 = p0.info 

let info_of_module m0 = m0.info

let id_of_module m0 = match m0.desc with
  | Mod(x,_,_) -> x

let sort_of_param = function
  | Param(_,_,s) -> s

let id_of_param = function
  | Param(_,x,_) -> x



