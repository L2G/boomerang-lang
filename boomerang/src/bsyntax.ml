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
(* $Id$                                                                        *)
(*******************************************************************************)

(* ----- imports and abbreviations ----- *)
type i = Info.t
let (@) = Safelist.append 
let sprintf = Printf.sprintf
let msg = Util.format

(* ------ identifiers ------ *)
module Id = struct
  type t = i * string 
  let info_of_t (i,_) = i
  let string_of_t (_,s) = s
  let mk i s = (i,s)
  let compare (_,x1) (_,x2) = compare x1 x2
  let equal i1 i2 = compare i1 i2 = 0
  let wild = (Info.M "_", "_")
end

(* ------ qualified identifiers ------ *)
module Qid = struct
  type t = Id.t list * Id.t
  let t_of_id x = ([],x)
  let id_dot x1 (qs2,x2) = (x1::qs2,x2)
  let t_dot_id (qs1,x1) x2 = ((qs1@[x1]),x2)
  let splice_id_dot x1 (qs2,x2) = (qs2@[x1],x2)
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
  let equal_ctx ctx q1 q2 = 
    (equal q1 q2) ||
      (Safelist.exists 
         (fun x -> (equal (id_dot x q1) q2) || (equal q1 (id_dot x q2)))
         ctx)

  let id_prefix q1 il2 = 
    let (is1,i1) = q1 in 
    let il1 = is1 @ [i1] in
      ((Safelist.length il1) <= (Safelist.length il2)) 
      && (Safelist.for_all 
            (fun (i1,i2) -> Id.equal i1 i2)
            (Safelist.combine il1 (Misc.take (Safelist.length il1) il2)))
        
  let qualifiers (qs,_) = qs

  let info_of_t = function
    | [],(i,_) -> i
    | (i1,_)::_,(i2,_) -> Info.merge_inc i1 i2
        
  let string_of_t (qs,i) = 
    Printf.sprintf "%s%s"
      (Safelist.fold_left 
         (fun acc qi -> Printf.sprintf "%s%s." acc (Id.string_of_t qi)) 
         ""
         qs)    
      (Id.string_of_t i)

  let mk_mod_t ml x = 
    let i = Info.M (sprintf "%s built-in" x) in 
    let qs = Safelist.map (Id.mk i) ml in 
    (qs, Id.mk i x)
  let mk_native_prelude_t = mk_mod_t ["Native"; "Prelude"] 
  let mk_prelude_t = mk_mod_t ["Prelude"]
  let mk_list_t = mk_mod_t ["List"]    
end

(* ----- sorts, parameters, expressions ----- *)

(* base sorts, used in constrained set *)
type base_sort = Unt | Str | Reg | Lns | Can

(* base sort sets *)
module BSSet = Set.Make(
  struct
    type t = base_sort
    let compare = Pervasives.compare
  end)

(* sbset_of_sl: convert a list of base_sorts to a BSSet.t *)
let sbset_of_sl l = 
  Safelist.fold_left 
    (fun s si -> BSSet.add si s) 
    BSSet.empty l 

type sort = 
    | SUnit                             (* unit *)
    | SString                           (* strings *)
    | SRegexp                           (* regular expressions *)
    | SLens                             (* lenses *)
    | SCanonizer                        (* canonizers *)
    | SFunction of Id.t * sort * sort   (* dependent functions *)
    | SData of sort list * Qid.t        (* data types *)
    | SProduct of sort * sort           (* products *)
    | SVar of svar                      (* sort variables *)
    | SRawVar of Id.t                   (* parsed sort variables *)
    | SRefine of Id.t * sort * exp      (* refinements *)

and svar = sbnd ref * int

and sbnd = Fre | Bnd of sort | Con of BSSet.t

and param = Param of i * Id.t * sort

and binding = Bind of i * pat * sort option * exp

and desc = 
    (* lambda calculus *)
    | EApp of exp * exp 
    | EVar of Qid.t 
    | EFun of param * sort option * exp 
    | ELet of binding * exp 

    (* with products, sums *)
    | EPair of exp * exp 
    | ECase of exp * (pat * exp) list 
        
    (* constants *)
    | EUnit  
    | EString of Bstring.t 
    | ECSet of bool * (Bstring.sym * Bstring.sym) list 

and pat = 
  | PWld of i
  | PUnt of i
  | PVar of i * Qid.t * sort option
  | PVnt of i * Qid.t * pat option 
  | PPar of i * pat * pat

and exp = 
    { info: i;
      desc: desc;
      sorto: sort option }

let mk_exp i d = { info=i; desc=d; sorto=None }
let mk_checked_exp i d s = { info=i; desc=d; sorto=Some s }

(* variable sets *)
module VSet = 
  Set.Make(struct
             type t = Id.t
             let compare = Id.compare 
           end)

(* sort variable sets *)
module SVSet = Set.Make
(struct
  type t = svar 
  let compare (_,x) (_,y) = Pervasives.compare x y
 end)

(* schemes and sort_or_scheme: used in sort checking environments *)
type scheme = SVSet.t * sort

(* mk_scheme: construct a scheme from a svar list and a sort *)
let mk_scheme svl s = 
  let svs = Safelist.fold_left (fun s svi -> SVSet.add svi s) SVSet.empty svl in 
  (svs,s)

(* scheme_of_sort: convert a sort to a scheme *)
let scheme_of_sort s = mk_scheme [] s

type sort_or_scheme = Sort of sort | Scheme of scheme 

(* test results *)
type test_result =
    | TestValue of exp
    | TestError
    | TestShow
    | TestSort of sort option
    | TestLensType of (exp option * exp option)

(* declarations *)
type decl = 
    | DLet of i * binding  
    | DType of i * sort list * Id.t * (Id.t * sort option) list 
    | DMod of i * Id.t * decl list 
    | DTest of i * exp * test_result
        
(* modules *)
type modl = Mod of i * Id.t * Id.t list * decl list

(* infix constructor for functions *)
let (^>) s1 s2 = SFunction(Id.wild,s1,s2)


(* sv_equal: true iff the two variables are equal *)
let sv_equal (_,x) (_,y) = x=y

(* free sort variables *)
let free_svs i s0 = 
  let rec go acc = function
    | SVar(sv1) -> 
        let (sor1,x1) = sv1 in 
        begin match !sor1 with 
          | Bnd s1 -> go acc s1
          | _    -> SVSet.add sv1 acc 
        end
    | SUnit | SString | SRegexp | SLens | SCanonizer -> acc
    | SFunction(_,s1,s2) -> go (go acc s1) s2
    | SProduct(s1,s2)  -> go (go acc s1) s2
    | SData(sl,s1)     -> Safelist.fold_left go acc sl 
    | SRawVar x        -> SVSet.empty
    | SRefine(_,s1,_) -> assert false
  in 
  go SVSet.empty s0

(* free expression variables *)
let free_vars e : VSet.t = assert false

(* free expression variables in sorts *)
let free_vars_sort s : VSet.t = assert false


(* info accessors *)
let info_of_exp e0 = e0.info
      
let info_of_pat = function
  | PWld(i)     -> i
  | PUnt(i)     -> i
  | PVar(i,_,_)   -> i
  | PVnt(i,_,_) -> i
  | PPar(i,_,_) -> i


let info_of_module = function
  | Mod(i,_,_,_) -> i

let id_of_module = function
  | Mod(_,x,_,_) -> x

let sort_of_param = function
  | Param(_,_,s) -> s

let id_of_param = function
  | Param(_,x,_) -> x



