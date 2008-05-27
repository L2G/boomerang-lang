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

type blame = Blame of Info.t 

let mk_blame i = Blame i 

let invert_blame b = b

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

let exp_of_blame i b = 
  let fn,(i1,i2),(j1,j2) = match b with 
    | Blame(Info.I(fn,(i1,i2),(j1,j2))) -> (fn,(i1,i2),(j1,j2))
    | Blame(Info.M s) -> s,(-1,-1),(-1,-1) in     
  let p1 = EPair(i,EInteger(i,i1),EInteger(i,i2)) in 
  let p2 = EPair(i,EInteger(i,j1),EInteger(i,j2)) in 
  EPair(i,EString(i,Bstring.t_of_string fn),EPair(i,p1,p2)) 
