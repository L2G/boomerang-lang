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
    (* base types *)
    | SUnit                           (* unit *)
    | SBool                           (* booleans *)
    | SInteger                        (* integers *)
    | SString                         (* strings *)
    | SRegexp                         (* regular expressions *)
    | SLens                           (* lenses *)
    | SCanonizer                      (* canonizers *)
    (* dependent types *)
    | SFunction of Id.t * sort * sort (* dependent functions *)
    | SData of sort list * Qid.t      (* data types *)
    | SProduct of sort * sort         (* products *)
    | SRefine of Id.t * sort * exp    (* refinements *)
    (* polymorphism *)
    | SVar of Id.t                    (* variables *)
    | SForall of Id.t * sort          (* universals *)
 
(* parameters *)
and param_desc = Param of Id.t * sort

(* variable bindings *)
and binding_desc = Bind of Id.t * sort option * exp 

(* expression syntax *)
and exp_desc = 
    (* lambda calculus *)
    | EApp of exp * exp 
    | EVar of Qid.t 
    | EOver of op * exp list 
    | EFun of param * sort option * exp 
    | ELet of binding * exp 

    (* err... System F rather *)
    | ETyFun of Id.t * exp 
    | ETyApp of exp * sort

    (* with products, case *)
    | EPair of exp * exp 
    | ECase of exp * (pat * exp) list * sort

    (* coercions *)
    | ECast of sort * sort * blame * exp 
        
    (* unit, strings, ints, character sets *)
    | EUnit  
    | EBoolean of bool
    | EInteger of int    
    | EString of Bstring.t 
    | ECSet of bool * (Bstring.sym * Bstring.sym) list 

(* overloaded operators *)
and op = 
  | OIter of int * int
  | ODot
  | OBar
  | OTilde

(* patterns *)
and pat_desc = 
  | PWld 
  | PUnt 
  | PBol of bool
  | PInt of int
  | PStr of string
  | PVar of Id.t 
  | PVnt of Qid.t * pat option 
  | PPar of pat * pat


(* syntax: carries info, ast, and annotation *) 
and ('a,'b) syntax = 
    { info: Info.t;
      desc: 'a;
      mutable annot: 'b }

and exp = (exp_desc,sort option) syntax

and pat = (pat_desc,sort option) syntax

and param = (param_desc,unit) syntax

and binding = (binding_desc,unit) syntax

let rec sort_equal u v = match u,v with 
  | SUnit,SUnit -> true
  | SBool,SBool -> true
  | SInteger,SInteger -> true
  | SString,SString -> true
  | SRegexp,SRegexp -> true
  | SLens,SLens -> true
  | SCanonizer,SCanonizer -> true
  | SFunction(x,u1,u2),SFunction(y,v1,v2) -> 
      Id.equal x y && sort_equal u1 v1 && sort_equal u2 v2
  | SData(sl1,x),SData(sl2,y) -> 
      (try Qid.equal x y && 
         Safelist.for_all (fun (s1,s2) -> sort_equal s1 s2) (Safelist.combine sl1 sl2) 
       with _ -> false)
  | SProduct(u1,u2),SProduct(v1,v2) -> 
      sort_equal u1 v1 && sort_equal u2 v2
  | SRefine(x1,s1,e1),SRefine(x2,s2,e2) -> 
      Id.equal x1 x2 && sort_equal s1 s2 && 
        e1 = e2 (* can we do better? this is finer than syntactic equality :-/ *)
  | SVar(x),SVar(y) -> 
      Id.equal x y
  | SForall(x1,s1),SForall(x2,s2) -> 
      Id.equal x1 x2 && sort_equal s1 s2
        (* should alpha vary! *)
  | _ -> false        

let mk_exp i e = { info=i; desc=e; annot=None }
let mk_pat i p = { info=i; desc=p; annot=None }
let mk_binding i b = { info=i; desc=b; annot=() }
let mk_param i p = { info=i; desc=p; annot=() }

let mk_annot_exp i e s = { info=i; desc=e; annot=(Some s) }
let mk_annot_pat i p s = { info=i; desc=p; annot=Some s }

let sort_of_param p0 = match p0.desc with
  | Param(_,s) -> s

let id_of_param p0 = match p0.desc with
  | Param(x,_) -> x

let id_of_binding b0 = match b0.desc with 
  | Bind(x,_,_) -> x

let exp_of_binding b0 = match b0.desc with 
  | Bind(_,_,e) -> e
  
(* test results *)
type test_result =
    | TestValue of exp
    | TestError
    | TestShow
    | TestLensType of (exp option * exp option)

(* declarations *)
type decl_desc = 
    | DLet of binding  
    | DType of Id.t list * Qid.t * (Id.t * sort option) list 
    | DMod of Id.t * decl list 
    | DTest of exp * test_result

and decl = (decl_desc,unit) syntax 
          
let mk_decl i d = { info=i; desc=d; annot=() }          
          
(* modules *)
type modl_desc = Mod of Id.t * Id.t list * decl list

and modl = (modl_desc,unit) syntax
          
let mk_mod i m = { info=i; desc=m; annot=() }
          
(* infix constructor for non-dependent functions *)
let (^>) s1 s2 = SFunction(Id.wild,s1,s2)

(* accessors *)
let info_of_exp e0 = e0.info
      
let info_of_pat p0 = p0.info 

let info_of_module m0 = m0.info

let id_of_module m0 = match m0.desc with
  | Mod(x,_,_) -> x
