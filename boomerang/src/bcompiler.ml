(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007-2008                                                     *)
(* J. Nathan Foster and Benjamin C. Pierce                                     *)
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
(* /boomerang/src/compiler.ml                                                  *)
(* Boomerang type checker and interpreter                                      *)
(* $Id$ *)
(*******************************************************************************)

(* --------------- Imports --------------- *)
open Bsyntax
open Bprint
open Bsubst
open Berror
module RS = Bstring
module R = Bregexp
module L = Blenses.DLens
module C = Blenses.Canonizer
module V = Bvalue
module G = Bregistry

(* abbreviations *)
let sprintf = Printf.sprintf  
let msg = Util.format
let (@) = Safelist.append

let v_of_rv = G.value_of_rv 

(* --------------- AST utilities --------------- *)

let mk_var i q = EVar(i,q)
let mk_core_var i s = 
  mk_var i (Qid.mk_core_t s)
let mk_native_prelude_var i s = 
  mk_var i (Qid.mk_native_prelude_t s)
let mk_unit i  = EUnit(i)
let mk_int i n = EInteger(i,n)
let mk_app i e1 e2 = EApp(i,e1,e2)
let mk_app3 i e1 e2 e3 = mk_app i (mk_app i e1 e2) e3
let mk_let i x s1 e1 e2 =
  let b = Bind(i,PVar(i,x,Some s1),None,e1) in 
  ELet(i,b,e2)

let mk_fun i x s e1 =
  let p = Param(i,x,s) in  
  EFun(i,p,None,e1)

let mk_tyfun i a e = ETyFun(i,a,e)

let mk_tyapp i e1 s2 = ETyApp(i,e1,s2)

let mk_if i e0 e1 e2 s =
  ECase(i,e0,[(PBol(i,true),e1);(PBol(i,false),e2)],s)
    
(* --------------- Unit tests --------------- *)
(* unit tests either succeed, yielding a value, or fail with a msg *)
type testresult = OK of Bvalue.t | Error of (unit -> unit)

let tests = Prefs.createStringList
  "test"
  "run unit test for the specified module"
  "run unit tests for the specified module"
let _ = Prefs.alias tests "t"

let test_all = Prefs.createBool "test-all" false
  "run unit tests for all modules"
  "run unit tests for all modules"

(* [check_test m] returns true iff the command line arguments
   '-test-all' or '-test m' are set *)
let check_test ms = 
  Safelist.fold_left 
    (fun r qs -> r or (Qid.id_prefix (Bvalue.parse_qid qs) ms))
    (Prefs.read test_all)
    (Prefs.read tests)

(* --------------- Error Reporting --------------- *)
let debug s_thk = 
  Trace.debug "compiler" (fun () -> msg "@[%s@\n%!@]" (s_thk ()))

let test_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> msg "@[%s: Unit test failed @ " (Info.string_of_t i); 
              msg_thk ();
              msg "@]"))

let run_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> msg "@[%s: Unexpected run-time error @\n"
              (Info.string_of_t i);
              msg_thk ();
              msg "@]"))

(* --------------- Environments --------------- *)
(* compilation environments *)
module type CEnvSig = 
sig
  type t 
  type v
  val empty : Qid.t -> t
  val get_ev : t -> G.REnv.t
  val set_ev : t -> G.REnv.t -> t
  val get_ctx : t -> Qid.t list
  val set_ctx : t -> Qid.t list -> t
  val get_mod : t -> Qid.t 
  val set_mod : t -> Qid.t -> t
  val lookup : t -> Qid.t -> v option
  val lookup_type : t -> Qid.t -> (Qid.t * G.tspec) option
  val lookup_con : t -> Qid.t -> (Qid.t * G.tspec) option
  val update : t -> Qid.t -> v -> t
  val update_list : t -> (Qid.t * v) list -> t
  val update_type : t -> Bsyntax.Id.t list -> Qid.t -> G.tcon list -> t
  val fold : (Qid.t -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

module CEnv : CEnvSig with type v = G.rv = 
struct
  type t = (Qid.t list * Qid.t) * G.REnv.t
  type v = G.rv

  let empty m = (([],m), G.REnv.empty ())

  (* getters and setters *)
  let get_ev cev = let (_,ev) = cev in ev
  let set_ev cev ev = let (os,_) = cev in (os,ev)
  let get_mod cev = let ((_,m),_) = cev in m
  let set_mod cev m = let ((os,_),ev) = cev in ((os,m),ev)
  let get_ctx cev = let ((os,_),_) = cev in os
  let set_ctx cev os = let ((_,m),ev) = cev in ((os,m),ev)

  (* lookup from a cev, then from the library *)
  let lookup_generic lookup_fun lookup_library_fun cev q = 
    let ev = get_ev cev in 
    let ctx = get_ctx cev in 
    let rec aux nctx q2 = match lookup_fun ev q2 with
      | Some r -> Some r
      | None -> begin  match nctx with
          | [] -> None
          | o::orest -> aux orest (Bsyntax.Qid.t_dot_t o q)
        end in 
    match aux ctx q with
      | Some r -> Some r
      | None -> lookup_library_fun ctx q
          
  let lookup cev q = 
    lookup_generic 
      G.REnv.lookup 
      G.lookup_library_ctx 
      cev q 

  let lookup_type cev q = 
    lookup_generic 
      G.REnv.lookup_type
      G.lookup_type_library_ctx 
      cev q 

  let lookup_con cev q = 
    lookup_generic 
      G.REnv.lookup_con
      G.lookup_con_library_ctx 
      cev q 

  let update cev q rv = 
    set_ev cev (G.REnv.update (get_ev cev) q rv)

  let update_list cev qs = 
    Safelist.fold_left 
      (fun cev (q,sv) -> update cev q sv)
      cev qs
            
  let update_type cev svars q cl = 
    set_ev cev (G.REnv.update_type (get_ev cev) svars q cl)

  let fold f cev a = G.REnv.fold f (get_ev cev) a
end
type cenv = CEnv.t

(* sort checking environment *)
module SCEnv : CEnvSig with type v = G.rs = 
struct
  type t = CEnv.t
  type v = G.rs

  let dummy_value = V.Unt (Info.M "dummy value")
  let empty = CEnv.empty        
  let get_ev = CEnv.get_ev
  let set_ev = CEnv.set_ev   
  let get_mod = CEnv.get_mod
  let set_mod = CEnv.set_mod
  let get_ctx = CEnv.get_ctx
  let set_ctx = CEnv.set_ctx

  let lookup sev q = 
    match CEnv.lookup sev q with 
    | None -> None
    | Some (s,_) -> Some s
  let lookup_type = CEnv.lookup_type
  let lookup_con = CEnv.lookup_con 
  let update sev q s = CEnv.update sev q (s,dummy_value)
  let update_list sev qs = 
    Safelist.fold_left
      (fun sev (q,s) -> update sev q s)
      sev qs
  let update_type sev svars q cs = CEnv.update_type sev svars q cs
  let fold f sev a = CEnv.fold (fun q (s,_) a -> f q s a) sev a
end

(* --------------- Sort Checking --------------- *)

let rec fresh_id x s = 
  if Id.Set.mem x s then 
    fresh_id (Id.mk (Id.info_of_t x) (Id.string_of_t x ^ "'")) s
  else x

let rec fresh_qid x s = 
  let q = Qid.t_of_id x in 
  if Qid.Set.mem q s then 
    fresh_qid (Id.mk (Id.info_of_t x) (Id.string_of_t x ^ "'")) s
  else q

(* lookup a constructor *)
let get_con i sev li = 
  match SCEnv.lookup_con sev li with
    | None -> sort_error i 
        (fun () -> msg "@[Unbound@ constructor@ %s@]" (Qid.string_of_t li))
    | Some r -> r

let get_type lookup_type i qx = 
  match lookup_type qx with
    | None -> sort_error i 
        (fun () -> msg "@[Unbound@ type@ %s@]" (Qid.string_of_t qx))
    | Some r -> r

let sl_of_svl svl = Safelist.map (fun svi -> SVar svi) svl 

let inst_cases subst cl = 
  Safelist.map 
    (fun (li,so) -> (li,Misc.map_option (subst_sort subst) so)) cl

(* check if one sort is compatible with another *)
let rec compatible f t = match f,t with
  | SUnit,SUnit       
  | SInteger,SInteger 
  | SBool,SBool       
  | SChar,SChar
  | SString,SString 
  | SRegexp,SRegexp 
  | SLens,SLens 
  | SCanonizer,SCanonizer ->
      true
  | SChar,SString 
  | SChar,SRegexp
  | SChar,SLens
  | SString,SRegexp 
  | SString,SLens 
  | SRegexp,SLens -> 
      true
  | SFunction(x,s11,s12),SFunction(y,s21,s22) -> 
      if (Id.equal x y) then (compatible s11 s21) && (compatible s12 s22)
      else 
        let qx = Qid.t_of_id x in 
        let qy = Qid.t_of_id y in 
        let f_fvs = Qid.Set.add qx (free_exp_vars_in_sort f) in 
        let t_fvs = Qid.Set.add qy (free_exp_vars_in_sort t) in 
        let z = fresh_qid x (Qid.Set.union f_fvs t_fvs) in
        let f_subst = [qx,EVar(Info.M "gensym'd variable",z)] in 
        let t_subst = [qy,EVar(Info.M "gensym'd variable",z)] in 
          (compatible 
             (subst_exp_in_sort f_subst s11) 
             (subst_exp_in_sort t_subst s21))
          && (compatible 
                (subst_exp_in_sort f_subst s12) 
                (subst_exp_in_sort t_subst s22))
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      (compatible s11 s21) && (compatible s12 s22)
  | SData(sl1,qx),SData(sl2,qy) -> 
      let ok,sl12 = 
        try true, Safelist.combine sl1 sl2 
        with Invalid_argument _ -> (false,[]) in 
      Safelist.fold_left
        (fun b (s1i,s2i) -> b && compatible s1i s2i)
        (ok && (Qid.equal qx qy))
        sl12
  | SVar x, SVar y -> 
      Id.equal x y
  | SForall(x,s1),SForall(y,s2) -> 
      if Id.equal x y then compatible s1 s2
      else   
        let f_fvs = free_sort_vars s1 in 
        let t_fvs = free_sort_vars s2 in 
        let z = fresh_id x (Id.Set.union f_fvs t_fvs) in
        let f_subst = [x,SVar z] in 
        let t_subst = [y,SVar z] in 
        (compatible (subst_sort f_subst s1) (subst_sort t_subst s2))
  | SRefine(_,s11,_),s2 -> compatible s11 s2
  | s1,SRefine(_,s21,_) -> compatible s1 s21
  | _ -> false

let skipped = ref 0
let casted = ref 0 

(* precondition: f and t must be compatible!! *)
let rec mk_cast_blame lt i b f t e = 
(*   Util.format "@[mk_cast_blame@\nF: %s@\nT: %s@\nE: %s@]@\n@\n" *)
(*     (string_of_sort f) (string_of_sort t) (string_of_exp e); *)
  let old_casted = incr casted; !casted in   
  let skip () = casted := old_casted; incr skipped in 
  let res = 
    if f = t then (skip (); e)
    else
      match f,t with
      | SUnit,SUnit
      | SBool,SBool
      | SInteger,SInteger
      | SChar,SChar
      | SString,SString
      | SRegexp,SRegexp
      | SLens,SLens 
      | SCanonizer,SCanonizer 
      | SVar(_),SVar(_) -> 
          skip ();
          e 
      | SChar,SString ->           
          mk_app i (mk_native_prelude_var i "string_of_char") e
      | SChar,SRegexp -> 
          mk_app i (mk_native_prelude_var i "str")
            (mk_app i (mk_native_prelude_var i "string_of_char") e)
      | SChar,SLens -> 
          mk_app i (mk_native_prelude_var i "copy")
            (mk_app i (mk_native_prelude_var i "str")
               (mk_app i (mk_native_prelude_var i "string_of_char") e))
      | SString,SRegexp ->  
          mk_app i (mk_native_prelude_var i "str") e
      | SString,SLens -> 
          mk_app i (mk_native_prelude_var i "copy")
            (mk_app i (mk_native_prelude_var i "str") e)
      | SRegexp,SLens -> 
          mk_app i (mk_native_prelude_var i "copy") e
      | SFunction(x,f1,f2), SFunction(y,t1,t2) -> 
          let fn = Id.mk i "fn" in 
          let qx = Qid.t_of_id x in 
          let qy = Qid.t_of_id y in 
          let qfn = Qid.t_of_id fn in 
          let e_x = mk_var i qx in 
          let e_fn = mk_var i qfn in 
          let e_fx = mk_app i e_fn e_x in 
          let c1 = mk_cast_blame lt i (invert_blame b) t1 f1 e_x in 
          let c2 = mk_cast_blame lt i b f2 t2 e_fx in 
            if c1 == e_x && c2 == e_fx then 
              (skip (); e)
            else                
              let cast_f = mk_fun i fn f (mk_fun i x f1 (mk_let i x t1 c1 c2)) in 
              let cast_e = 
                if Id.equal x y then mk_app i cast_f e
                else mk_fun i y t1 (mk_app i (mk_app i cast_f e) (mk_var i qy)) in 
                cast_e 
      | SProduct(f1,f2), SProduct(t1,t2) -> 
          let x = Id.mk i "x" in 
          let y = Id.mk i "y" in 
          let qx = Qid.t_of_id x in 
          let qy = Qid.t_of_id y in 
          let e_x = mk_var i qx in 
          let e_y = mk_var i qy in 
          let c1 = mk_cast_blame lt i b f1 t1 e_x in
          let c2 = mk_cast_blame lt i b f2 t2 e_y in                 
            if c1 == e_x && c2 == e_y then (skip (); e)
            else
              let px = PVar(i,x,Some f1) in
              let py = PVar(i,y,Some f2) in
              ECase(i,e,[ (PPar(i,px,py), EPair(i,c1,c2)) ],t)
      | SData(fl,x),SData(tl,y) when Qid.equal x y -> 
          let _,(svl,cl) = get_type lt i x in               
          let fsubst = Safelist.combine svl fl in 
          let tsubst = Safelist.combine svl tl in 
          let cl_finst = inst_cases fsubst cl in 
          let cl_tinst = inst_cases tsubst cl in
          let x = Id.mk i "x" in 
          let qx = Qid.t_of_id x in 
          let y = Id.mk i "y" in 
          let qy = Qid.t_of_id y in 
          let pl = Safelist.map
            (fun ((li,fio),(_,tio)) -> 
               match fio,tio with 
                 | None,None -> 
                     let pi = PVnt(i,li,None) in 
                     let ei = mk_var i qx in 
                       (pi,ei)
                 | Some fi,Some ti -> 
                     let li_f = 
                       Safelist.fold_right 
                         (fun tj acc -> mk_tyapp i acc tj)
                         tl (mk_var i li) in 
                     let py = PVar(i,y,Some fi) in 
                     let pi = PVnt(i,li,Some py) in 
                     let ei = mk_app i li_f (ECast(i,fi,ti,b,mk_var i qy)) in (* this cast cannot be recursive! *)
                       (pi,ei)
                 | _ -> run_error i (fun () -> msg "@[different@ datatypes@ in@ cast@ expression@]"))
            (Safelist.combine cl_finst cl_tinst) in 
            mk_let i x f e (ECase(i,mk_var i qx,pl,t))
      | _,SRefine(x,t2,e2) -> 
          let qx = Qid.t_of_id x in 
          let e_blame = ETyApp(i,ETyApp(i,mk_core_var i "blame",t2),t) in 
          let e_info = exp_of_blame i b in                         
          let e_t = EString(i,Bstring.empty) in (* EString(i,Bstring.t_of_string (string_of_sort t)) in *)
            mk_let i x t2
              (mk_cast_blame lt i b f t2 e) 
              (mk_if i e2 
                 (mk_var i qx) 
                 (mk_app i 
                    (mk_app i 
                       (mk_app i e_blame e_info)
                       (mk_var i qx))
                    e_t)
                 t) 
      | SRefine(x,f1,e1),t1 -> 
          mk_cast_blame lt i b f1 t1 e
      | SForall(x,f1),SForall(y,t1) ->
          let e_ex = mk_tyapp i e (SVar x) in 
          let c1 = mk_cast_blame lt i b f1 t1 e_ex in 
          if c1 == e_ex then (skip (); e)
          else mk_tyfun i x c1
      | _ -> 
          run_error i 
            (fun () -> 
               msg "@[cannot@ cast@ incompatible@ %s@ to@ %s@]"
                 (string_of_sort f)
                 (string_of_sort t)) in 
    Trace.debug "cast+"
      (fun () -> 
         msg "@[CASTING %s to %s@]@\n" 
           (string_of_sort f)
           (string_of_sort t);
         msg "@[%s ~> %s@]@\n" 
           (string_of_exp e)
           (string_of_exp res));
    res

  
        
let mk_cast lt i f t e = 
  mk_cast_blame lt i (mk_blame i) f t e 

(* static_match: determine if a value with a given sort *could* match
   a pattern; annotate PVars with their sorts, return the list of sort
   bindings for variables. *)
let rec static_match i sev p0 s = 
(*   msg "STATIC_MATCH: %s # %s@\n" (string_of_pat p0) (string_of_sort s); *)
(*   msg "LOOKING UP %s@\n" (Qid.string_of_t li);         *)
(*   msg "QX: %s SVL: %t@\n" (Qid.string_of_t qx) (fun _ -> Misc.format_list "," (format_svar false) svl); *)
(*   msg "INSTANTIATED DATA: %s@\n" (string_of_sort s_expect); *)
  let err p str s = sort_error i 
    (fun () -> msg "@[in@ pattern@ %s:@ expected %s,@ but@ found@ %s@]"
       (string_of_pat p) str (string_of_sort s)) in 
    match p0 with 
      | PWld _ -> 
          Some (p0,[])
      | PVar(i,x,_) -> 
          Some (PVar(i,x,Some s),[(x,s)])
      | PUnt(i) ->
          if not (compatible s SUnit) then err p0 "unit" s;
          Some (p0,[])
      | PInt _ -> 
          if not (compatible s SInteger) then err p0 "int" s;
          Some (p0,[])
      | PBol _ -> 
          if not (compatible s SBool) then err p0 "bool" s;
          Some (p0,[])
      | PStr _ -> 
          if not (compatible s SString) then err p0 "string" s;
          Some (p0,[])
      | PVnt(pi,li,ptio) -> 
          begin match expose_sort s with 
            | SData(sl1,qy) -> 
                (* lookup the constructor from the environment *)
                let qx,(svl,cl) = get_con i sev li in 
                let cl_inst = 
                  if not (Qid.equal qx qy) then     
                    let s_expect = SData(sl_of_svl svl,qx) in 
                    err p0 (string_of_sort s_expect) s
                  else 
                    let subst = Safelist.combine svl sl1 in 
                    inst_cases subst cl in 
                let rec resolve_label li lj = function
                  | [] -> None
                  | o::os -> 
                      let qi = Qid.t_dot_t o li in 
                        if Qid.equal qi lj then Some qi
                        else resolve_label li lj os in
                let rec find_label = function
                  | [] -> None
                  | (lj,sjo)::rest ->                            
                      let go qi = 
                        match ptio,sjo with 
                          | None,None -> 
                              Some(PVnt(pi,qi,ptio),[])
                          | Some pti,Some sj -> 
                              begin 
                                match static_match i sev pti sj with
                                  | Some (new_pti,binds) -> 
                                      Some (PVnt(pi,qi,Some new_pti),binds)
                                  | None -> find_label rest
                              end
                          | _ -> sort_error i 
                              (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                                 (Qid.string_of_t lj)) in 
                        if Qid.equal li lj then go li
                        else match resolve_label li lj (SCEnv.get_ctx sev) with
                          | None -> find_label rest
                          | Some qi -> go qi in 
                  find_label cl_inst                
            | _ -> err p0 "data type" s
          end
            
      | PPar(pi,p1,p2) -> 
          begin match s with 
            | SProduct(s1,s2) -> 
                begin match
                  static_match i sev p1 s1, 
                  static_match i sev p2 s2 
                with 
                  | Some (new_p1,l1),Some(new_p2,l2) -> 
                      Some (PPar(pi,new_p1,new_p2),l1 @ l2)
                  | _ -> None 
                end
            | _ -> err p0 "product" s
          end
            
(* dynamic match: determine if a value *does* match a pattern; return
   the list of value bindings for variables. *)
let rec dynamic_match i p0 v0 = 
(*  msg "DYNAMIC_MATCH [";
  V.format v0;
  msg "] WITH [";
  format_pat p0;
  msg "]@\n";*)
  match p0,v0 with   
  | PWld _,_ -> Some []
  | PVar(_,q,so),_ -> Some [(q,v0,so)]
  | PUnt _,V.Unt(_) -> Some []
  | PInt(_,n1),V.Int(_,n2) -> if n1=n2 then Some [] else None
  | PBol(_,b1),V.Bol(_,b2) -> if b1=b2 then Some [] else None
  | PStr(_,s1),V.Str(_,s2) -> if s1 = RS.string_of_t s2 then Some [] else None
  | PVnt(_,(_,li),pio),V.Vnt(_,_,lj,vjo) -> 
      if Id.equal li lj then 
        (match pio,vjo with 
           | None,None -> Some []
           | Some pi,Some vj -> dynamic_match i pi vj
           | _ -> 
               run_error i (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                              (Id.string_of_t li)))
      else None
  | PPar(_,p1,p2),V.Par(_,v1,v2) -> 
      (match dynamic_match i p1 v1,dynamic_match i p2 v2 with 
         | Some l1,Some l2 -> Some (l1 @ l2)
         | _ -> None)
  | _ -> None 

(* ------ sort resolution ----- *)

(* resolve_sort: resolves QIds in SData *)
let rec resolve_sort i sev s0 = 
  let rec go s0 = match s0 with 
    | SUnit | SBool | SInteger | SChar | SString 
    | SRegexp | SLens | SCanonizer | SVar _ -> 
        s0
    | SFunction(x,s1,s2) -> 
        let new_s1 = go s1 in 
        let sev1 = SCEnv.update sev (Qid.t_of_id x) (G.Sort new_s1) in 
        let new_s2 = resolve_sort i sev1 s2 in  
        SFunction(x,new_s1,new_s2)
    | SProduct(s1,s2)  -> 
        SProduct(go s1,go s2)
    | SData(sl,qx) ->        
        let qx',_ = match SCEnv.lookup_type sev qx with 
          | None -> 
              sort_error i  
                (fun () -> msg "@[cannot@ resolve@ sort@ %s@]" 
                   (Qid.string_of_t qx))
          | Some q -> q in 
          SData(Safelist.fold_left (fun acc si -> go si::acc) [] sl,qx')
    | SForall(x,s1) -> SForall(x,go s1)
    | SRefine(x,s1,e1) -> 
        let sev1 = SCEnv.update sev (Qid.t_of_id x) (G.Sort (erase_sort s1)) in 
        let s1' = resolve_sort i sev1 s1 in 
        let sev2 = SCEnv.update sev (Qid.t_of_id x) (G.Sort s1') in 
        let e1_sort,new_e1 = check_exp sev2 e1 in  
        if not (compatible e1_sort SBool) then
          sort_error i 
            (fun () -> msg "@[in@ refinement: expected@ %s@ but@ found@ %s@]"
               (string_of_sort SBool)
               (string_of_sort e1_sort));
        SRefine(x,s1',new_e1) in 
  go s0

(* ------ sort checking expressions ----- *)
and check_exp sev e0 = 
  Trace.debug "check"
    (fun () -> 
       msg "@[check_exp ";
       format_exp e0;
       msg "@]@\n");
  match e0 with
  | EVar(i,q) ->
      (* lookup q in the environment *)
      let e0_sort = match SCEnv.lookup sev q with
        | Some (G.Sort s) -> 
            (* if it is bound, return the sort *)
            s
        | _ -> 
            (* otherwise raise an exception *)
            sort_error i
              (fun () -> msg "@[%s is not bound@]" 
                 (Qid.string_of_t q)) in 
      (e0_sort,e0)

  | EOver(i,op,es) -> begin 
      let err () = sort_error i 
        (fun () -> msg "@[could@ not@ resolve@ %s@]" (string_of_op op)) in 
      (* rules for overloaded symbols *)
      let iter_rules = 
        [ SRegexp, "regexp_iter";
          SLens, "lens_iter";
          SCanonizer, "canonizer_iter" ] in 
      let bin_rules =
        [ ODot, 
          [ SString, "string_concat";
            SRegexp, "regexp_concat";
            SLens, "lens_concat";
            SCanonizer, "canonizer_concat" ]
        ; OTilde,
          [ SLens, "lens_swap";
            SCanonizer, "canonizer_swap" ] 
        ; OMinus,
          [ SRegexp, "diff";
            SInteger, "minus" ]
        ; OBar,
          [ SRegexp, "regexp_union";
            SLens, "lens_disjoint_union";
            SCanonizer, "canonizer_union" ]
        ; OAmp,    [ SRegexp, "inter" ]
        ; OBarBar, [ SLens, "lens_union";
                     SBool, "lor" ]
        ; OAmpAmp, [ SBool, "land" ]
        ; ODarrow, [ SLens, "set" ]
        ; ODeqarrow, [ SLens, "rewrite" ]
        ; OLt, [ SInteger, "blt" ] 
        ; OLeq, [ SInteger, "bleq" ] 
        ; OGt, [ SInteger, "bgt" ] 
        ; OGeq, [ SInteger, "bgeq" ] ] in 
      (* helper to find rule *)
      let rec find_rule r es = match r with 
        | [] -> None
        | (s,x)::t -> 
            if Safelist.for_all (fun (si,_) -> compatible si s) es then 
              Some x
            else 
              find_rule t es in 
      (* type check es *)
      let new_es = 
        Safelist.fold_right
          (fun ei new_es -> 
             let ei_sort,new_ei = check_exp sev ei in 
             ((ei_sort,new_ei)::new_es))
          es [] in
      (* rewrite the overloaded symbol using the rules above *)
      match op,new_es with
        | OIter(min,max),[e1_sort,new_e1] -> begin 
            match find_rule iter_rules new_es with 
              | Some x ->
                  check_exp sev 
                    (mk_app i 
                       (mk_app i 
                          (mk_app i 
                             (mk_var i (Qid.mk_core_t x))
                             new_e1)
                          (mk_int i min))
                       (mk_int i max)) 
              | None -> err () 
          end
        | OEqual,[e1_sort,new_e1;e2_sort,new_e2] -> 
            (SBool,
             mk_app3 i 
               (mk_tyapp i (mk_var i (Qid.mk_core_t "equals")) e1_sort) 
               new_e1 new_e2)
        | op,[e1_sort,new_e1; e2_sort,new_e2] -> begin
            let rules = try Safelist.assoc op bin_rules with _ -> err () in 
             match find_rule rules new_es with 
               | Some x -> 
                   let new_e = mk_app3 i (mk_var i (Qid.mk_core_t x)) new_e1 new_e2 in
                   check_exp sev new_e
               | None -> err ()
          end
        | _ -> err () 
    end

  | EFun(i,Param(p_i,p_x,p_s),ret_sorto,body) ->
      (* resolve the parameter sort *)
      let new_p_s = resolve_sort p_i sev p_s in 
      (* create the environment for the body *)
      let body_sev = SCEnv.update sev (Qid.t_of_id p_x) (G.Sort new_p_s) in      
      let body_sort,new_body = 
        match ret_sorto with 
          | None -> 
              (* if no return sort declared, just check the body *)
              check_exp body_sev body 
          | Some ret_sort ->
              (* otherwise, resolve the declared return sort *)
              let new_ret_sort = resolve_sort i body_sev ret_sort in 
              (* then check the body *)
              let body_sort,new_body = check_exp body_sev body in
              (* and check that the declared return sort is a subsort of the body sort *)
              if not (compatible body_sort new_ret_sort) then
                sort_error i
                  (fun () ->
                     msg "@[in@ function:@ %s@ expected@ but@ %s@ found@]"
                       (string_of_sort new_ret_sort)
                       (string_of_sort body_sort));
              let cast_body = mk_cast (SCEnv.lookup_type sev) (info_of_exp body) body_sort new_ret_sort new_body in
              (new_ret_sort,cast_body) in
      let e0_sort = SFunction(p_x,new_p_s,body_sort) in
      let new_p = Param(p_i,p_x,new_p_s) in
      let new_e0 = EFun(i,new_p,Some body_sort,new_body) in
      (e0_sort,new_e0)

  | ELet(i,b,e) ->
      (* for let-expressions, check the bindings *)
      let bevs,_,new_b = check_binding sev b in 
      (* use the resulting environment to check the exp *)
      let e_sort,new_e = check_exp bevs e in 
      let new_e0 = ELet(i,new_b,new_e) in 
      (e_sort,new_e0)

  | EPair(i,e1,e2) -> 
      (* for pairs, recursively check e1 and e2 *)
      let e1_sort,new_e1 = check_exp sev e1 in 
      let e2_sort,new_e2 = check_exp sev e2 in 
      let e0_sort = SProduct(e1_sort,e2_sort) in 
      let new_e0 = EPair(i,new_e1,new_e2) in 
      (e0_sort,new_e0)

  | EUnit(_) -> 
      (* units have sort SUnit *)
      (SUnit,e0)

  | EBoolean(_) -> 
      (* boolean constants have sort SBool *)
      (SBool,e0)

  | EInteger(_) -> 
      (* integer constants have sort SInteger *)
      (SInteger,e0)

  | EChar(_) -> 
      (SChar,e0)

  | EString(_) -> 
      (* string constants have sort SString *)
      (SString,e0)

  | ECSet(_) -> 
      (* character sets have sort SRegexp *)
      (SRegexp,e0)

  | EApp(i,e1,e2) ->  
      (* for function applications, check the left-hand expression *)
      let e1_sort,new_e1 = check_exp sev e1 in 
      (* and make sure it is a function sort *)
      begin match e1_sort with 
        | SFunction(x,param_sort,return_sort) -> 
            (* then check the right-hand expression *)
            let e2_sort,new_e2 = check_exp sev e2 in 
            (* and make sure its sort is compatible with the parameter *)
            let i2 = info_of_exp e2 in 
            if not (compatible e2_sort param_sort) then 
              sort_error i2
                (fun () ->                    
                   msg "@[in@ application:@ expected@ %s@ but@ found@ %s@]"
                     (string_of_sort param_sort)
                     (string_of_sort e2_sort));
            (* insert cast if needed *)
            let cast_e2 = mk_cast (SCEnv.lookup_type sev) i2 e2_sort param_sort new_e2 in
            let new_e0 = EApp(i,new_e1,cast_e2) in 
            let e0_sort =
              if Id.equal x Id.wild then return_sort 
              else subst_exp_in_sort [(Qid.t_of_id x,cast_e2)] return_sort in 
(*               msg "@[IN APP: "; format_exp e0; msg "@]@\n"; *)
(*               msg "@[E1_SORT: %s@\n@]" (string_of_sort e1_sort); *)
(*               msg "@[E2_SORT: %s@\n@]" (string_of_sort e2_sort); *)
(*               msg "@[RESULT: %s@\n@]" (string_of_sort e0_sort); *)
(*               msg "@[SUBST_EXP_IN_SORT: %s := %s IN %s ~> %s@]@\n" (Id.string_of_t x) (string_of_exp cast_e2) (string_of_sort return_sort) (string_of_sort e0_sort); *)
            (e0_sort,new_e0)
        | _ -> 
            sort_error (info_of_exp e1)
              (fun () ->              
                 msg "@[in@ application:@ expected@ function@ sort@ but@ found@ %s.@]"
                   (string_of_sort e1_sort))
      end

  | ECase(i,e1,pl,ps) -> 
      (* helper function for printing error messages *)
      let err2 i p s1 s2 = sort_error i (fun () -> msg p s1 s2) in 
      (* resolve the sort *)
      let new_ps = resolve_sort i sev ps in 
      (* check the expression being matched *)
      let e1_sort,new_e1 = check_exp sev e1 in 
      (* fold over the list of patterns and expressions *)
      let new_pl_rev = Safelist.fold_left 
        (fun new_pl_rev (pi,ei) -> 
           match static_match i sev pi e1_sort with 
             | None -> 
                 (* if the branch is useless, raise an exception *)
                 err2 i "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" 
                   (string_of_pat pi) 
                   (string_of_sort e1_sort)
             | Some (new_pi,binds) ->                
                 (* otherwise, extend the environment with bindings for pattern vars *)
                 let ei_sev = Safelist.fold_left 
                   (fun ei_sev (qj,sj) -> SCEnv.update ei_sev (Qid.t_of_id qj) (G.Sort sj))
                   sev binds in 
                 (* sort check the expression *) 
                 let ei_sort,new_ei = check_exp ei_sev ei in
                 (* and check that it is compatible with the sorts of the other branches *)
                 let ii = info_of_exp ei in 
                 if not (compatible ei_sort new_ps) then 
                   err2 ii                     
                     "@[in@ match:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort new_ps)
                     (string_of_sort ei_sort);
                 let cast_ei = mk_cast (SCEnv.lookup_type sev) ii ei_sort new_ps new_ei in
                 (new_pi,cast_ei)::new_pl_rev)
        [] pl in 
      if Safelist.length new_pl_rev = 0 then 
        sort_error i (fun () -> msg "@[empty@ match@ expression@]");          
      let new_e0 = ECase(i,new_e1,Safelist.rev new_pl_rev,new_ps) in 
      (new_ps,new_e0)

  | ETyFun(i,x,e1) -> 
      let e1_sort,new_e1 = check_exp sev e1 in 
      let new_e0 = ETyFun(i,x,new_e1) in 
      let e0_sort = SForall(x,e1_sort) in 
      (e0_sort,new_e0)

  | ETyApp(i,e1,s2) -> 
      let e1_sort,new_e1 = check_exp sev e1 in 
      let new_s2 = resolve_sort i sev s2 in 
      let e0_sort = match e1_sort with 
        | SForall(x,s11) -> subst_sort [x,new_s2] s11
        | _ -> sort_error i
            (fun () -> msg "@[in@ type@ application:@ expected@ universal@ type@ but@ %s@ found.@]"
               (string_of_sort e1_sort)) in 
      let new_e0 = ETyApp(i,new_e1,new_s2) in 
      (e0_sort,new_e0)

  | ECast(i,f,t,b,e) -> 
      let _,new_e = check_exp sev e in 
      let f' = resolve_sort i sev f in 
      let t' = resolve_sort i sev t in 
      let new_e0 = ECast(i,f',t',b,new_e) in 
      let e0_sort = t' in 
      (e0_sort,new_e0)

and check_binding sev b0 = match b0 with
  | Bind(i,p,so,e) ->
      let e_sort,new_e = check_exp sev e in        
      let new_s,cast_e = match so with 
        | None -> (e_sort,new_e)
        | Some s -> 
            let new_s = resolve_sort i sev s in
            if not (compatible e_sort new_s) then 
              sort_error i
                (fun () ->
                   msg "@[in@ let-binding:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort new_s)
                     (string_of_sort e_sort));
            let cast_e = mk_cast (SCEnv.lookup_type sev) (info_of_exp e) e_sort new_s new_e in 
            (new_s,cast_e) in 
      let new_p,xs,bsev = match static_match i sev p new_s with 
        | None -> 
            sort_error i 
              (fun () -> 
                 msg "@[in@ let-binding:@ %s@ does not match@ %s@]"
                   (string_of_pat p)
                   (string_of_sort new_s))
        | Some(new_p,binds) ->             
            let xs_rev,bsev = Safelist.fold_left 
              (fun (xsi,sevi) (xj,sj) -> 
                 let qj = Qid.t_of_id xj in 
                 (qj::xsi,SCEnv.update sevi qj (G.Sort sj)))
              ([],sev) binds in
            (new_p,Safelist.rev xs_rev,bsev) in 
      let new_b = Bind(i,new_p,Some new_s,cast_e) in 
      (bsev,xs,new_b)

(* type check a single declaration *)
let rec check_decl sev ms d0 = match d0 with
  | DLet(i,b) ->
      let bsev,xs,new_b = check_binding sev b in
      let new_d = DLet(i,new_b) in
      (bsev,xs,new_d)
  | DMod(i,n,ds) ->
      let qmn = Qid.t_dot_id (SCEnv.get_mod sev) n in 
      let msev = SCEnv.set_mod sev qmn in 
      let ms = ms @ [n] in 
      (* check the module *)
      let msev,names,new_ds = check_module_aux msev ms ds in
      let nsev, names_rev = Safelist.fold_left 
        (fun (nsev, names) q -> 
           match SCEnv.lookup msev q with
               None -> run_error i
                 (fun () -> msg "@[declaration@ for@ %s@ missing@]"
                    (Qid.string_of_t q))
             | Some s ->
                 (* prefix the qualifiers in each name with n *)
                 let nq = Qid.splice_id_dot n q in
                 (SCEnv.update nsev nq s, nq::names))
        (msev,[]) names in 
      let new_d = DMod(i,n,new_ds) in 
      (nsev,Safelist.rev names_rev,new_d)

  | DType(i,svl,qx,cl) -> 
      (* get module prefix *)
      let qm = SCEnv.get_mod sev in       
      let new_qx = Qid.t_dot_t qm qx in      
      (* install a dummy for qx in environment *)
      let sev2 = SCEnv.update_type sev svl new_qx [] in 
      (* then resolve sorts in cl *)
      let new_cl_rev = 
        Safelist.fold_left  
          (fun acc (x,so) -> 
             let x_so' = match so with 
               | None -> (x,so)
               | Some s -> (x,Some (resolve_sort i sev2 s)) in 
             x_so'::acc)
          [] cl in 
      let new_cl = Safelist.rev new_cl_rev in 
      let new_qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) new_cl in
      (* put the real qx in environment -- note! must discard dummy sev2 *)
      let sev3 = SCEnv.update_type sev svl new_qx new_qcl in
      (* build the datatype *)
      let sx = SData(sl_of_svl svl,new_qx) in
      let mk_univ s = Safelist.fold_right (fun svi acc -> SForall(svi,acc)) svl s in 
      (* add constructors to sev *)
      let sev4,xs_rev = Safelist.fold_left 
        (fun (sevi,acc) (li,sio) ->            
           let li_sort = match sio with 
             | None -> mk_univ sx
             | Some si -> mk_univ (SFunction(Id.wild,si,sx)) in 
           let qli = Qid.t_of_id li in 
           (SCEnv.update sevi qli (G.Sort li_sort),qli::acc))
        (sev3,[]) new_cl_rev in
      let new_d = DType(i,svl,new_qx,Safelist.rev new_cl_rev) in 
      (sev4,Safelist.rev xs_rev,new_d)
          
  | DTest(i,e1,tr) -> 
      (* check the expression *)
      let e1_sort,new_e1 = check_exp sev e1 in
      let new_tr = match tr with 
        | TestError | TestPrint -> tr
        | TestEqual e2 -> 
            (* for values, check that the exps have compatible types *)
            let e2_sort,new_e2 = check_exp sev e2 in 
              if not (compatible e2_sort e1_sort) then
              sort_error (info_of_exp e2)
                (fun () -> 
                   msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort e1_sort)
                     (string_of_sort e2_sort));
            TestEqual new_e2
        | TestSortPrint _ -> TestSortPrint (Some e1_sort)
        | TestSortEqual s -> 
            let s' = resolve_sort i sev s in 
            TestSortEqual s' in            
      let new_d = DTest(i,new_e1,new_tr) in 
      (sev,[],new_d)
          
and check_module_aux sev m ds = 
  let msev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sevi, names, new_ds_rev) di -> 
         let dsev,new_names,new_di = check_decl sevi m di in
         dsev,names@new_names,new_di::new_ds_rev)
      (sev,[],[]) ds in 
  (msev, names, Safelist.rev new_ds_rev)

(* entry point to static analysis / instrumentation *)
let check_module m0 = match m0 with 
  | Mod(i,m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let sev = SCEnv.set_ctx (SCEnv.empty qm) (qm::nctx@G.pre_ctx) in
      let checked_sev,_,checked_ds = check_module_aux sev [m] ds in 
      let res = Mod(i,m,nctx,checked_ds) in 
      Trace.debug "instr+" (fun () -> format_module res; msg "@\n");
      res

(* --------------- Compiler --------------- *)
(* expressions *)
let rec compile_exp cev e0 = match e0 with 
  | EVar(i,q) ->       
      begin match CEnv.lookup cev q with
        | Some(_,v) -> 
            v
        | None -> 
           run_error i 
              (fun () -> msg "@[%s is not bound@]" (Qid.string_of_t q))
      end

  | EOver(i,op,_) -> 
      run_error i
        (fun () ->
           format_exp e0;
           msg "@[unresolved@ overloaded@ operator %s@]" (string_of_op op))

  | EApp(_,e1,e2) ->
      let v1 = compile_exp cev e1 in 
      let v2 = compile_exp cev e2 in 
      (V.get_f v1) v2

  | ELet(_,b,e) -> 
      let bcev,_ = compile_binding cev b in
      compile_exp bcev e
          
  | EFun(i,p,_,e) ->
      let f v =        
        let body_cev = 
          CEnv.update cev 
            (Qid.t_of_id (id_of_param p)) 
            (G.Unknown,v) in 
        compile_exp body_cev e in 
      V.Fun(i,f)

  | EPair(i,e1,e2) -> 
      let v1 = compile_exp cev e1 in 
      let v2 = compile_exp cev e2 in 
      V.Par(i,v1,v2)

  | ECase(i,e1,pl,_) -> 
      let v1 = compile_exp cev e1 in 
      (* first match *)
      let rec find_match = function
        | [] -> 
            run_error i
              (fun () -> 
                 msg "@[match@ failure@]")
        | (pi,ei)::rest -> 
            (match dynamic_match i pi v1 with 
               | None -> find_match rest
               | Some l -> l,ei) in 
      let binds,ei = find_match pl in 
      let qid_binds = Safelist.map 
        (fun (x,v,so) -> 
           let rs = match so with None -> G.Unknown | Some s -> G.Sort s in 
           (Qid.t_of_id x,(rs,v))) binds in         
      let ei_cev = CEnv.update_list cev qid_binds in
      compile_exp ei_cev ei

  | ETyFun(i,_,e) -> 
      compile_exp cev (mk_fun i Id.wild SUnit e)

  | ETyApp(i,e1,s2) -> 
      compile_exp cev (mk_app i e1 (EUnit(i)))

  | EUnit(i) -> 
      V.Unt(i)

  | EBoolean(i,b) -> 
      V.Bol(i,b)

  | EInteger(i,n) -> 
      V.Int(i,n)

  | EChar(i,c) -> 
      V.Chr(i,c)

  | EString(i,s) -> 
      V.Str(i,s)

  | ECSet(i,pos,cs) -> 
      let mk_rx = if pos then R.set else R.negset in 
      V.Rx(i,mk_rx cs)

  | ECast(i,f,t,b,e) -> 
      compile_exp cev (mk_cast_blame (CEnv.lookup_type cev) i b f t e) 

and compile_binding cev b0 = match b0 with
  | Bind(i,p,so,e) -> 
      let v = compile_exp cev e in 
      let xs_rev,bcev = match dynamic_match i p v with
        | None -> run_error i 
            (fun () -> msg "@[in let-binding: %s does not match %s@]"
               (string_of_pat p) (V.string_of_t v))
        | Some binds -> 
            Safelist.fold_left 
              (fun (xsi,cevi) (xi,vi,soi) -> 
                 let qxi = Qid.t_of_id xi in 
                 let rsi = match soi with None -> G.Unknown | Some s -> G.Sort s in  
                 (qxi::xsi,CEnv.update cevi (Qid.t_of_id xi) (rsi,vi)))
              ([],cev) binds in 
      (bcev,Safelist.rev xs_rev)
      
let rec compile_decl cev ms d0 = match d0 with
  | DLet(i,b) ->
      let bcev,xs = compile_binding cev b in
      (bcev,xs)
  | DType(i,svl,qx,cl) -> 
      let sx = SData(sl_of_svl svl,qx) in 
      let mk_univ s = Safelist.fold_right (fun svi acc -> SForall(svi,acc)) svl s in 
      let mk_impl v = Safelist.fold_right (fun _ f -> V.Fun(i,(fun v -> f))) svl v in 
      let new_cev,xs = 
        Safelist.fold_left 
          (fun (cev,xs) (l,so) -> 
             let ql = Qid.t_of_id l in 
             let rv = match so with 
               | None -> 
                   let s = mk_univ sx in 
                   let v = mk_impl (V.Vnt(i,qx,l,None)) in 
                   (G.Sort s,v) 
               | Some s -> 
                   let s = mk_univ (SFunction(Id.wild,s,sx)) in 
                   let f v = V.Vnt(V.info_of_t v,qx,l,Some v) in 
                   let v = mk_impl (V.Fun(i,f)) in 
                   (G.Sort s,v) in 
             (CEnv.update cev ql rv,Qid.t_of_id l::xs))
        (cev,[]) cl in 
      let qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) cl in 
      let new_cev' = CEnv.update_type new_cev svl qx qcl in   
      (new_cev',xs)

  | DMod(i,n,ds) ->
      let m_cev, names = compile_mod_aux cev ms ds in
      let n_cev,names_rev = 
        Safelist.fold_left
          (fun (n_cev, names) q ->
             match CEnv.lookup m_cev q with
               | Some rv ->
                   let nq = Qid.splice_id_dot n q in
                   (CEnv.update n_cev nq rv, nq::names)
               | None -> 
                   run_error i 
                     (fun () -> msg "@[compiled declaration for %s missing@]"
                        (Qid.string_of_t q)))
          (cev,[])
          names in
        (n_cev, Safelist.rev names_rev)

  | DTest(i,e,tr) ->
      if check_test ms then 
        begin
          let vo = 
            try OK(compile_exp cev e)
            with (Error.Harmony_error(err)) -> Error err in 
          match vo,tr with 
            | OK (v),TestPrint ->
                msg "Test result:@ "; 
                V.format v; 
                msg "@\n%!"
            | OK (v),TestSortPrint so -> begin 
                match so with 
                  | Some s -> 
                      msg "Test result:@ "; 
                      format_sort s;
                      msg "@\n%!"
                  | None -> run_error i (fun () -> msg "@[unannotated@ unit@ test@]")
              end
            | Error err, TestPrint 
            | Error err, TestSortPrint _ -> 
                test_error i 
                (fun () -> 
                   msg "Test result: error@\n";
                   err (); 
                   msg "@\n%!")
            | Error _, TestError -> ()
            | OK v1, TestEqual e2 -> 
                let v2 = compile_exp cev e2 in
                if not (V.equal v1 v2) then
                  test_error i 
                    (fun () ->
                       msg "@\nExpected@ "; V.format v2;
                       msg "@ but found@ "; V.format v1; 
                       msg "@\n%!")
            | OK v1, TestSortEqual s2 -> 
                run_error i 
                  (fun () -> 
                     msg "@[unimplemented unit test!@]")
            | Error err, TestEqual e2 -> 
                let v2 = compile_exp cev e2 in 
                  test_error i 
                    (fun () ->
                       msg "@\nExpected@ "; V.format v2; 
                       msg "@ but found an error:@ "; 
                       err (); 
                       msg "@\n%!")
            | Error err, TestSortEqual s2 -> 
                test_error i 
                  (fun () ->
                     msg "@\nExpected@ "; format_sort s2;
                     msg "@ but found an error:@ "; 
                     err (); 
                     msg "@\n%!")
            | OK v, TestError -> 
                test_error i 
                  (fun () ->
                    msg "@\nExpected an error@ "; 
                    msg "@ but found:@ "; 
                    V.format v; 
                    msg "@\n%!")
        end;
      (cev, [])
        
and compile_mod_aux cev ms ds = 
  Safelist.fold_left
    (fun (cev, names) di ->
      let m_cev, new_names = compile_decl cev ms di in
        (m_cev, names@new_names))
    (cev,[])
    ds

let compile_module m0 = match m0 with 
  | Mod(i,m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let cev = CEnv.set_ctx (CEnv.empty qm) (qm::nctx@G.pre_ctx) in
      let new_cev,_ = compile_mod_aux cev [m] ds in
      G.register_env (CEnv.get_ev new_cev) m

let print_stats () = 
  let s = !skipped in 
  let c = !casted in 
    Util.format "@[%d skipped, %d casted = %.1f%%@]@\n" s c (100.0 *. (float_of_int s /. float_of_int (s+c)));           
    
