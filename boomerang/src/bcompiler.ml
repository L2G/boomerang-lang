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
let get_sort e0 = match e0.annot with
  | None,_ -> run_error e0.info 
      (fun () -> msg "@[Unchecked expression@]")
  | (Some s),_ -> s

let get_decl_sort d0 = match d0.annot with 
  | None -> run_error d0.info 
      (fun () -> msg "@[Unchecked declaration@]")
  | Some s -> s
        
let mk_var i q = mk_exp i (EVar q) 
let mk_unit i  = mk_exp i EUnit
let mk_int i n = mk_exp i (EInteger n)
let mk_app i e1 e2 = mk_exp i (EApp(e1,e2))
let mk_app3 i e1 e2 e3 = mk_app i (mk_app i e1 e2) e3
let mk_let i qx s1 e1 e2 =
  let b = mk_binding i (Bind(qx,s1,e1)) in 
  mk_exp i (ELet(b,e2))

let mk_fun i x s e1 =
  let p = mk_param i (Param(x,s)) in 
  mk_exp i (EFun(p,None,e1))

let mk_pair i e1 e2 = mk_exp i (EPair(e1,e2))

let mk_case i e1 cl = mk_exp i (ECase(e1,cl))

let mk_if i e0 e1 e2 =
  let e = ECase(e0,
                [(mk_pat i (PVnt(Qid.mk_prelude_t "True",None)),e1);
                 (mk_pat i (PVnt(Qid.mk_prelude_t "False",None)),e2)]) in
  mk_exp i e

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
  val get_ctx : t -> Id.t list
  val set_ctx : t -> Id.t list -> t
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
  type t = (Id.t list * Qid.t) * G.REnv.t
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
          | o::orest -> aux orest (Bsyntax.Qid.id_dot o q)
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

  let dummy_value = V.Unt (V.Pos (Info.M "dummy value"))
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
let rec subst_sort subst s0 = match s0 with
  | SVar(x) -> 
      let rec aux = function
        | [] -> s0
        | (y,s)::rest -> if Id.equal x y then s else aux rest in 
      aux subst 
  | SUnit | SBool | SInteger | SString | SRegexp | SLens | SCanonizer -> s0
  | SFunction(x,s1,s2) -> SFunction(x,subst_sort subst s1, subst_sort subst s2)
  | SProduct(s1,s2)    -> SProduct(subst_sort subst s1,subst_sort subst s2)
  | SData(sl,qx)       -> 
      let sl' = Safelist.fold_right (fun si acc -> subst_sort subst si::acc) sl [] in 
        SData(sl',qx)
  | SRefine(x,s1,e1)   -> assert false
  | SForall(x,s)       -> 
      let subst' = Safelist.remove_assoc x subst in 
      SForall(x,subst_sort subst' s) 

(* check if one sort is a subsort of another *)
let rec subsort u v = match u,v with        
  | SUnit,SUnit           -> true
  | SInteger,SInteger     -> true
  | SBool,SBool           -> true
  | SString,SString       -> true
  | SString,SRegexp       -> true
  | SString,SLens         -> true
  | SRegexp,SRegexp       -> true
  | SRegexp,SLens         -> true
  | SLens,SLens           -> true
  | SCanonizer,SCanonizer -> true
  | SFunction(x,s11,s12),SFunction(y,s21,s22) -> 
      let b1 = true || Id.equal x y in 
      let b2 = subsort s21 s11  in
      let b3 = subsort s12 s22 in 
      b1 && b2 && b3
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      let b1 = subsort s11 s21  in
      let b2 = subsort s12 s22 in 
      b1 && b2     
  | SData(sl1,qx),SData(sl2,qy) -> 
      let ok,sl12 = 
        try true, Safelist.combine sl1 sl2 
        with Invalid_argument _ -> (false,[]) in 
      Safelist.fold_left
        (fun b (s1i,s2i) -> b && subsort s1i s2i)
        (ok && (Qid.equal qx qy))
        sl12
  | SVar x, SVar y -> 
      Id.equal x y
  | SForall(x,s1),SForall(y,s2) -> 
      Id.equal x y && subsort s1 s2
  | SRefine(_,s11,_),s2 -> subsort s11 s2
  | s1,SRefine(_,s21,_) -> subsort s1 s21
  | _ -> false

let rec join i u v = 
  let err () = 
    run_error i
      (fun () -> Util.format "@[%s and %s do not join@]"
         (string_of_sort u)
         (string_of_sort v)) in 
  match u,v with 
    | SUnit,SUnit           -> SUnit
    | SBool,SBool           -> SBool
    | SInteger,SInteger     -> SInteger
    | SString,SString       -> SString
    | SString,SRegexp       -> SRegexp
    | SRegexp,SString       -> SRegexp
    | SString,SLens         -> SLens
    | SLens,SString         -> SLens
    | SRegexp,SLens         -> SLens
    | SLens,SRegexp         -> SLens
    | SLens,SLens           -> SLens
    | SCanonizer,SCanonizer -> SCanonizer
  | SFunction(x,s11,s12),SFunction(y,s21,s22) -> 
      if Id.equal x y then 
        try SFunction(x,meet i s11 s12,join i s21 s22) 
        with _ -> err ()
      else err () 
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      begin try SProduct(meet i s11 s12,join i s21 s22) 
      with _ -> err () end
  | SData(sl1,qx),SData(sl2,qy) -> 
      let sl12 = 
        try Safelist.combine sl1 sl2 
        with Invalid_argument _ -> err () in      
        if Qid.equal qx qy then 
          SData(Safelist.map (fun (s1i,s2i) -> join i s1i s2i) sl12,qx)
        else err ()
  | SVar x, SVar y -> 
      if Id.equal x y then u 
      else err ()
  | SForall(x,s1),SForall(y,s2) -> 
      if Id.equal x y then SForall(x,join i s1 s2)
      else err ()
  | SRefine(x,s,e),_ | _,SRefine(x,s,e) -> err ()
  | _ -> err ()
         
and meet i u v = 
  let err () = 
    run_error i
      (fun () -> Util.format "@[%s and %s do not meet@]"
         (string_of_sort u)
         (string_of_sort v)) in       
  match u,v with
    | SUnit,SUnit           -> SUnit
    | SBool,SBool           -> SBool
    | SInteger,SInteger     -> SInteger
    | SString,SString       -> SString
    | SString,SRegexp       -> SString
    | SRegexp,SString       -> SString
    | SString,SLens         -> SString
    | SLens,SString         -> SString
    | SRegexp,SLens         -> SRegexp
    | SLens,SRegexp         -> SRegexp
    | SLens,SLens           -> SLens
    | SCanonizer,SCanonizer -> SCanonizer
    | SFunction(x,s11,s12),SFunction(y,s21,s22) -> 
      if Id.equal x y then 
        try SFunction(x,join i s11 s12,meet i s21 s22) 
        with _ -> err ()
      else err ()
    | SProduct(s11,s12),SProduct(s21,s22) -> 
        begin 
          try SProduct(join i s11 s12,meet i s21 s22) 
          with _ -> err ()
        end
    | SData(sl1,qx),SData(sl2,qy) -> 
        let sl12 = 
          try Safelist.combine sl1 sl2 
          with Invalid_argument _ -> err () in      
          if Qid.equal qx qy then 
            SData(Safelist.map (fun (s1i,s2i) -> meet i s1i s2i) sl12,qx)
          else err ()
    | SVar x,SVar y -> 
        if Id.equal x y then u else err ()
    | SForall(x,s1),SForall(y,s2) -> 
        if Id.equal x y then SForall(x,meet i s1 s2)
        else err ()
    | SRefine(x,s,e),_ | _,SRefine(x,s,e) -> err ()
    | _ -> err ()

(* lookup a constructor *)
let get_con i sev li = 
  match SCEnv.lookup_con sev li with
    | None -> sort_error i 
        (fun () -> msg "@[Unbound@ constructor@ %s@]" (Qid.string_of_t li))
    | Some r -> r

let sl_of_svl svl = Safelist.map (fun svi -> SVar svi) svl 

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
    match p0.desc with 
      | PWld -> 
          Some (p0,[])
      | PVar(x) -> 
          p0.annot <- Some s;
          Some (p0,[(x,s)])
      | PUnt ->
          if not (subsort s SUnit) then err p0 "unit" s;
          Some (p0,[])
      | PInt _ -> 
          if not (subsort s SInteger) then err p0 "int" s;
          Some (p0,[])
      | PBol _ -> 
          if not (subsort s SBool) then err p0 "bool" s;
          Some (p0,[])
      | PStr _ -> 
          if not (subsort s SString) then err p0 "string" s;
          Some (p0,[])

      | PVnt(li,pio) -> 
          begin match s with 
            | SData(sl1,qy) -> 
                (* lookup the constructor from the environment *)
                let qx,(svl,cl) = get_con p0.info sev li in 
                let cl_inst = 
                  if not (Qid.equal qx qy) then     
                    let s_expect = SData(sl_of_svl svl,qx) in 
                    err p0 (string_of_sort s_expect) s
                  else 
                    let subst = Safelist.combine svl sl1 in 
                      Safelist.fold_right 
                        (fun (li,so) acc -> 
                           let so' = Misc.map_option (subst_sort subst) so in 
                             (li,so')::acc) 
                        cl [] in 
                let rec resolve_label li lj = function
                  | [] -> None
                  | o::os -> 
                      let qi = Qid.id_dot o li in 
                        if Qid.equal qi lj then Some qi
                        else resolve_label li lj os in
                let rec find_label = function
                  | [] -> None
                  | (lj,sjo)::rest ->                            
                      let go qi = 
                        match pio,sjo with 
                          | None,None -> 
                              Some(mk_pat p0.info (PVnt(qi,pio)),[])
                          | Some pi,Some sj -> 
                              begin 
                                match static_match i sev pi sj with
                                  | Some (new_pi,binds) -> 
                                      Some (mk_pat p0.info (PVnt(qi,pio)),binds)
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
            
      | PPar(p1,p2) -> 
          begin match s with 
            | SProduct(s1,s2) -> 
                begin match
                  static_match i sev p1 s1, 
                  static_match i sev p2 s2 
                with 
                  | Some (new_p1,l1),Some(new_p2,l2) -> 
                      Some (mk_pat p0.info (PPar(new_p1,new_p2)),l1 @ l2)
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
    msg "]@\n";
*)
  match p0.desc,v0 with   
  | PWld,_ -> Some []
  | PVar q,_ -> Some [(q,v0)]
  | PUnt,V.Unt(_) -> Some []
  | PInt(n1),V.Int(_,n2) -> if n1=n2 then Some [] else None
  | PBol(b1),V.Bol(_,b2) -> if b1=b2 then Some [] else None
  | PStr(s1),V.Str(_,s2) -> if s1 = RS.string_of_t s2 then Some [] else None
  | PVnt((_,li),pio),V.Vnt(_,_,lj,vjo) -> 
      if Id.equal li lj then 
        (match pio,vjo with 
           | None,None -> Some []
           | Some pi,Some vj -> dynamic_match i pi vj
           | _ -> 
               run_error i (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                              (Id.string_of_t li)))
      else None
  | PPar(p1,p2),V.Par(_,v1,v2) -> 
      (match dynamic_match i p1 v1,dynamic_match i p2 v2 with 
         | Some l1,Some l2 -> Some (l1 @ l2)
         | _ -> None)
  | _ -> None 

(* ------ sort resolution ----- *)

(* resolve_sort: resolves QIds in SData *)
let rec resolve_sort i sev s0 = 
  let rec go s0 = match s0 with 
    | SUnit | SBool | SString | SInteger | SRegexp | SLens | SCanonizer | SVar _ -> 
        s0
    | SFunction(x0,s1,s2) -> 
        SFunction(x0,go s1,go s2) 
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
    | SRefine _ -> assert false in 
  go s0

(* ------ sort checking expressions ----- *)
let rec check_exp sev e0 = 
  Trace.debug "check"
    (fun () -> 
       msg "@[check_exp ";
       format_exp e0;
       msg "@]@\n");
  match e0.desc with
  | EVar(q) ->
      (* lookup q in the environment *)
      let e0_sort = match SCEnv.lookup sev q with
        | Some (G.Sort s) -> 
            (* if it is bound, return the sort *)
            s
        | _ -> 
            (* otherwise raise an exception *)
            sort_error e0.info
              (fun () -> msg "@[%s is not bound@]" 
                 (Qid.string_of_t q)) in 
      (e0_sort,mk_annot_exp e0.info e0.desc e0_sort)

  | EOver(op,es) -> begin 
      let i = e0.info in 
      let err () =
        sort_error i 
          (fun () -> 
             msg "@[could@ not@ resolve@ overloaded@ operator@]") in 
      (* rules for overloaded symbols *)
      let iter_rules = 
        [ SRegexp, "regexp_iter";
          SLens, "lens_iter";
          SCanonizer, "canonizer_iter" ] in 
      let dot_rules = 
        [ SString, "string_concat";
          SRegexp, "regexp_concat";
          SLens, "lens_concat";
          SCanonizer, "canonizer_concat" ] in 
      let bar_rules = 
        [ SRegexp, "regexp_union";
          SLens, "lens_union";
          SCanonizer, "canonizer_union" ] in 
      let tilde_rules =
        [ SLens, "lens_swap";
          SCanonizer, "canonizer_swap" ] in 
      (* helper to find rule *)
      let rec find_rule r es = match r with 
        | [] -> None
        | (s,x)::t -> 
            if Safelist.for_all (fun (si,_) -> subsort si s) es then 
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
      let raw_new_e = 
        match op,new_es with
          | OIter(min,max),[e1_sort,new_e1] -> begin 
              match find_rule iter_rules new_es with 
                | Some x ->
                    mk_app i 
                      (mk_app i 
                         (mk_app i 
                            (mk_var i (Qid.mk_native_prelude_t x))
                            new_e1)
                         (mk_int i min))
                      (mk_int i max) 
                | None -> err () 
            end
          | ODot,[e1_sort,new_e1; e2_sort,new_e2] -> begin
              match find_rule dot_rules new_es with 
                | Some x -> 
                    mk_app3 i (mk_var i (Qid.mk_native_prelude_t x)) new_e1 new_e2 
                | None -> err ()
            end
          | OBar,[e1_sort,new_e1; e2_sort,new_e2] -> begin
              match find_rule bar_rules new_es with 
                | Some x -> 
                    mk_app3 i (mk_var i (Qid.mk_native_prelude_t x)) new_e1 new_e2 
                | None -> err ()
            end
          | OTilde,[e1_sort,new_e1; e2_sort,new_e2] -> begin
              match find_rule tilde_rules new_es with
                | Some x -> 
                    mk_app3 i (mk_var i (Qid.mk_native_prelude_t x)) new_e1 new_e2 
                | None -> err ()
            end
          | _ -> err () in 
      (* and check the result *)
      check_exp sev raw_new_e
    end

  | EFun(p,ret_sorto,body) ->
      let (px,param_sort) = match p.desc with
        | Param(x,s) -> x,s in 
      (* resolve the parameter sort *)
      let new_param_sort = resolve_sort p.info sev param_sort in 
      (* create the environment for the body *)
      let qx = Qid.t_of_id px in 
      let body_sev = SCEnv.update sev qx (G.Sort new_param_sort) in      
      let body_sort,new_body = 
        match ret_sorto with 
          | None -> 
              (* if no return sort declared, just check the body *)
              check_exp body_sev body 
          | Some ret_sort ->
              (* otherwise, resolve the declared return sort *)
              let new_ret_sort = resolve_sort e0.info body_sev ret_sort in 
              (* then check the body *)
              let body_sort,new_body = check_exp body_sev body in       
              (* and check that the declared return sort is a subsort of the body sort *)
              if not (subsort body_sort new_ret_sort) then 
                sort_error e0.info
                  (fun () -> 
                     format_exp e0; msg "@\n";
                     msg "@[in@ function:@ %s@ expected@ but@ %s@ found@]"
                       (string_of_sort new_ret_sort)
                       (string_of_sort body_sort));
              (body_sort,new_body) in 
      let e0_sort = SFunction(px,new_param_sort,body_sort) in 
      let new_p = mk_param p.info (Param(px,new_param_sort)) in 
      let new_e0 = EFun(new_p,Some body_sort,new_body) in         
      (e0_sort,mk_annot_exp e0.info new_e0 e0_sort)

  | ELet(b,e) ->
      (* for let-expressions, check the bindings *)
      let bevs,_,new_b = check_binding sev b in 
      (* use the resulting environment to check the exp *)
      let e_sort,new_e = check_exp bevs e in 
      let new_e0 = ELet(new_b,new_e) in 
      (e_sort,mk_annot_exp e0.info new_e0 e_sort)

  | EPair(e1,e2) -> 
      (* for pairs, recursively check e1 and e2 *)
      let e1_sort,new_e1 = check_exp sev e1 in 
      let e2_sort,new_e2 = check_exp sev e2 in 
      let e0_sort = SProduct(e1_sort,e2_sort) in 
      let new_e0 = EPair(new_e1,new_e2) in 
      (e0_sort,mk_annot_exp e0.info new_e0 e0_sort)

  | EUnit -> 
      (* units have sort SUnit *)
      (SUnit,mk_annot_exp e0.info e0.desc SUnit)

  | EBoolean(_) -> 
      (* boolean constants have sort SBool *)
      (SBool,mk_annot_exp e0.info e0.desc SBool)

  | EInteger(_) -> 
      (* integer constants have sort SInteger *)
      (SInteger,mk_annot_exp e0.info e0.desc SInteger)

  | EString(_) -> 
      (* string constants have sort SString *)
      (SString,mk_annot_exp e0.info e0.desc SString)

  | ECSet(_) -> 
      (* character sets have sort SRegexp *)
      (SRegexp,mk_annot_exp e0.info e0.desc SRegexp)

(*       msg "@[IN APP: "; format_exp e0; msg "@]@\n"; *)
(*       msg "@[E1_SORT: %s@\n@]" (string_of_sort e1_sort); *)
(*       msg "@[E2_SORT: %s@\n@]" (string_of_sort e2_sort); *)
(*       msg "@[RESULT: %s@\n@]" (string_of_sort ret_sort); *)

  | EApp(e1,e2) ->  
      (* for function applications, check the left-hand expression *)
      let e1_sort,new_e1 = check_exp sev e1 in 
      (* and make sure it is a function sort *)
      begin match e1_sort with 
        | SFunction(_,param_sort,return_sort) -> 
            (* then check the right-hand expression *)
            let e2_sort,new_e2 = check_exp sev e2 in 
            (* and make sure its sort is compatible with the parameter *)
            if not (subsort e2_sort param_sort) then 
              sort_error e2.info
                (fun () -> 
                   format_exp e0; msg "@\n";
                   msg "@[in@ application:@ expected@ %s@ but@ found@ %s@]"
                     (string_of_sort param_sort)
                     (string_of_sort e2_sort));
            let new_e0 = EApp(new_e1,new_e2) in 
            (return_sort,mk_annot_exp e0.info new_e0 return_sort)
        | _ -> 
            sort_error e1.info
              (fun () ->              
                 msg "@[in@ application:@ expected@ function@ sort@ but@ found@ %s.@]"
                   (string_of_sort e1_sort))
      end

(*       msg "ECASE: %s@\n" (string_of_sort e1_sort); *)
(*       msg "BRANCHES SORT: %s@\n" (string_of_sort branches_sort); *)
(*            msg "CHECKING BRANCH: "; *)
(*            format_pat pi; *)
(*            msg " -> "; *)
(*            format_exp ei; *)
(*            msg "@\n"; *)
(*                  msg "EI_SORT: %s@\n" (string_of_sort ei_sort); *)
(*                  msg "BRANCHES_SORT: %s@\n" (string_of_sort branches_sort); *)
(*       msg "END OF CASE %t@\n" (fun _ -> format_sort e0_sort); *)
(*   Trace.debug "check" *)
(*     (fun () ->  *)
(*        msg "@[check_exp result "; *)
(*        format_exp e0; *)
(*        msg "@ :@ "; format_sort (match res with (_,s,_) -> s); *)
(*        msg "@]@\n"); *)
(*     res *)

  | ECase(e1,pl) -> 
      (* helper function for printing error messages *)
      let err2 i p s1 s2 = sort_error i (fun () -> msg p s1 s2) in 
      (* check the expression being matched *)
      let e1_sort,new_e1 = check_exp sev e1 in 
      (* fold over the list of patterns and expressions *)
      let new_pl_rev,pl_sorto = Safelist.fold_left 
        (fun (new_pl_rev,branches_sorto) (pi,ei) -> 
           match static_match e0.info sev pi e1_sort with 
             | None -> 
                 (* if the branch is useless, raise an exception *)
                 err2 e0.info "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" 
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
                 let branches_sort = 
                   match branches_sorto with 
                     | None -> ei_sort 
                     | Some branches_sort ->
                         try join ei.info ei_sort branches_sort with _ -> 
                           err2 ei.info "@[in@ match:@ %s@ expected@ but@ %s@ found@]"
                             (string_of_sort branches_sort)
                             (string_of_sort ei_sort) in  
                 let new_pl_rev' = (new_pi,new_ei)::new_pl_rev in 
                 (new_pl_rev',Some branches_sort))
        ([],None) pl in 
      let e0_sort = match pl_sorto with 
        | Some pl_sort -> pl_sort 
        | None -> sort_error e0.info (fun () -> msg "@[empty@ match@ expression@]") in  
      let new_e0 = ECase(new_e1,Safelist.rev new_pl_rev) in 
      (e0_sort,mk_annot_exp e0.info new_e0 e0_sort)

  | ETyFun(x,e1) -> 
      let e1_sort,new_e1 = check_exp sev e1 in 
      let new_e0 = ETyFun(x,new_e1) in 
      let e0_sort = SForall(x,e1_sort) in 
      (e0_sort,mk_annot_exp e0.info new_e0 e0_sort)

  | ETyApp(e1,s2) -> 
      let e1_sort,new_e1 = check_exp sev e1 in 
      let new_s2 = resolve_sort e0.info sev s2 in 
      let e0_sort = match e1_sort with 
        | SForall(x,s11) -> subst_sort [x,new_s2] s11
        | _ -> sort_error e0.info 
            (fun () -> msg "@[in@ type@ application:@ expected@ universal@ type@ but@ %s@ found.@]"
               (string_of_sort e1_sort)) in 
      let new_e0 = ETyApp(new_e1,new_s2) in 
      (e0_sort,mk_annot_exp e0.info new_e0 e0_sort)

and check_binding sev b0 = match b0.desc with
  | Bind(x,so,e) ->
      let qx = Qid.t_of_id x in 
      let e_sort,new_e = check_exp sev e in
      let new_s = match so with 
        | None -> e_sort 
        | Some s -> 
            let new_s = resolve_sort b0.info sev s in
              if not (subsort e_sort new_s) then 
                sort_error b0.info
                  (fun () ->
                     msg "@[in@ let-binding:@ %s@ expected@ but@ %s@ found@]"
                       (string_of_sort new_s)
                       (string_of_sort e_sort));
            new_s in               
      let bsev = SCEnv.update sev qx (G.Sort new_s) in 
      let new_b = mk_binding b0.info (Bind(x,Some new_s,new_e)) in 
      (bsev,[qx],new_b)

(* type check a single declaration *)
let rec check_decl sev ms d0 = match d0.desc with
  | DLet(b) ->
      let bsev,xs,new_b = check_binding sev b in 
      let new_d = DLet(new_b) in
      (bsev,xs,mk_decl d0.info new_d)
  | DMod(n,ds) ->
      let qmn = Qid.t_dot_id (SCEnv.get_mod sev) n in 
      let msev = SCEnv.set_mod sev qmn in 
      let ms = ms @ [n] in 
      (* check the module *)
      let msev,names,new_ds = check_module_aux msev ms ds in
      let nsev, names_rev = Safelist.fold_left 
        (fun (nsev, names) q -> 
           match SCEnv.lookup msev q with
               None -> run_error d0.info
                 (fun () -> msg "@[declaration@ for@ %s@ missing@]"
                    (Qid.string_of_t q))
             | Some s ->
                 (* prefix the qualifiers in each name with n *)
                 let nq = Qid.splice_id_dot n q in
                 (SCEnv.update nsev nq s, nq::names))
        (msev,[]) names in 
      let new_d = DMod(n,new_ds) in 
      (nsev,Safelist.rev names_rev,mk_decl d0.info new_d)

  | DType(svl,qx,cl) -> 
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
               | Some s -> (x,Some (resolve_sort d0.info sev2 s)) in 
             x_so'::acc)
          [] cl in 
      let new_cl = Safelist.rev new_cl_rev in 
      let new_qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) new_cl in
      (* put the real qx in environment -- note! must discard dummy sev2 *)
      let sev3 = SCEnv.update_type sev svl new_qx new_qcl in
      (* build the datatype *)
      let sx = SData(sl_of_svl svl,new_qx) in
      (* add constructors to sev *)
      let sev4,xs_rev = Safelist.fold_left 
        (fun (sevi,acc) (li,sio) ->            
           let li_sort = match sio with 
             | None -> sx
             | Some si -> SFunction(Id.wild,si,sx) in
           let qli = Qid.t_of_id li in 
           (SCEnv.update sevi qli (G.Sort li_sort),qli::acc))
        (sev3,[]) new_cl_rev in
      let new_d = DType(svl,new_qx,Safelist.rev new_cl_rev) in 
      (sev4,Safelist.rev xs_rev,mk_decl d0.info new_d)
          
  | DTest(e1,tr) -> 
      (* check the expression *)
      let e1_sort,new_e1 = check_exp sev e1 in
      let new_tr = match tr with 
        | TestError | TestShow -> tr
        | TestValue e2 -> 
            (* for values, check that the exps have compatible types *)
            (* TODO: this should only be for "equality types"?! *)
            let e2_sort,new_e2 = check_exp sev e2 in 
            if not (subsort e2_sort e1_sort) then
              sort_error e2.info
                (fun () -> 
                   msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort e1_sort)
                     (string_of_sort e2_sort));
            TestValue new_e2
        | TestLensType(e21o,e22o) -> 
            (* for lens type results, check that both exps are regexps *)
            let chk_eo evsi = function
              | None -> None
              | Some e -> 
                  let e_sort,new_e = check_exp evsi e in 
                  if not (subsort e_sort SRegexp) then
                    sort_error e.info 
                      (fun () ->
                         msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                           (string_of_sort SRegexp)
                           (string_of_sort e_sort));
                  Some new_e in 
            let new_e21o = chk_eo sev e21o in 
            let new_e22o = chk_eo sev e22o in 
           TestLensType(new_e21o,new_e22o) in 
      let new_d = DTest(new_e1,new_tr) in 
      (sev,[],mk_decl d0.info new_d)
          
and check_module_aux sev m ds = 
  let msev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sevi, names, new_ds_rev) di -> 
         let dsev,new_names,new_di = check_decl sevi m di in
         dsev,names@new_names,new_di::new_ds_rev)
      (sev,[],[]) ds in 
  (msev, names, Safelist.rev new_ds_rev)

(* --------------- Instrumentation --------------- *)
(* 
module SVMap = Env.Make(
  struct
    type t = svar
    let compare (x,_) (y,_) = Pervasives.compare x y
    let to_string (x,_) = sprintf "<%d>" x
  end)

module IEnv = struct
  (* instrumentation environment: maps of svars to Qid.ts and a lookup
     function for constructor names *)
  type t = Qid.t SVMap.t * (Qid.t -> (Qid.t * G.tspec) option)
  let empty lookup = (SVMap.empty (), lookup)
  let update (sm,lk) sv q = (SVMap.update sm sv q,lk) 
  let lookup (sm,lk) q = SVMap.lookup sm q 
  let lookup_type (sm,lk) q = lk q
end

(* gensym *)
let fresh_counter = ref 0 
let fresh_var i exclo = 
  let gen () = 
    let x = !fresh_counter in 
    let n = x / 26 in 
    let s = 
      sprintf "_%c%s" 
        (Char.chr (97 + x mod 26))
        (if n=0 then "" else string_of_int n) in 
      incr fresh_counter;
      Id.mk i s in 
  (* generate fresh variables until we find one not in l *)
  let rec loop x = 
    match exclo with 
      | None -> x
      | Some y -> if Id.equal x y then loop (gen ()) else x in
  loop (gen ())
   
(* instrumentation at sorts *)
let rec instrument_sort i iev s0 = match Bunify.zonk i s0 with
  | SVar(sv) -> begin
      match IEnv.lookup iev sv with
        | Some(q) -> 
            mk_var i q
        | None -> 
            run_error i 
              (fun () -> 
                 msg "@[unbound sort variable ";
                       Bprint.reset_name_ctxt ();
                       format_sort s0; msg "@]")
    end

  | SRawVar(_) -> 
      run_error i 
        (fun () ->
           msg "@[unexpected raw sort variable ";
           format_sort s0; msg "@]")

  | SUnit | SString | SInteger | SRegexp | SLens | SCanonizer -> 
      let x = fresh_var i None in 
      mk_fun i x s0
        (mk_var i (Qid.t_of_id x))

  | SRefine(x,s1,e0) ->
      let e_x = mk_var i (Qid.t_of_id x) in 
      let e_blame = mk_var i (Qid.mk_native_prelude_t "blame") in        
        mk_fun i x s1
          (mk_if i e0 e_x (mk_app i e_blame e_x))
               
  | SFunction(x,s1,s2) ->
      let s1_checker = instrument_sort i iev s1 in 
      let s2_checker = instrument_sort i iev s2 in 
      let f = fresh_var i (Some x) in 
      let qf = Qid.t_of_id f in 
      let y = 
        if Id.equal Id.wild x then fresh_var i None
        else x in 
      let qy = Qid.t_of_id y in 
      let ef = mk_var i qf in 
      let ey = mk_var i qy in
      let merge_blame = mk_var i (Qid.mk_native_prelude_t "merge_blame") in
        mk_fun i f s0
          (mk_fun i y s1
             (mk_let i y s1
                (mk_app i 
                   s1_checker
                   (mk_app3 i merge_blame ef ey))
                (mk_app i 
                   s2_checker 
                   (mk_app i ef ey))))
        
  | SProduct(s1,s2) ->
      let s1_checker = instrument_sort i iev s1 in
      let s2_checker = instrument_sort i iev s2 in 
      let p = fresh_var i None in
      let ep = mk_var i (Qid.t_of_id p) in
      let x1 = fresh_var i None in
      let ex1 = mk_var i (Qid.t_of_id x1) in
      let x2 = fresh_var i None in
      let ex2 = mk_var i (Qid.t_of_id x2) in
      mk_fun i p s0
        (mk_case i ep 
           [(mk_annot_pat i 
               (PPar(mk_annot_pat i (PVar x1) s1,
                     mk_annot_pat i (PVar x2) s2))
               s0,
             (mk_pair i 
                (mk_app i s1_checker ex1)
                (mk_app i s2_checker ex2)))])
      
  | SData(sl,qx) -> 
      let v = fresh_var i None in
      let ev = mk_var i (Qid.t_of_id v) in
      let y = fresh_var i None in 
      let qy = Qid.t_of_id y in 
      let svl,cl = match IEnv.lookup_type iev qx with
        | None -> run_error i (fun () -> msg "@[cannot resolve %s@]" (Qid.string_of_t qx))
        | Some (_,ts) -> ts in 
      let s0_inst,cl_inst = Bunify.instantiate_cases i (qx,(svl,cl)) in
      let () = 
        if not (Bunify.unify i s0 s0_inst) then 
          run_error i (fun () -> msg "@[unexpected error instrumenting %s]" (string_of_sort s0)) in 
      let cl_rev' = 
        Safelist.fold_left 
          (fun acc tc -> match tc with
             | (q,None) -> 
                 let p = mk_annot_pat i (PVnt(q,None)) s0 in 
                 (p,ev)::acc
             | (q,Some s) -> 
                 let s_checker = instrument_sort i iev s in 
                 let py = mk_annot_pat i (PVar y) s in 
                 let p = mk_annot_pat i (PVnt(q,Some py)) s0 in 
                 let ey = mk_var i qy in 
                 (p,
                  mk_app i 
                    (mk_var i q)
                    (mk_app i s_checker ey))::acc)
          [] cl_inst in 
      mk_fun i v s0 (mk_case i ev (Safelist.rev cl_rev'))

and instrument_exp iev e0 = 
  let res = match e0.desc with
  | EVar(q) -> 
      let so,sl = e0.annot in 
      let chkl = Safelist.map (instrument_sort e0.info iev) sl in 
        Safelist.fold_right 
          (fun chki acc -> mk_app e0.info acc chki)
          chkl e0 

  | EOver _ -> run_error e0.info (fun () -> msg "@[unresolved overloaded operator@]")

  | EApp(e1,e2) ->     
      let i1 = instrument_exp iev e1 in 
      let i2 = instrument_exp iev e2 in 
      mk_exp e0.info (EApp(i1,i2))

  | ELet(b,e) -> 
      let ib = instrument_binding iev b in 
      let ie = instrument_exp iev e in 
      mk_exp e0.info (ELet(ib,ie))

  | EFun(p1,_,e1) -> 
      let s0_checker = instrument_sort e0.info iev (get_sort e0) in
      let i1 = instrument_exp iev e1 in 
      let i0 = mk_exp e0.info (EFun(p1,None,i1)) in 
      mk_exp e0.info (EApp(s0_checker,i0))

  | EPair(e1,e2) ->
      let i1 = instrument_exp iev e1 in 
      let i2 = instrument_exp iev e2 in 
      mk_exp e0.info (EPair(i1,i2)) 

  | ECase(e1,cl) -> 
      let i1 = instrument_exp iev e1 in 
      let new_cl_rev = 
        Safelist.fold_left 
          (fun acc (pi,ei) -> (pi,instrument_exp iev ei)::acc)
          [] cl in 
      mk_exp e0.info (ECase(i1,Safelist.rev new_cl_rev)) 

  | EUnit | EString _ | EInteger _ | ECSet _ -> 
      e0 in 
  Trace.debug "instr++" 
    (fun () -> 
       msg "@["; 
       format_exp e0; 
       msg " ~> ";
       format_exp res; 
       msg "@]@\n"); 
    res

and instrument_binding iev b0 = 
  match b0.desc,b0.annot with 
  | Bind(p,e),Some (svl,s) ->       
      let assoc = Safelist.map (fun svi -> (svi,fresh_var b0.info None)) svl in 
      let fiev = 
        Safelist.fold_right 
          (fun (svi,ci) ievi -> IEnv.update ievi svi (Qid.t_of_id ci))
          assoc iev in
      let new_e = 
        Safelist.fold_right 
          (fun (svi,ci) acc -> 
             let ci_sort = SVar svi in 
             let f_sort = SFunction(Id.wild,ci_sort,ci_sort) in 
             mk_fun b0.info ci f_sort acc)
          assoc (instrument_exp fiev e) in
      mk_annot_binding b0.info (Bind(p,new_e)) (svl,s)

  | Bind _,None -> 
      run_error b0.info
        (fun () -> msg "@[unannotated binding!@ ";
                   Bprint.format_binding b0;
                   msg "@]")

and instrument_decl iev d0 =
  Trace.debug "instr" 
    (fun () -> msg "@[instrument_decl: "; format_decl d0; msg "@]@\n"); 
  let res = match d0.desc with
  | DLet(b) ->
      let ib = instrument_binding iev b in 
      mk_decl d0.info (DLet(ib)) 

  | DMod(n,ds) ->
      let ids = instrument_module_aux iev ds in 
      mk_decl d0.info (DMod(n,ids)) 

  | DType(sl,qx,cl) -> d0
      
  | DTest(e1,tr) -> 
      let i1 = instrument_exp iev e1 in 
      let itr = match tr with
        | TestValue e2 -> 
            let i2 = instrument_exp iev e2 in 
            TestValue(i2)
        | TestLensType(e1o,e2o) -> 
            let instr = instrument_exp iev in
            let i1o = Misc.map_option instr e1o in 
            let i2o = Misc.map_option instr e2o in 
            TestLensType(i1o,i2o)
        | _ -> tr in 
      mk_decl d0.info (DTest(i1,itr)) in
    Trace.debug "instr_test"
      (fun () -> msg "@[~> "; format_decl res; msg "@]@\n"); 
    res

and instrument_module_aux iev ds = 
  Safelist.fold_right
    (fun di acc -> instrument_decl iev di::acc)
    ds []
*)
(* entry point to static analysis / instrumentation *)
let check_module m0 = 
  match m0.desc with
  | Mod(m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let sev = SCEnv.set_ctx (SCEnv.empty qm) (m::nctx@G.pre_ctx) in
      let checked_sev,_,checked_ds = check_module_aux sev [m] ds in 
      let instrumented_ds = checked_ds in 
(*         instrument_module_aux (IEnv.empty (SCEnv.lookup_type sev)) checked_ds  *)
      let res = mk_mod m0.info (Mod(m,nctx,instrumented_ds)) in 
      Trace.debug "instr+" (fun () -> format_module res; msg "@\n");
      res

(* --------------- Compiler --------------- *)
(* expressions *)
let rec compile_exp cev e0 = match e0.desc with 
  | ETyFun(_,e) -> 
      compile_exp cev (mk_fun e0.info Id.wild SUnit e)

  | ETyApp(e1,s2) -> 
      compile_exp cev (mk_app e0.info e1 (mk_unit e0.info))

  | EVar(q) ->       
      begin match CEnv.lookup cev q with
        | Some(_,v) -> v
        | None -> 
            run_error e0.info 
              (fun () -> msg "@[%s is not bound@]" (Qid.string_of_t q))
      end

  | EOver _ -> 
      run_error e0.info (fun () -> msg "@[unresolved overloaded operator@]")

  | EApp(e1,e2) ->
      let v1 = compile_exp cev e1 in 
      let v2 = compile_exp cev e2 in 
      (V.get_f v1) v2

  | ELet(b,e) -> 
      let bcev,_ = compile_binding cev b in
      compile_exp bcev e
          
  | EFun(p,_,e) ->
      let f v =        
        let body_cev = CEnv.update cev (Qid.t_of_id (id_of_param p)) (G.Unknown,v) in 
        compile_exp body_cev e in 
      V.Fun (V.blame_of_info e0.info,f)

  | EPair(e1,e2) -> 
      let v1 = compile_exp cev e1 in 
      let v2 = compile_exp cev e2 in 
      V.Par(V.blame_of_info e0.info,v1,v2)

  | ECase(e1,pl) -> 
      let v1 = compile_exp cev e1 in 
      (* first match *)
      let rec find_match = function
        | [] -> 
            run_error e0.info 
              (fun () -> msg "@[match@ failure@]")
        | (pi,ei)::rest -> 
            (match dynamic_match e0.info pi v1 with 
               | None -> find_match rest
               | Some l -> l,ei) in 
      let binds,ei = find_match pl in 
      let qid_binds = Safelist.map (fun (x,v) -> (Qid.t_of_id x,(G.Unknown,v))) binds in         
      let ei_cev = CEnv.update_list cev qid_binds in
      compile_exp ei_cev ei

  | EUnit -> 
      V.Unt (V.blame_of_info e0.info)

  | EBoolean(b) -> 
      V.Bol(V.blame_of_info e0.info,b)

  | EInteger(i) -> 
      V.Int(V.blame_of_info e0.info,i)

  | EString(s) -> 
      V.Str(V.blame_of_info e0.info,s)

  | ECSet(pos,cs) -> 
      let mk_rx = if pos then R.set else R.negset in 
      V.Rx (V.blame_of_info e0.info, mk_rx cs)

and compile_binding cev b0 = match b0.desc with
  | Bind(x,Some s,e) -> 
      let v = compile_exp cev e in 
      let qx = Qid.t_of_id x in 
      let bcev = CEnv.update cev qx (G.Sort s,v) in       
      (bcev,[qx])
  | _ -> 
      run_error b0.info (fun () -> msg "@[unannotated@ binding@]")
      
let rec compile_decl cev ms d0 = match d0.desc with 
  | DLet(b) -> 
      let bcev,xs = compile_binding cev b in
      (bcev,xs)
  | DType(svl,qx,cl) -> 
      let sx = SData(sl_of_svl svl,qx) in 
      let mk_univ s = Safelist.fold_right (fun svi acc -> SForall(svi,acc)) svl s in 
      let mk_impl v = Safelist.fold_right (fun _ f -> V.Fun(V.blame_of_info d0.info,(fun v -> f))) svl v in 
      let new_cev,xs = 
        Safelist.fold_left 
          (fun (cev,xs) (l,so) -> 
             let ql = Qid.t_of_id l in 
             let rv = match so with 
               | None -> 
                   let s = mk_univ sx in 
                   let v = mk_impl (V.Vnt(V.blame_of_info d0.info,qx,l,None)) in 
                   (G.Sort s,v) 
               | Some s -> 
                   let s = mk_univ (SFunction(Id.wild,s,sx)) in 
                   let f v = V.Vnt(V.blame_of_t v,qx,l,Some v) in 
                   let v = mk_impl (V.Fun(V.blame_of_info d0.info,f)) in 
                   (G.Sort s,v) in 
             (CEnv.update cev ql rv,Qid.t_of_id l::xs))
        (cev,[]) cl in 
      let qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) cl in 
      let new_cev' = CEnv.update_type new_cev svl qx qcl in   
      (new_cev',xs)

  | DMod(n,ds) ->
      let m_cev, names = compile_mod_aux cev ms ds in
      let n_cev,names_rev = 
        Safelist.fold_left
          (fun (n_cev, names) q ->
             match CEnv.lookup m_cev q with
               | Some rv ->
                   let nq = Qid.splice_id_dot n q in
                   (CEnv.update n_cev nq rv, nq::names)
               | None -> 
                   run_error d0.info 
                     (fun () -> msg "@[compiled declaration for %s missing@]"
                        (Qid.string_of_t q)))
          (cev,[])
          names in
        (n_cev, Safelist.rev names_rev)
  | DTest(e,tr) ->
      if check_test ms then 
        begin
          let vo = 
            try let v = compile_exp cev e in 
            OK(v)
            with (Error.Harmony_error(err)) -> Error err in 
          match vo,tr with 
            | OK (v), TestShow ->
                msg "Test result:@ "; 
                V.format v; 
                msg "@\n%!"
            | OK(v), TestLensType(e1o,e2o) -> 
                let l = V.get_l v in 
                let c,a = L.ctype l,L.atype l in 
                let chk_eo r = function
                  | None -> true,"?"
                  | Some e -> 
                      let expected = V.get_r (compile_exp cev e) in 
                      (R.equiv r expected, R.string_of_t expected) in 
                let c_ok,c_str = chk_eo c e1o in 
                let a_ok,a_str = chk_eo a e2o in 
                  if c_ok && a_ok then 
                    (if e1o = None || e2o = None then 
                      begin 
                        msg "Test type:@ ";
                        msg "@[<2>%s <-> %s@]" (R.string_of_t c) (R.string_of_t a);
                        msg "@\n%!"
                      end)
                  else
                    begin
                  test_error d0.info
                    (fun () -> 
                       msg "@\nExpected@ "; 
                       msg "@[<2>%s <-> %s@]" c_str a_str;
                       msg "@ but found@ "; 
                       msg "@[<2>%s <-> %s@]" (R.string_of_t c) (R.string_of_t a);
                       msg "@\n%!");
                    end                      
            | Error err, TestShow 
            | Error err, TestLensType _ -> 
                test_error d0.info 
                  (fun () -> 
                    msg "Test result: error@\n";
                    err (); 
                    msg "@\n%!")
            | Error _, TestError -> ()
            | OK(v), TestValue res -> 
                let resv = compile_exp cev res in
                  if not (V.equal v resv) then
                    test_error d0.info 
                      (fun () ->
                        msg "@\nExpected@ "; V.format resv;
                        msg "@ but found@ "; V.format v; 
                        msg "@\n%!")
            | Error err, TestValue res -> 
                let resv = compile_exp cev res in 
                  test_error d0.info 
                    (fun () ->
                      msg "@\nExpected@ "; V.format resv; 
                      msg "@ but found an error:@ "; 
                      err (); 
                      msg "@\n%!")
            | OK(v), TestError -> 
                test_error d0.info 
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

let compile_module m0 = match m0.desc with 
  | Mod(m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let cev = CEnv.set_ctx (CEnv.empty qm) (m::nctx@G.pre_ctx) in
      let new_cev,_ = compile_mod_aux cev [m] ds in
      G.register_env (CEnv.get_ev new_cev) m
