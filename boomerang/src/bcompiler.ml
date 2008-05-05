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
(* lookup a sort *)
let get_sort e0 = match e0.annot with
  | None,_ -> run_error e0.info 
      (fun () -> msg "@[Unchecked expression@]")
  | (Some s),_ -> s

let get_decl_sort d0 = match d0.annot with 
  | None -> run_error d0.info 
      (fun () -> msg "@[Unchecked declaration@]")
  | Some s -> s
        
let mk_var i q = mk_exp i (EVar q) 
let mk_app i e1 e2 = mk_exp i (EApp(e1,e2))
let mk_app3 i e1 e2 e3 = mk_app i (mk_app i e1 e2) e3
let mk_let i qx s1 e1 e2 =
  let p = mk_checked_pat i (PVar(qx)) s1 in 
  let b = mk_annot_binding i (Bind(p,e1)) ([],s1) in 
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
  val update_type : t -> Bsyntax.svar list -> Qid.t -> G.tcon list -> t
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
(* calculate the free sort variables in a compilation environment *)
let cenv_free_svs i cev = 
  CEnv.fold 
    (fun _ (rs,_) acc -> match rs with 
       | G.Scheme (svli,si) -> 
           let svsi = Bunify.svs_of_svl svli in
           SVSet.union acc (SVSet.diff (free_svs i si) svsi)
       | _ -> acc)
    cev SVSet.empty

(* calculate free variables in a sort checking environment *)
let scenv_free_svs i sev = 
  SCEnv.fold 
    (fun _ rs acc -> match rs with 
       | G.Scheme (svli,si) -> 
           let svsi = Bunify.svs_of_svl svli in 
           SVSet.union acc (SVSet.diff (free_svs i si) svsi)
       | _ -> acc)
    sev SVSet.empty

(* lookup a constructor *)
let get_con i sev li = 
  match SCEnv.lookup_con sev li with
    | None -> sort_error i 
        (fun () -> msg "@[Unbound@ constructor@ %s@]" (Qid.string_of_t li))
    | Some r -> r

(* static_match: determine if a value with a given sort *could* match
   a pattern; annotate PVars with their sorts, return the list of sort
   bindings for variables. *)
let rec static_match i sev p0 s = 
(*   msg "STATIC_MATCH: %s # %s@\n" (string_of_pat p0) (string_of_sort s); *)
(*         msg "LOOKING UP %s@\n" (Qid.string_of_t li); *)
(*               msg "QX: %s SVL: %t@\n" (Qid.string_of_t qx) (fun _ -> Misc.format_list "," (format_svar false) svl); *)
(*               msg "RAW DATA         : %s@\n" (string_of_sort (SData(sl,qx))); *)
(*               msg "INSTANTIATED DATA: %s@\n" (string_of_sort s_expect); *)
  let err p s1 s2 = sort_error i 
    (fun () -> msg "@[in@ pattern@ %s:@ expected %s,@ but@ found@ %s@]"
       (string_of_pat p)
       (string_of_sort s1)
       (string_of_sort s2)) in 
  match p0.desc with 
    | PWld -> Some []
    | PVar(x) -> 
        p0.annot <- Some s;
        Some [(x,s)]
    | PUnt -> 
        if not (Bunify.unify i s SUnit) then err p0 SUnit s;
        Some []
    | PVnt(li,pio) -> 
        (* lookup the constructor from the environment *)
        let qx,(svl,cl) = get_con p0.info sev li in 
        (* instantiate with *)
        let s_expect,cl_inst = Bunify.instantiate_cases i (qx,(svl,cl)) in 
          if not (Bunify.unify i s s_expect) then err p0 s_expect s;
          let rec find_constructor = function
            | [] -> None
            | (lj,sjo)::rest ->            
                if Qid.equal li lj then 
                  (match pio,sjo with 
                     | None,None -> Some []
                     | Some pi,Some sj -> 
                         Misc.map_option 
                           (fun l -> l) 
                           (static_match i sev pi sj)                             
                     | _ -> sort_error i 
                         (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                            (Qid.string_of_t lj)))
                else find_constructor rest in 
          find_constructor cl_inst
    | PPar(p1,p2) -> 
        let s1,s2 = Bunify.fresh_sort Fre,Bunify.fresh_sort Fre in 
        let s_expect = SProduct(s1,s2) in 
        if not (Bunify.unify i s s_expect) then err p0 s_expect s;
        (match static_match i sev p1 s1, static_match i sev p2 s2 with 
           | Some l1,Some l2 -> Some (l1 @ l2)
           | _ -> None)

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
  | PWld,_ -> 
      Some []
  | PVar q,_ -> 
      (match p0.annot with 
         | None -> run_error i (fun () -> msg "@[unannotated@ pattern@ variable@]")
         | Some s -> Some [(q,s,v0)])
  | PUnt,V.Unt(_) -> Some []
  | PVnt((_,li),pio),V.Vnt(_,_,lj,vjo) -> 
      if Id.equal li lj then 
        (match pio,vjo with 
           | None,None -> Some []
           | Some pi,Some vj -> dynamic_match i pi vj
           | _ -> run_error i (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                                 (Id.string_of_t li)))
      else None
  | PPar(p1,p2),V.Par(_,v1,v2) -> 
      (match dynamic_match i p1 v1,dynamic_match i p2 v2 with 
         | Some l1,Some l2 -> Some (l1 @ l2)
         | _ -> None)
  | _ -> None 

(* ------ sort resolution ----- *)

(* resolve_sort: 
 *   (1) resolves QIds in SData and 
 *   (2) replaces SRawVars with fresh SVars 
 *)
let rec resolve_sort i evs s0 = 
  let res = 
  match s0 with
  | SVar _ | SUnit | SString | SInteger | SRegexp | SLens | SCanonizer -> 
      (evs,s0)
  | SFunction(x0,s1,s2) -> 
      let evs1,s1' = resolve_sort i evs s1 in 
      let evs2,s2' = resolve_sort i evs1 s2 in 
      (evs2,SFunction(x0,s1',s2'))
  | SProduct(s1,s2)  -> 
      let evs1,s1' = resolve_sort i evs s1 in 
      let evs2,s2' = resolve_sort i evs1 s2 in 
      (evs2,SProduct(s1',s2'))
  | SData(sl,qx) -> 
      let _,sev = evs in 
      let qx',_ = match SCEnv.lookup_type sev qx with 
        | None -> 
            sort_error i  
              (fun () -> msg "@[cannot@ resolve@ sort@ %s@]" 
                 (Qid.string_of_t qx))
        | Some q -> q in 
      let evs',sl' = 
        Safelist.fold_left 
          (fun (evs,sl) si -> 
             let evs',si' = resolve_sort i evs si in 
               (evs',si'::sl))
          (evs,[]) sl in 
        (evs',SData(sl',qx'))
  | SRawVar(x) -> 
      let tev,sev = evs in 
      begin
        try 
          (evs,snd (Safelist.find (fun (y,_) -> Id.equal x y) tev))
        with Not_found -> 
          let s_fresh = Bunify.fresh_sort Fre in 
          (((x,s_fresh)::tev,sev),s_fresh)
      end
  | SRefine(x0,s0,e0) -> assert false in 
(*     msg "RESOLVE_SORT: %s ~> %s@\n" (string_of_sort s0) (string_of_sort (snd res)); *)
    res


(* ------ sort checking expressions ----- *)
let rec check_exp evs e0 = 
  Trace.debug "check"
    (fun () -> 
       msg "@[check_exp ";
       format_exp e0;
       msg "@]@\n");
  match e0.desc with
  | EVar(q) ->
      let _,sev = evs in 
      (* lookup q in the environment *)
      let e0_sl,e0_sort,new_e = match SCEnv.lookup sev q with
        | Some (G.Sort s) -> 
            (* if q is bound to a sort, return that sort *)
            ([],s,e0)
        | Some (G.Scheme ss) -> 
            (* if q is bound to a scheme, instantiate with fresh variables, insert type applications. *)
            let sl,s = Bunify.instantiate e0.info ss in 
            (sl,s,e0)
        | _ -> 
            (* otherwise, q is unbound, so raise an exception *)
            sort_error e0.info
              (fun () -> msg "@[%s is not bound@]" 
                 (Qid.string_of_t q)) in 
      Trace.debug "check" (fun () -> msg "@[annot: ["; Misc.format_list "," format_sort e0_sl; msg "] => "; format_sort e0_sort; msg "@]@\n");
      (evs,e0_sort,mk_checked_annot_exp e0.info e0.desc e0_sort e0_sl)

  | EFun(p,ret_sorto,body) ->
      let (px,param_sort) = match p.desc with
        | Param(x,s) -> x,s in 
      (* for functions, resolve the parameter sort *)
      let (tev1,sev1),new_param_sort = resolve_sort p.info evs param_sort in 
      (* create the environment for the body *)
      let body_sev = SCEnv.update sev1 (Qid.t_of_id px) (G.Sort new_param_sort) in      
      let new_evs,body_sort,new_body = 
        match ret_sorto with 
          | None -> 
              (* if no return sort declared, just check the body *)
              check_exp (tev1,body_sev) body 
          | Some ret_sort ->
              (* otherwise, resolve the declared return sort *)
              let evs2,new_ret_sort = resolve_sort e0.info (tev1,body_sev) ret_sort in 
              (* then check the body *)
              let evs3,body_sort,new_body = check_exp evs2 body in       
              (* and unify the declared return sort with the inferred body sort *)
              if not (Bunify.unify e0.info body_sort new_ret_sort) then 
                sort_error e0.info
                  (fun () -> 
                     msg "@[in@ function:@ %s@ expected@ but@ %s@ found@]"
                       (string_of_sort new_ret_sort)
                       (string_of_sort body_sort));
              (evs3,body_sort,new_body) in 
      let dep_x = 
        if VSet.mem px (free_vars_sort body_sort) then px 
        else Id.wild in        
      let e0_sort = SFunction(dep_x,new_param_sort,body_sort) in 
      let new_p = mk_param p.info (Param(px,new_param_sort)) in 
      let new_e0 = EFun(new_p,Some body_sort,new_body) in         
      (new_evs,e0_sort,mk_checked_exp e0.info new_e0 e0_sort)

  | ELet(b,e) ->
      (* for let-expressions, check the bindings *)
      let bevs,_,new_b = check_binding evs b in 
      (* use the resulting environment to check the exp *)
      let evs1,e_sort,new_e = check_exp bevs e in 
      let new_e0 = ELet(new_b,new_e) in 
      (evs1,e_sort,mk_checked_exp e0.info new_e0 e_sort)

  | EPair(e1,e2) -> 
      (* for pairs, recursively check e1 and e2 *)
      let evs1,e1_sort,new_e1 = check_exp evs e1 in 
      let evs2,e2_sort,new_e2 = check_exp evs1 e2 in 
      let e0_sort = SProduct(e1_sort,e2_sort) in 
      let new_e0 = EPair(new_e1,new_e2) in 
      (evs2,e0_sort,mk_checked_exp e0.info new_e0 e0_sort)

  | EUnit -> 
      (* units have sort SUnit *)
      (evs,SUnit,mk_checked_exp e0.info e0.desc SUnit)

  | EString(_) -> 
      (* string constants have sort SString *)
      (evs,SString,mk_checked_exp e0.info e0.desc SString)

  | EInteger(_) -> 
      (* integer constants have sort SString *)
      (evs,SInteger,mk_checked_exp e0.info e0.desc SInteger)

  | ECSet(_) -> 
      (* character sets have sort SRegexp *)
      (evs,SRegexp,mk_checked_exp e0.info e0.desc SRegexp)

  (* elimination forms *)
  | EApp(e1,e2) ->  
(*       msg "@[IN APP: "; format_exp e0; msg "@]@\n"; *)
(*       msg "@[E1_SORT: %s@\n@]" (string_of_sort e1_sort); *)
(*       msg "@[E2_SORT: %s@\n@]" (string_of_sort e2_sort); *)
(*       msg "@[RESULT: %s@\n@]" (string_of_sort ret_sort); *)
      (* for function applications, check the left-hand expression *)
      let evs1,e1_sort,new_e1 = check_exp evs e1 in 
      let param_sort = Bunify.fresh_sort Fre in 
      let ret_sort = Bunify.fresh_sort Fre in      
      let sf = SFunction(Id.wild,param_sort,ret_sort) in
      (* and make sure it is a function *)
      if not (Bunify.unify e1.info e1_sort sf) then
        sort_error e1.info
          (fun () -> 
             msg "@[in@ application:@ %s@ expected@ but@ %s@ found@]"
               (string_of_sort sf)
               (string_of_sort e1_sort));
      (* then check the right-hand expression *)
      let evs2,e2_sort,new_e2 = check_exp evs1 e2 in
      (* and make sure its sort is the same as the parameter *)
      if not (Bunify.unify e2.info e2_sort param_sort) then 
        sort_error e0.info
          (fun () -> 
             msg "@[in@ application:@ %s@ expected@ but@ %s@ found@]"
               (string_of_sort param_sort)
               (string_of_sort e2_sort));
      let e0_sort = ret_sort in 
      let new_e0 = EApp(new_e1,new_e2) in 
      (evs2,e0_sort,mk_checked_exp e0.info new_e0 e0_sort)

  | ECase(e1,pl) -> 
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
      let err2 i p s1 s2 = sort_error i (fun () -> msg p s1 s2) in 
      (* for case expressions, first check the expression being matched *)
      let evs1,e1_sort,new_e1 = check_exp evs e1 in 
      (* get a fresh variable for the sort of the branches *)
      let branches_sort = Bunify.fresh_sort Fre in 
      (* fold over the list of patterns and expressions *)
      let new_evs,new_pl_rev = Safelist.fold_left 
        (fun ((tev,sev),new_pl_rev) (pi,ei) -> 
           match static_match e0.info sev pi e1_sort with 
             | None -> 
                 (* if a branch is useless, raise an exception *)
                 err2 e0.info "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" 
                   (string_of_pat pi) 
                   (string_of_sort e1_sort)
             | Some binds ->                
                 (* otherwise, extend the env with bindings for pattern vars *)
                 let ei_sev = Safelist.fold_left 
                   (fun ei_sev (qj,sj) -> SCEnv.update ei_sev (Qid.t_of_id qj) (G.Sort sj))
                   sev binds in 
                 (* sort check the expression *) 
                 let evsi,ei_sort,new_ei = check_exp (tev,ei_sev) ei in
                 (* and check that it is compatible with the sort of the branches *)
                 if not (Bunify.unify e0.info ei_sort branches_sort) then
                     sort_error e0.info 
                       (fun () -> 
                          msg "@[in@ match:@ %s@ expected@ but@ %s@ found@]"
                            (string_of_sort branches_sort)
                            (string_of_sort ei_sort));                   
                   let new_pl_rev' = (pi,new_ei)::new_pl_rev in 
                   (evsi,new_pl_rev'))
        (evs1,[]) pl in 
      let e0_sort = branches_sort in 
      let new_e0 = ECase(new_e1,Safelist.rev new_pl_rev) in 
      (new_evs,e0_sort,mk_checked_exp e0.info new_e0 e0_sort)
        
and check_binding evs b0 = match b0.desc with
  | Bind(p,e) -> match p.desc with
      | PVar(x) -> 
          let qx = Qid.t_of_id x in 
          let evs1,e_sort,new_e = check_exp evs e in
          let evs2 = match p.annot with
            | None -> evs1 
            | Some s -> 
                let evs2,s' = resolve_sort b0.info evs s in 
                  if not (Bunify.unify b0.info e_sort s') then
                    sort_error b0.info
                      (fun () ->
                         msg "@[in@ let-binding:@ %s@ expected@ but@ %s@ found@]"
                           (string_of_sort s')
                           (string_of_sort e_sort));
                  evs2 in 
          let () = p.annot <- Some e_sort in 
          let tev2,sev2 = evs2 in 
          let sev_fsvs = scenv_free_svs b0.info sev2 in
            Trace.debug "check"
              (fun () -> 
                 msg "@[generalizing ";
                 format_sort e_sort;
                 msg "...@]@\n");
          let e_scheme = Bunify.generalize b0.info sev_fsvs e_sort in 
            Trace.debug "check"
              (fun () -> 
                 msg "@[...to ";
                 format_sort (snd e_scheme);
                 msg "@]@\n");
          let sev3 = SCEnv.update sev2 qx (G.Scheme e_scheme) in 
          let final_e = mk_checked_exp new_e.info new_e.desc e_sort in 
          let new_b = mk_annot_binding b0.info (Bind(p,final_e)) e_scheme in 
          ((tev2,sev3),[qx],new_b)
      | _ -> 
          sort_error b0.info 
            (fun () -> msg "@[in@ let-binding: pattern must be a variable@]") 

(** old code for handling patterns in lets **)
(*       (\* then calculate bindings for variables mentioned in p *\) *)
(*       let new_tev,new_sev = new_evs in *)
(*       let bindso = static_match i new_sev p e_sort in *)
(*       let sev2,xs_rev = match bindso with *)
(*         | None -> *)
(*             (\* if pattern does not match the sort, raise an exception *\) *)
(*             sort_error i *)
(*               (fun () -> msg "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" *)
(*                  (string_of_pat p) *)
(*                  (string_of_sort e_sort)) *)
(*         | Some binds -> *)
(*             (\* otherwise extend the environment with the new bindings *\) *)
(*             Safelist.fold_left *)
(*               (fun (sevi,xs) (q,s) ->  *)
(*                  let sev_fsvs = scenv_free_svs i sevi in  *)
(*                  let q_scheme = Scheme(Bunify.generalize i sev_fsvs s) in  *)
(*                  let sevi2 = SCEnv.update sevi q q_scheme in  *)
(*                  (sevi2, q::xs)) *)
(*               (new_sev,[]) binds in *)


(* type check a single declaration *)
let rec check_decl (_,sev) ms d0 = match d0.desc with
  | DLet(b) ->
      (* discard old tev *)
      let evs = ([],sev) in 
      let bevs,xs,new_b = check_binding evs b in 
      let new_d = DLet(new_b) in
      (bevs,xs,mk_decl d0.info new_d)
  | DMod(n,ds) ->
      let qmn = Qid.t_dot_id (SCEnv.get_mod sev) n in 
      let evs = ([],SCEnv.set_mod sev qmn) in 
      let ms = ms @ [n] in 
      (* check the module *)
      let (m_tev,m_sev),names,new_ds = check_module_aux evs ms ds in
      let n_sev, names_rev = Safelist.fold_left 
        (fun (n_sev, names) q -> 
           match SCEnv.lookup m_sev q with
               None -> run_error d0.info
                 (fun () -> 
                    msg "@[declaration for %s missing@]"
                      (Qid.string_of_t q))
             | Some s ->
                 (* prefix the qualifiers in each name with n *)
                 let nq = Qid.splice_id_dot n q in
                 (SCEnv.update n_sev nq s, nq::names))
        (m_sev,[]) names in 
      let new_d = DMod(n,new_ds) in 
      ((m_tev,n_sev),Safelist.rev names_rev,mk_decl d0.info new_d)

  | DType(sl,qx,cl) -> 
      (* discard old tev *)
      let evs = ([],sev) in 
      (* get module prefix *)
      let qm = SCEnv.get_mod sev in 
      let (tev1,sev1),new_sl_rev = Safelist.fold_left
        (fun (evsi,acc) si -> 
           let evsi2,new_so = resolve_sort d0.info evsi si in 
           (evsi2,new_so::acc))
        (evs,[]) sl in 
      let new_sl = Safelist.rev new_sl_rev in 
      let svl = Bunify.svl_of_sl d0.info new_sl in 
      let new_qx = Qid.t_dot_t qm qx in 
      (* put dummy in environment *)
      let sev2 = SCEnv.update_type sev1 svl new_qx [] in 
      (* resolve sorts in cl *)
      let (tev3,_),new_cl_rev = 
        Safelist.fold_left  
          (fun (evsi,acc) (x,so) -> 
             match so with 
               | None -> (evsi,(x,so)::acc)
               | Some s -> 
                   let evsi2,new_s = resolve_sort d0.info evsi s in 
                   (evsi2,(x,Some new_s)::acc))
          ((tev1,sev2),[]) cl in 
      let new_cl = Safelist.rev new_cl_rev in 
      let new_qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) new_cl in
      (* put real binding in environment -- note! discard sev2 *)
      let sev3 = SCEnv.update_type sev1 svl new_qx new_qcl in         
      (* construct the datatype *)
      let sx = SData(new_sl,new_qx) in         
      (* add constructors to sev *)
      let evs4 = Safelist.fold_left 
        (fun evsi (l,so) ->            
           let (tevi2,sevi2),s = match so with 
             | None -> (evsi,sx)
             | Some s -> 
                 let evsi2,new_s = resolve_sort d0.info evsi s in 
                 (evsi2,SFunction (Id.wild,new_s,sx)) in 
           let scheme = G.Scheme (mk_scheme svl s) in
           let sevi3 = SCEnv.update sevi2 (Qid.t_of_id l) scheme in 
           (tevi2,sevi3))
        (tev3,sev3) cl in 
      let new_d = DType(new_sl,new_qx,new_cl) in 
      (evs4,[],mk_checked_decl d0.info new_d sx)
          
  | DTest(e1,tr) -> 
      (* discard tev *)
      let evs = ([],sev) in 
      (* check the expression *)
      let evs1,e1_sort,new_e1 = check_exp evs e1 in
      let evs2,new_tr = match tr with 
        | TestError 
        | TestShow -> evs1,tr
        | TestValue e2 -> 
            (* for values, check that the exps have compatible types *)
            (* TODO: this should only be for "equality types"?! *)
            let evs2,e2_sort,new_e2 = check_exp evs1 e2 in 
            if not (Bunify.unify e2.info e2_sort e1_sort) then
              sort_error e2.info
                (fun () -> 
                   msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort e1_sort)
                     (string_of_sort e2_sort));
            (evs2, TestValue new_e2)
        | TestLensType(e21o,e22o) -> 
            (* for lens type results, check that both exps are regexps *)
            let chk_eo evsi = function
              | None -> (evsi,None)
              | Some e -> 
                  let evsi2,e_sort,new_e = check_exp evsi e in 
                  if not (Bunify.unify e.info e_sort SRegexp) then
                    sort_error e.info 
                      (fun () ->
                         msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                           (string_of_sort SRegexp)
                           (string_of_sort e_sort));
                  (evsi2,Some new_e) in 
            let evs2,new_e21o = chk_eo evs1 e21o in 
            let evs3,new_e22o = chk_eo evs2 e22o in 
           (evs3,TestLensType(new_e21o,new_e22o)) in 
      let new_d = DTest(new_e1,new_tr) in 
      (evs2,[],mk_decl d0.info new_d)
          
and check_module_aux evs m ds = 
  let m_evs, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (evs, names, new_ds_rev) di -> 
         let m_evs,new_names,new_di = check_decl evs m di in
           m_evs, names@new_names,new_di::new_ds_rev)
      (evs,[],[])
      ds in
    (m_evs, names, Safelist.rev new_ds_rev)
          
(* --------------- Instrumentation --------------- *)

(* instrumentation environment: maps svars to Qid.ts *)
module IEnv = Env.Make(
  struct
    type t = svar
    let compare (x,_) (y,_) = Pervasives.compare x y
    let to_string (x,_) = sprintf "<%d>" x
  end)

let format_ienv ienv = 
  IEnv.format_t ienv (fun x -> msg "@[%s@]" (Qid.string_of_t x))

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
let rec instrument_sort i (sev,iev) s0 = match Bunify.zonk i s0 with
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
      let s1_checker = instrument_sort i (sev,iev) s1 in 
      let s2_checker = instrument_sort i (sev,iev) s2 in 
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
      let s1_checker = instrument_sort i (sev,iev) s1 in
      let s2_checker = instrument_sort i (sev,iev) s2 in
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
      let svl,cl = match SCEnv.lookup_type sev qx with
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
                 let p = mk_checked_pat i (PVnt(q,None)) s0 in 
                 (p,ev)::acc
             | (q,Some s) -> 
                 let s_checker = instrument_sort i (sev,iev) s in
                 let py = mk_checked_pat i (PVar y) s in 
                 let p = mk_checked_pat i (PVnt(q,Some py)) s0 in 
                 let ey = mk_var i qy in 
                 (p,
                  mk_app i 
                    (mk_var i q)
                    (mk_app i s_checker ey))::acc)
          [] cl_inst in 
      mk_fun i v s0 (mk_case i ev (Safelist.rev cl_rev'))

and instrument_exp (sev,iev) e0 = 
  let res = match e0.desc with
  | EVar(q) -> 
      let _,sl = e0.annot in 
      let chkl = 
        try Safelist.map (instrument_sort e0.info (sev,iev)) sl 
        with err -> 
          msg "ERR in"; 
          format_exp e0;
          msg "@\n";
          raise err
      in 
      Safelist.fold_right 
        (fun chki acc -> mk_app e0.info acc chki)
        chkl e0

  | EApp(e1,e2) ->
      let i1 = instrument_exp (sev,iev) e1 in 
      let i2 = instrument_exp (sev,iev) e2 in 
      mk_exp e0.info (EApp(i1,i2))

  | ELet(b,e) -> 
      let ib = instrument_binding (sev,iev) b in 
      let ie = instrument_exp (sev,iev) e in 
      mk_exp e0.info (ELet(ib,ie))

  | EFun(p1,_,e1) -> 
      let s0_checker = 
        try instrument_sort e0.info (sev,iev) (get_sort e0)
        with err -> 
          msg "ERR in"; 
          format_exp e0;
          msg "@\n";
          raise err
      in 
      let i1 = instrument_exp (sev,iev) e1 in 
      let i0 = mk_exp e0.info (EFun(p1,None,i1)) in 
      mk_exp e0.info (EApp(s0_checker,i0))

  | EPair(e1,e2) ->
      let i1 = instrument_exp (sev,iev) e1 in 
      let i2 = instrument_exp (sev,iev) e2 in 
      mk_exp e0.info (EPair(i1,i2)) 

  | ECase(e1,cl) -> 
      let i1 = instrument_exp (sev,iev) e1 in 
      let new_cl_rev = 
        Safelist.fold_left 
          (fun acc (pi,ei) -> (pi,instrument_exp (sev,iev) ei)::acc)
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

and instrument_binding (sev,iev) b0 = 
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
          assoc (instrument_exp (sev,fiev) e) in 
      mk_annot_binding b0.info (Bind(p,new_e)) (svl,s)

  | Bind _,None -> 
      run_error b0.info
        (fun () -> msg "@[unannotated binding!@ ";
                   Bprint.format_binding b0;
                   msg "@]")

and instrument_decl (sev,iev) d0 =
  Trace.debug "instr" 
    (fun () -> msg "@[instrument_decl: "; format_decl d0; msg "@]@\n"); 
  let res = match d0.desc with
  | DLet(b) ->
      let ib = instrument_binding (sev,iev) b in 
      mk_decl d0.info (DLet(ib)) 

  | DMod(n,ds) ->
      let ids = instrument_module_aux (sev,iev) ds in 
      mk_decl d0.info (DMod(n,ids)) 

  | DType(sl,qx,cl) -> d0
      
  | DTest(e1,tr) -> 
      let i1 = instrument_exp (sev,iev) e1 in 
      let itr = match tr with
        | TestValue e2 -> 
            let i2 = instrument_exp (sev,iev) e2 in 
            TestValue(i2)
        | TestLensType(e1o,e2o) -> 
            let instr = instrument_exp (sev,iev) in
            let i1o = Misc.map_option instr e1o in 
            let i2o = Misc.map_option instr e2o in 
            TestLensType(i1o,i2o)
        | _ -> tr in 
      mk_decl d0.info (DTest(i1,itr)) in
    Trace.debug "instr_test"
      (fun () -> msg "@[~> "; format_decl res; msg "@]@\n"); 
    res

and instrument_module_aux (sev,iev) ds = 
  Safelist.fold_right
    (fun di acc -> instrument_decl (sev,iev) di::acc)
    ds []

(* entry point to static analysis / instrumentation *)
let check_module m0 = 
  match m0.desc with
  | Mod(m,nctx,ds) -> 
      let tev = [] in 
      let qm = Qid.t_of_id m in 
      let sev = SCEnv.set_ctx (SCEnv.empty qm) (m::nctx@G.pre_ctx) in
      let (_,checked_sev),_,checked_ds = check_module_aux (tev,sev) [m] ds in 
      let instrumented_ds = if false then instrument_module_aux (checked_sev,IEnv.empty ()) checked_ds else checked_ds in 
      let res = mk_mod m0.info (Mod(m,nctx,instrumented_ds)) in 
      Trace.debug "instr+" (fun () -> format_module res; msg "@\n");
      res

(* --------------- Compiler --------------- *)
(* expressions *)
let rec compile_exp cev e0 = match e0.desc with 
  | EVar(q) ->       
      begin match CEnv.lookup cev q with
        | Some(_,v) -> v
        | None -> 
            run_error e0.info 
              (fun () -> msg "@[%s is not bound@]" (Qid.string_of_t q))
      end

  | EApp(e1,e2) ->
      let v1 = compile_exp cev e1 in 
      let v2 = compile_exp cev e2 in 
      (V.get_f v1) v2

  | ELet(b,e) -> 
      let bcev,_ = compile_binding cev b in
      compile_exp bcev e
          
  | EFun(p,_,e) ->
      let f v =
        let body_cev = 
          CEnv.update cev 
            (Qid.t_of_id (id_of_param p))
            (G.Unknown, v) in 
      compile_exp body_cev e in 
      V.Fun (V.blame_of_info e0.info,f)

  | EUnit -> 
      V.Unt (V.blame_of_info e0.info)

  | EPair(e1,e2) -> 
      let v1 = compile_exp cev e1 in 
      let v2 = compile_exp cev e2 in 
      V.Par(V.blame_of_info e0.info,v1,v2)

  | ECase(e1,pl) -> 
      let v1 = compile_exp cev e1 in 
      (* find_match implements first-match *)
      let rec find_match = function
        | [] -> 
            run_error e0.info 
              (fun () -> msg "@[match@ failure@]")
        | (pi,ei)::rest -> 
            (match dynamic_match e0.info pi v1 with 
               | None -> find_match rest
               | Some l -> l,ei) in 
      let binds,ei = find_match pl in 
      let qid_binds = 
        Safelist.map 
          (fun (x,s,v) -> (Qid.t_of_id x,(G.Sort s,v))) binds in         
      let ei_cev = CEnv.update_list cev qid_binds in
      compile_exp ei_cev ei

  | EString(s) -> 
      V.Str(V.blame_of_info e0.info,s)

  | EInteger(i) -> 
      V.Int(V.blame_of_info e0.info,i)

  | ECSet(pos,cs) -> 
      let mk_rx = if pos then R.set else R.negset in 
      V.Rx (V.blame_of_info e0.info, mk_rx cs)

and compile_binding cev b0 = match b0.desc,b0.annot with
  | Bind(p,e),Some(svl,s) -> 
      let v = compile_exp cev e in 
      let bindso = dynamic_match b0.info p v in 
      begin match bindso with 
        | None -> 
            run_error b0.info
              (fun () -> msg "@[pattern %s and value %s do not match@]" 
                 (string_of_pat p)
                 (V.string_of_t v))
        | Some binds -> 
            let cev_fsvs = cenv_free_svs b0.info cev in 
            let binds_schemes = 
              Safelist.map 
                (fun (x,s,v) -> 
                   let qx = Qid.t_of_id x in 
                   let ss = Bunify.generalize b0.info cev_fsvs s in 
                   (qx,(G.Scheme ss,v)))
                binds in 
            let bcev = CEnv.update_list cev binds_schemes in 
            let xs = Safelist.map (fun (qx,_) -> qx) binds_schemes in 
            (bcev,xs)
      end
  | _ -> 
      run_error b0.info 
        (fun () -> msg "@[unannotated binding@]")
  
let rec compile_decl cev ms d0 = match d0.desc with 
  | DLet(b) -> 
      let bcev,xs = compile_binding cev b in
      (bcev,xs)
  | DType(sl,qx,cl) -> 
      let x_sort = get_decl_sort d0 in
      let svl = Bunify.svl_of_sl d0.info sl in 
      let x_scheme = G.Scheme (svl,x_sort) in 
      let new_cev = Safelist.fold_left 
        (fun cev (l,so) -> 
           let ql = Qid.t_of_id l in 
           let rv = match so with 
             | None -> (x_scheme,V.Vnt(V.blame_of_info d0.info,qx,l,None))
             | Some s -> 
                 let sf = SFunction(Id.wild,s,x_sort) in 
                 let sf_scheme = G.Scheme (svl,sf) in
                 (sf_scheme, 
                  V.Fun(V.blame_of_info d0.info,
                        (fun v -> V.Vnt(V.blame_of_t v,qx,l,Some v)))) in 
           CEnv.update cev ql rv)
        cev cl in 
      let qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) cl in 
      let new_cev' = CEnv.update_type new_cev svl qx qcl in   
      (new_cev',[qx])

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
                    msg "%!")
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
