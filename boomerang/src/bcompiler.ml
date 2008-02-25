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
(* /boomerang/src/compiler.ml                                                  *)
(* Boomerang type checker and interpreter                                      *)
(* $Id$ *)
(*******************************************************************************)

open Bsyntax
open Berror
module RS = Bstring
module R = Bregexp
module L = Blenses.DLens
module C = Blenses.Canonizer
module V = Bvalue

(* --------------- Imports --------------- *)
let sprintf = Printf.sprintf  
let msg = Util.format
let (@) = Safelist.append

let s_of_rv = Bregistry.scheme_of_rv 
let v_of_rv = Bregistry.value_of_rv 
let p_of_rv rv = (s_of_rv rv, v_of_rv rv)
let mk_rv = Bregistry.make_rv

(* --------------- Unit tests --------------- *)

(* unit tests either succeed, yielding a value, or fail with a msg *)
type testresult = OK of sort * Bvalue.t | Error of (unit -> unit)

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
    (fun r qs -> r or (id_prefix (Bvalue.parse_qid qs) ms))
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
module type CEnvSig = 
sig
  type t 
  type v
  val empty : unit -> t
  val get_ev : t -> Bregistry.REnv.t
  val set_ev : t -> Bregistry.REnv.t -> t
  val get_ctx : t -> id list
  val set_ctx : t -> id list -> t
  val lookup : t -> qid -> v option
  val update : t -> qid -> v -> t
  val fold : (qid -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

module TEnv = struct 
  (* map from type names to sort lists *)
  module M = Env.Make(struct
                        type t = qid
                        let compare = qid_compare
                        let to_string = string_of_qid
                      end)
  (* reverse map from constructor names to data type descrs. *)
  module MCon = Env.Make (struct
                            type t = id
                            let compare = id_compare
                            let to_string = string_of_id
                          end)
  type sl = (id * sort option) list
  type t = (sl M.t * qid MCon.t)
  let empty () = (M.empty (), MCon.empty ())
  let lookup (t,_) x = M.lookup t x
  let lookup_con (t,tcon) x = match MCon.lookup tcon x with 
    | None -> None
    | Some q -> begin match M.lookup t q with 
        | None -> 
            run_error (Info.M "TEnv.lookup_con") 
              (fun () -> msg "datatype %s missing" (string_of_qid q))
        | Some sl -> Some (q,sl)
      end
  let update (t,tcon) x sl = 
    let t' = M.update t x sl in 
    let tcon' = 
      Safelist.fold_left 
        (fun tcon (xi,so) -> MCon.update tcon xi x)
        tcon sl in 
    (t',tcon')
end

module CEnv : CEnvSig with type v = (scheme * Bvalue.t) = 
struct
  type t = id list * (Bregistry.REnv.t)
  type v = scheme * Bvalue.t

  let empty () = ([], (Bregistry.REnv.empty ()))    

  (* getters and setters *)
  let get_ev cev = let (_,ev) = cev in ev
  let set_ev cev ev = let (os,_) = cev in (os,ev)
  let get_ctx cev = let (os,_) = cev in os
  let set_ctx cev os = let (_,ev) = cev in (os,ev)

  (* lookup from a cev, then from the library *)
  let lookup cev q = match Bregistry.REnv.lookup (get_ev cev) q with
    | None -> 
        begin match Bregistry.lookup_library_ctx (get_ctx cev) q with
          | None -> None
          | Some rv -> Some (p_of_rv rv)
        end
    | Some rv -> Some (p_of_rv rv)

  (* update a cev with a new rv *)
  let update cev q (s,v) = 
    set_ev cev (Bregistry.REnv.update (get_ev cev) q (mk_rv s v))

  let fold f cev a = 
    let ev = get_ev cev in   
    Bregistry.REnv.fold (fun q v a -> f q (s_of_rv v,v_of_rv v) a) ev a
end


type cenv = CEnv.t
let empty_cenv () = CEnv.empty ()

module SCEnv : CEnvSig with type v = scheme = 
struct
  type t = CEnv.t
  type v = scheme 

  let dummy_value = Bvalue.Unt (Info.M "dummy value") 
  let empty = CEnv.empty        
  let get_ev = CEnv.get_ev
  let set_ev = CEnv.set_ev   
  let get_ctx = CEnv.get_ctx
  let set_ctx = CEnv.set_ctx

  let lookup sev q = 
    match CEnv.lookup sev q with 
    | None -> None
    | Some (s,_) -> Some s
  let update sev q s = CEnv.update sev q (s,dummy_value)
  let fold f sev a = CEnv.fold (fun q (s,_) a -> f q s a) sev a
end

(* --------------- Checker --------------- *)

let cenv_free_svs cev = 
  CEnv.fold 
    (fun _ ((svsi,si),_) acc -> SVSet.union acc (SVSet.diff (free_svs si) svsi))
    cev SVSet.empty

let scenv_free_svs sev = 
  SCEnv.fold 
    (fun _ (svsi,si) acc -> SVSet.union acc (SVSet.diff (free_svs si) svsi))
    sev SVSet.empty

(* helper: check if a sort matches a pattern; return bindings for variables *)
let rec static_match i tev p s = 
  let err p s1 s2 = sort_error i 
    (fun () -> msg "@[in@ pattern@ %s:@ expected %s,@ but@ found@ %s@]"
       (string_of_pat p)
       (string_of_sort s1)
       (string_of_sort s2)) in 
  match p,s with 
    | PWld _,_ -> Some []
    | PVar(_,x),_ -> Some [(x,scheme_of_sort s)]
    | PUnt(_),s -> 
        if not (unify i s SUnit) then err p SUnit s;
        Some []
  | PVnt(_,li,pio),s -> 
      (* lookup which datatype we have using li *)
      begin match TEnv.lookup_con tev li with
        | None -> 
            sort_error i 
              (fun () -> msg "@[Unbound@ constructor@ %s@]" (string_of_id li))
        | Some (q,sl) -> 
            let s_expect = SData([],q) in 
            if not (unify i s s_expect) then err p s_expect s;
            let rec aux = function
              | [] -> None
              | (lj,sjo)::rest -> 
                  if (id_equal li lj) then 
                    (match pio,sjo with 
                       | None,None -> Some []
                       | Some pi,Some sj -> static_match i tev pi sj
                       | _ -> sort_error i (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" (string_of_id li)))
                  else aux rest in 
              aux sl
      end
  | PPar(_,p1,p2),SProduct(s1,s2) -> 
      (match static_match i tev p1 s1, static_match i tev p2 s2 with 
         | Some l1, Some l2 -> Some (l1 @ l2) 
         | _ -> None)
  | _ -> None 

let rec dynamic_match i p v = match p,v with 
  | PWld(_),_ -> Some []
  | PVar(_,x),_ -> Some [(x,v)]
  | PUnt(_),V.Unt(_) -> Some []
  | PVnt(_,li,pio),V.Vnt(_,_,lj,vjo) -> 
      if (id_equal li lj) then 
        (match pio,vjo with 
           | None,None -> Some []
           | Some pi,Some vj -> dynamic_match i pi vj
           | _ -> 
               run_error i 
                 (fun () -> msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" (string_of_id li)))
      else None
  | PPar(_,p1,p2),V.Par(_,v1,v2) -> 
      (match dynamic_match i p1 v1,dynamic_match i p2 v2 with 
         | Some l1,Some l2 -> Some (l1 @ l2)
         | _ -> None)
  | _ -> None 

let rec check_exp ((tev,sev) as evs) e0 = match e0 with
  (* overloaded, polymorphic operators *)
  | EOver(i,s) -> 
      let mk_bin c q = 
        let alpha = fresh_sort (Con c) in 
        let e0_sort = SFunction(alpha,SFunction(alpha,alpha)) in 
        let new_e0 = EVar(i,q) in
        (e0_sort,new_e0) in 
      let mk_un c q = 
        let alpha = fresh_sort (Con c) in 
        let e0_sort = SFunction(alpha,alpha) in 
        let new_e0 = EVar(i,q) in 
        (e0_sort,new_e0) in
      begin match s with 
      | ODot -> mk_bin Str (mk_native_prelude_qid "poly_concat")
      | OBar -> mk_bin Reg (mk_native_prelude_qid "poly_union")
      | OStar -> mk_un Reg (mk_native_prelude_qid "poly_star")
      | OTilde -> mk_bin Lns (mk_native_prelude_qid "poly_swap")
    end

  | EVar(i,q) ->
      let e0_sort = match SCEnv.lookup sev q with
        | Some ss -> instantiate ss 
        | None -> 
            sort_error i 
              (fun () -> msg "@[%s is not bound@]" 
                 (string_of_qid q)) in 
      let new_e0 = e0 in 
      (e0_sort,new_e0)

  | EFun(i,p,ret_sorto,body) ->
      let p_id = id_of_param p in
      let p_sort = sort_of_param p in 
      let body_sev = SCEnv.update sev (qid_of_id p_id) (scheme_of_sort p_sort) in
      let body_sort,new_body = check_exp (tev,body_sev) body in       
      (match ret_sorto with 
        | None -> ()
        | Some ret_sort -> 
            if not (unify i body_sort ret_sort) then 
              sort_error i 
                (fun () -> 
                   msg "@[in@ function:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort ret_sort)
                     (string_of_sort body_sort)));      
      let e0_sort = SFunction(p_sort,body_sort) in 
      let new_e0 = EFun(i,p,Some body_sort,new_body) in 
      (e0_sort, new_e0)

  | ELet(i,b,e) ->
      let bevs,_,new_b = check_binding evs b in 
      let e0_sort,new_e = check_exp bevs e in 
      let new_e0 = ELet(i,new_b,new_e) in 
      (e0_sort, new_e0)

  | EUnit(_) -> (SUnit,e0)

  | EString(_,_) -> (SString,e0)

  | ECSet(_) -> (SRegexp,e0)

  | EPair(i,e1,e2) -> 
      let e1_sort,new_e1 = check_exp evs e1 in 
      let e2_sort,new_e2 = check_exp evs e2 in 
      let e0_sort = SProduct(e1_sort,e2_sort) in 
      let new_e0 = EPair(i,new_e1,new_e2) in 
      (e0_sort,new_e0)
 
  (* elimination forms *)
  | EApp(i,e1,e2) -> 
      let e1_sort,new_e1 = check_exp evs e1 in 
      let e2_sort,new_e2 = check_exp evs e2 in 
      (* msg "@[IN APP: "; format_exp e0; msg "@]@\n";
      msg "@[E1_SORT: %s@\n@]" (string_of_sort e1_sort);
      msg "@[E2_SORT: %s@\n@]" (string_of_sort e2_sort); *)
      let param_sort = fresh_sort Fre in 
      let ret_sort = fresh_sort Fre in 
      let sf = SFunction(param_sort,ret_sort) in        
      if not (unify i e1_sort sf) then
        sort_error i 
          (fun () -> 
             msg "@[in@ application:@ %s@ expected@ but@ %s@ found@]"
               (string_of_sort sf)
               (string_of_sort e1_sort));
      if not (unify i e2_sort param_sort) then 
        sort_error i 
          (fun () -> 
             msg "@[in@ application:@ %s@ expected@ but@ %s@ found@]"
               (string_of_sort param_sort)
               (string_of_sort e2_sort));
      let e0_sort = ret_sort in 
      let new_e0 = EApp(i,new_e1,new_e2) in 
      (e0_sort,new_e0)

  | ECase(i,e1,pl) -> 
      let err2 i p s1 s2 = sort_error i (fun () -> msg p s1 s2) in 
      let e1_sort,new_e1 = check_exp evs e1 in 
      let branches_sort = fresh_sort Fre in 
      let new_pl_rev = Safelist.fold_left 
        (fun new_pl_rev (pi,ei) -> 
           match static_match i tev pi e1_sort with 
             | None -> 
                 err2 i "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" 
                   (string_of_pat pi) 
                   (string_of_sort e1_sort)
             | Some binds -> 
                 let ei_sev = Safelist.fold_left 
                   (fun ei_sev (xj,sj) -> SCEnv.update ei_sev (qid_of_id xj) sj)
                   sev binds in 
                 let ei_sort,new_ei = check_exp (tev,ei_sev) ei in
                 if not (unify i ei_sort branches_sort) then
                   sort_error i 
                     (fun () -> 
                        msg "@[in@ match:@ %s@ expected@ but@ %s@ found@]"
                          (string_of_sort branches_sort)
                          (string_of_sort ei_sort));                   
                 let new_pl_rev' = (pi,new_ei)::new_pl_rev in 
                 new_pl_rev')
        [] pl in 
      let e0_sort = branches_sort in 
      let new_e0 = ECase(i,new_e1,Safelist.rev new_pl_rev) in 
      (e0_sort,new_e0)
        
and check_binding ((tev,sev) as evs) = function
  | Bind(i,PVar(ix,x),sorto,e) -> 
      let e_sort,new_e = check_exp evs e in 
      let qx = qid_of_id x in 
      let sev_fsvs = scenv_free_svs sev in 
      let x_scheme = generalize sev_fsvs e_sort in 
      let bsev = SCEnv.update sev qx x_scheme in 
      (match sorto with 
        | None -> ()
        | Some s -> 
            if not (unify i e_sort s) then 
              sort_error i 
                (fun () -> 
                   msg "@[in@ let-binding:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort s)
                     (string_of_sort e_sort)));      
      let new_b = Bind(i,PVar(ix,x),Some e_sort,new_e) in 
      msg "@[BINDING %s has sort %s@\n@]" (string_of_id x) (string_of_scheme x_scheme);
      ((tev,bsev),[qx],new_b)
  | Bind(i,p,_,e) ->      
      let e_sort,new_e = check_exp evs e in 
      let bindso = static_match i tev p e_sort in 
      let bsev,xs_rev = match bindso with 
        | None -> sort_error i 
            (fun () -> msg "@[pattern@ %s@ does@ not@ match@ sort@ %s@]"
               (string_of_pat p) 
               (string_of_sort e_sort))
        | Some binds -> 
            Safelist.fold_left 
              (fun (bsev,xs) (x,s) -> 
                 let qx = qid_of_id x in 
                 (SCEnv.update bsev qx s, qx::xs))
              (sev,[]) binds in 
      let new_b = Bind(i,p,Some e_sort,new_e) in 
      ((tev,bsev),Safelist.rev xs_rev,new_b)

(* type check a single declaration *)
let rec check_decl ((tev,sev) as evs) ms = function 
  | DLet(i,b) -> 
      let bevs,xs,new_b = check_binding evs b in 
      let new_d = DLet(i,new_b) in
      (bevs,xs,new_d)
  | DMod(i,n,ds) ->
      let ms = ms @ [n] in 
      let (m_tev,m_sev),names,new_ds= check_module_aux evs ms ds in
      let n_sev, names_rev = Safelist.fold_left 
        (fun (n_sev, names) q -> 
           match Bregistry.REnv.lookup (SCEnv.get_ev m_sev) q with
              None -> run_error i 
                (fun () -> 
                  msg "@[declaration for %s missing@]"
                    (string_of_qid q))
            | Some q_rv ->
                let nq = splice_id_dot n q in
                (SCEnv.update n_sev nq (s_of_rv q_rv), nq::names))
        (sev,[])
        names in 
      let new_d = DMod(i,n,new_ds) in 
      ((m_tev,n_sev),Safelist.rev names_rev,new_d)

  | DType(i,sl,x,cl) as d -> 
      let qx = qid_of_id x in 
      let sx = SData(sl,qx) in
      let scenv_fsvs = scenv_free_svs sev in
      let new_sev = Safelist.fold_left 
        (fun sev (l,so) -> 
           let ql = qid_of_id l in 
           let s = match so with 
             | None -> sx
             | Some s -> SFunction (s,sx) in
             SCEnv.update sev ql (generalize scenv_fsvs s))
        sev cl in 
      let new_tev = TEnv.update tev qx cl in 
      ((new_tev,new_sev),[],d)
      
  | DTest(i,e1,tr) -> 
      let e1_sort,new_e1 = check_exp evs e1 in
      let new_tr = match tr with 
        | TestError -> tr
        | TestShow -> tr
        | TestValue e2 -> 
            let e2_sort,new_e2 = check_exp evs e2 in 
            if not (unify i e2_sort e1_sort) then
              sort_error i 
                (fun () -> 
                   msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                     (string_of_sort e1_sort)
                     (string_of_sort e2_sort));
            TestValue (new_e2) 
        | TestSort _ -> tr
        | TestLensType(e21o,e22o) -> 
            let chk_eo = function
              | None -> None
              | Some e -> 
                  let e_sort,new_e = check_exp evs e in 
                    if not (unify i e_sort SRegexp) then
                      sort_error i 
                        (fun () ->
                           msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
                             (string_of_sort SRegexp)
                             (string_of_sort e_sort));
                    Some new_e in 
            TestLensType(chk_eo e21o, chk_eo e22o) in 
      let new_d = DTest(i,new_e1,new_tr) in 
      (evs,[],new_d)
          
and check_module_aux evs m ds = 
  let m_evs, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (evs, names, new_ds_rev) di -> 
        let m_evs,new_names,new_di = check_decl evs m di in
          m_evs, names@new_names,new_di::new_ds_rev)
      (evs,[],[])
      ds in
  (m_evs, names, Safelist.rev new_ds_rev)

let check_module = function
  | Mod(i,m,nctx,ds) -> 
      let tev = TEnv.empty () in 
      let sev = SCEnv.set_ctx (SCEnv.empty ()) (nctx@Bregistry.pre_ctx) in
      let _,_,new_ds = check_module_aux (tev,sev) [m] ds in 
      Mod(i,m,nctx,new_ds)

(* --------------- Compiler --------------- *)
(* expressions *)
let rec compile_exp cev e0 = match e0 with
  | EOver(i,s) -> 
      run_error i 
        (fun () -> 
           msg "@[unresolved overloaded symbol ";
           (format_sym s);
           msg "@]")

  | EVar(i,q) -> 
      begin match CEnv.lookup cev q with
        | Some((_,SRegexp),v) -> 
            (* rethink this: it renames regexps too agressively! --JNF *)
            let x = string_of_qid q in 
            let r' = Bregexp.set_str (Bvalue.get_r v i) x in
            let rv' = scheme_of_sort SRegexp, Bvalue.Rx(i,r') in 
            rv'
        | Some rv -> rv
        | None -> run_error i (fun () -> msg "@[%s is not bound@]" (string_of_qid q))
      end

  | EApp(i,e1,e2) ->
      let (_,s1),v1 = compile_exp cev e1 in
      let (_,s2),v2 = compile_exp cev e2 in
      begin match s1,v1 with
        | (SFunction(_,ret_sort)),Bvalue.Fun(_,_,_,f) -> 
          (scheme_of_sort ret_sort, f i v2)
        | _   -> 
            run_error i 
              (fun () -> 
                msg
                  "@[expected function in left-hand side of application but found %s"
                  (string_of_sort s1))
      end

  | ELet(i,b,e) ->
      let bcev,_ = compile_binding cev b in       
      compile_exp bcev e
        
  | EFun(i,p,Some ret_sort,e) ->
      let p_sort = sort_of_param p in 
      let p_id = id_of_param p in 
      let f_sort = SFunction(p_sort,ret_sort) in
      let f_impl i v =
        let p_qid = qid_of_id p_id in 
        let body_cev = CEnv.update cev p_qid (scheme_of_sort p_sort, v) in
        snd (compile_exp body_cev e) in 
      (scheme_of_sort f_sort, (Bvalue.Fun (i,p_sort,ret_sort,f_impl)))

  | EFun(i,_,_,_) -> 
      run_error i 
        (fun () -> 
          msg 
            "@[compiler bug: unannotated function!@]")

  | EUnit i -> (scheme_of_sort SUnit,Bvalue.Unt i)

  | EPair(i,e1,e2) -> 
      let (_,s1),v1 = compile_exp cev e1 in 
      let (_,s2),v2 = compile_exp cev e2 in 
      (scheme_of_sort (SProduct(s1,s2)),Bvalue.Par(i,v1,v2))

  | ECase(i,e1,pl) -> 
      let s1,v1 = compile_exp cev e1 in 
      let rec find_match = function
        | [] -> run_error i (fun () -> msg "@[match@ failure@]")
        | (pi,ei)::rest -> 
            (match dynamic_match i pi v1 with 
               | None -> find_match rest
               | Some l -> l,ei) in 
      let binds,ei = find_match pl in 
      let ei_cev = Safelist.fold_left 
        (fun ei_cev (x,v) -> 
           let s = scheme_of_sort (Bvalue.sort_of_t v) in 
           CEnv.update ei_cev (qid_of_id x) (s,v))
        cev binds in 
        compile_exp ei_cev ei

  | EString(i,s) -> (scheme_of_sort SString,Bvalue.Str(i,s))

  | ECSet(i,true,cs) -> (scheme_of_sort SRegexp,Bvalue.Rx (i, R.set cs))

  | ECSet(i,false,cs) -> (scheme_of_sort SRegexp,Bvalue.Rx (i, R.negset cs))
      
and compile_binding cev = function
  | Bind(i,p,so,e) ->
      let (_,s),v = compile_exp cev e in 
      let bindso = dynamic_match i p v in 
      let cev_fsvs = cenv_free_svs cev in 
      let bcev,xs_rev = match bindso with 
        | None -> run_error i 
            (fun () -> msg "@[pattern %s and value %s do not match@]" 
               (string_of_pat p)
               (V.string_of_t v))
        | Some binds -> 
            Safelist.fold_left 
              (fun (bcev,xs) (x,v) -> 
                 let qx = qid_of_id x in 
                 let s = V.sort_of_t v in 
                 (CEnv.update bcev qx (generalize cev_fsvs s,v), qx::xs))
              (cev,[]) binds in 
      (bcev,Safelist.rev xs_rev)

let rec compile_decl cev ms = function
  | DLet(i,b) -> 
      let bcev,xs = compile_binding cev b in
      (bcev,xs)
  | DType(i,sl,x,cl) -> 
      let qx = qid_of_id x in 
      let sx = SData(sl,qx) in 
      let new_cev = Safelist.fold_left 
        (fun cev (l,so) -> 
           let ql = qid_of_id l in 
           let rv = match so with 
             | None -> (scheme_of_sort sx,V.Vnt(i,qx,l,None))
             | Some s -> 
                 let sf = SFunction(s,sx) in 
                   (scheme_of_sort sf,
                    V.Fun(i,s,sx,(fun i v -> V.Vnt(i,qx,l,Some v)))) in 
             CEnv.update cev ql rv)
        cev cl in 
      (new_cev,[qx])

  | DMod(i,n,ds) ->
      let m_cev, names = compile_mod_aux cev ms ds in
      let n_cev,names_rev = 
        Safelist.fold_left
          (fun (n_cev, names) q ->
           match Bregistry.REnv.lookup (CEnv.get_ev m_cev) q with
              | Some rv ->
                  let nq = splice_id_dot n q in
                  (CEnv.update cev nq (p_of_rv rv), nq::names)
              | None -> 
                  run_error i 
                    (fun () -> msg "@[compiled declaration for %s missing@]"
                      (string_of_qid q)))
          (cev,[])
          names in
        (n_cev, Safelist.rev names_rev)
  | DTest(i,e,tr) ->
      if check_test ms then 
        begin
          let vo = 
            try let (_,s),v = compile_exp cev e in 
            OK(s,v)
            with (Error.Harmony_error(err)) -> Error err in 
          match vo,tr with 
            | OK (_,v), TestShow ->
                msg "Test result:@ "; 
                Bvalue.format v; 
                msg "@\n%!"
            | OK (s0,v), TestSort(Some s) -> 
                if not (unify i s0 s) then
                  test_error i
                    (fun () -> 
                       msg "@\nExpected@ "; format_sort s;
                       msg "@ but found@ "; format_sort s0; 
                       msg "@\n%!")
            | OK(s0,v), TestSort None -> 
                msg "Test sort:@ %s@\n%!" (string_of_sort s0);
            | OK(_,v), TestLensType(e1o,e2o) -> 
                if not (unify i (Bvalue.sort_of_t v) SLens) then 
                  test_error i 
                    (fun () -> 
                       msg "@\nExpected@ "; format_sort SLens;
                       msg "@ but found@ "; Bvalue.format v;
                       msg "@\n%!");
                let l = Bvalue.get_l v i in 
                let c,a = L.ctype l,L.atype l in 
                let chk_eo r = function
                  | None -> true,"?"
                  | Some e -> 
                      let expected = Bvalue.get_r (snd (compile_exp cev e)) i in 
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
                  test_error i 
                    (fun () -> 
                       msg "@\nExpected@ "; 
                       msg "@[<2>%s <-> %s@]" c_str a_str;
                       msg "@ but found@ "; 
                       msg "@[<2>%s <-> %s@]" (R.string_of_t c) (R.string_of_t a);
                       msg "@\n%!");
                    end                      
            | Error err, TestShow 
            | Error err, TestSort _ 
            | Error err, TestLensType _ -> 
                test_error i 
                  (fun () -> 
                    msg "Test result: error";
                    err (); 
                    msg "%!")
            | Error _, TestError -> ()
            | OK(_,v), TestValue res -> 
                let resv = snd (compile_exp cev res) in
                  if not (Bvalue.equal v resv) then
                    test_error i 
                      (fun () ->
                        msg "@\nExpected@ "; Bvalue.format resv;
                        msg "@ but found@ "; Bvalue.format v; 
                        msg "@\n%!")
            | Error err, TestValue res -> 
                let resv = snd (compile_exp cev res) in
                  test_error i 
                    (fun () ->
                      msg "@\nExpected@ "; Bvalue.format resv; 
                      msg "@ but found an error:@ "; 
                      err (); 
                      msg "@\n%!")
            | OK(_,v), TestError -> 
                test_error i 
                  (fun () ->
                    msg "@\nExpected an error@ "; 
                    msg "@ but found:@ "; 
                    Bvalue.format v; 
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

let compile_module = function
  | Mod(i,m,nctx,ds) -> 
      let cev = CEnv.set_ctx (CEnv.empty ()) (nctx@Bregistry.pre_ctx) in
      let new_cev,_ = compile_mod_aux cev [m] ds in
      Bregistry.register_env (CEnv.get_ev new_cev) m
