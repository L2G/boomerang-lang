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
(* $Id$                                                                        *)
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
let (@) = Safelist.append

let s_of_rv = Bregistry.sort_of_rv 
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

let no_assert = Prefs.createBool "no-assert" false
  "don't check assertions"
  "don't check assertions"

(* --------------- Error Reporting --------------- *)
let debug s_thk = 
  Trace.debug "compiler" (fun () -> Util.format "@[%s@\n%!@]" (s_thk ()))

let test_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> Util.format "@[%s: Unit test failed @ " (Info.string_of_t i); 
              msg_thk ();
              Util.format "@]"))

let run_error i msg msg_thk = 
  raise (Error.Harmony_error
           (fun () -> Util.format "@[%s: Unexpected run-time error @\n"
              (Info.string_of_t i);
              msg_thk ();
              Util.format "@]"))

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
end

module TEnv = struct 
  module M = Env.Make(struct
                        type t = qid
                        let compare = qid_compare
                        let to_string = string_of_qid
                      end)
  type s = (id * sort option) list
  type t = s M.t
  let empty : unit -> t = M.empty 
  let lookup : t -> qid -> s option = M.lookup
  let update : t -> qid -> s -> t = M.update
end

module CEnv : CEnvSig with type v = (sort * Bvalue.t) = struct
  type t = id list * (Bregistry.REnv.t)
  type v = sort * Bvalue.t

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
end


type cenv = CEnv.t
let empty_cenv () = CEnv.empty ()

module SCEnv : CEnvSig with type v = sort = struct
  type t = CEnv.t
  type v = sort 

  let empty = CEnv.empty        
  let get_ev = CEnv.get_ev
  let set_ev = CEnv.set_ev   
  let get_ctx = CEnv.get_ctx
  let set_ctx = CEnv.set_ctx

  let lookup sev q = 
    match CEnv.lookup sev q with 
    | None -> None
    | Some (s,_) -> Some s
  let update sev q s = 
    CEnv.update sev q (s,Bvalue.Unt (Info.M "dummy"))
end

(* --------------- Checker --------------- *)

(* helper function to convert a list of parameters [ps] with sorts
 * [s1,..,sk] and a sort [s] to the arrow sort [s1 -> ... -> sk -> s] *)
let sort_of_params i ps s = 
  Safelist.fold_right (fun p ts -> SFunction(sort_of_param p,ts))  ps s

(* check if one sort is a subsort of another *)
let rec subsort u v = match u,v with        
  | SString,SRegexp -> true
  | SString,SLens -> true
  | SRegexp,SLens -> true
  | SFunction(s11,s12),SFunction(s21,s22) -> 
      let b1 = subsort s21 s11  in
      let b2 = subsort s12 s22 in 
        b1 && b2
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      let b1 = subsort s11 s21  in
      let b2 = subsort s12 s22 in 
        b1 && b2     
  | SVar x, SVar y -> qid_equal x y
  | _ -> u=v

let rec join i u v = 
  let err () = 
    run_error i "join"
      (fun () -> Util.format "%s and %s do not join"
         (string_of_sort u)
         (string_of_sort v)) in 
  match u,v with 
  | SString,SRegexp -> SRegexp
  | SRegexp,SString -> SRegexp
  | SString,SLens -> SLens
  | SLens,SString -> SLens
  | SRegexp,SLens -> SLens
  | SLens,SRegexp -> SLens
  | SFunction(s11,s12),SFunction(s21,s22) -> 
      begin 
        try SFunction(meet i s11 s12,join i s21 s22) 
        with _ -> run_error i "join" 
          (fun () -> Util.format "%s and %s do not join"
            (string_of_sort u)
            (string_of_sort v))
      end
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      begin 
        try SProduct(join i s11 s12,join i s21 s22) 
        with _ -> run_error i "join" 
          (fun () -> Util.format "%s and %s do not join"
             (string_of_sort u)
             (string_of_sort v))
      end
  | SVar x, SVar y -> if qid_equal x y then u else err ()
  | _ -> if u=v then u else err ()
         
and meet i u v = 
  let err () = 
    run_error i "meet"
      (fun () -> Util.format "%s and %s do not meet"
         (string_of_sort u)
         (string_of_sort v)) in       
  match u,v with
  | SString,SRegexp -> SString
  | SRegexp,SString -> SString
  | SString,SLens -> SString
  | SLens,SString -> SString
  | SRegexp,SLens -> SRegexp
  | SLens,SRegexp -> SRegexp
  | SFunction(s11,s12),SFunction(s21,s22) -> 
      begin 
        try SFunction(join i s11 s12,meet i s21 s22) 
        with _ -> run_error i "meet" 
          (fun () -> Util.format "%s and %s do not meet"
            (string_of_sort u)
            (string_of_sort v))
      end
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      begin 
        try SProduct(meet i s11 s12,meet i s21 s22) 
        with _ -> run_error i "meet" 
          (fun () -> Util.format "%s and %s do not meet"
            (string_of_sort u)
            (string_of_sort v))
      end
  | SVar x,SVar y -> if qid_equal x y then u else err ()
  | _ -> if u=v then u else err ()

(* pattern matching *)
(* helper: check if a sort matches a pattern; return bindings for variables *)
let rec static_match i tev p s = match p,s with 
  | PWld,_ -> Some []
  | PVar x,_ -> Some [(x,s)] 
  | PUnt,SUnit -> Some []
  | PVnt(li,pio),SVar x -> 
      begin match TEnv.lookup tev x with
        | None -> sort_error i (fun () -> Util.format "@[%s is not bound@]" (string_of_qid x))
        | Some sl -> 
            let rec aux = function
              | [] -> None
              | (lj,sjo)::rest -> 
                  if (id_equal li lj) then 
                    (match pio,sjo with 
                       | None,None -> Some []
                       | Some pi,Some sj -> static_match i tev pi sj
                       | _ -> sort_error i (fun () -> Util.format "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" (string_of_id li)))
                  else aux rest in 
              aux sl
      end
  | PPar(p1,p2),SProduct(s1,s2) -> 
      (match static_match i tev p1 s1, static_match i tev p2 s2 with 
         | Some l1, Some l2 -> Some (l1 @ l2) 
         | _ -> None)
  | _ -> None 

let rec dynamic_match i p v = match p,v with 
  | PWld,_ -> Some []
  | PVar x,_ -> Some [(x,v)]
  | PUnt,V.Unt(_) -> Some []
  | PVnt(li,pio),V.Vnt(_,_,lj,vjo) -> 
      if (id_equal li lj) then 
        (match pio,vjo with 
           | None,None -> Some []
           | Some pi,Some vj -> dynamic_match i pi vj
           | _ -> run_error i "dynamic match" (fun () -> Util.format "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" (string_of_id li)))
      else None
  | PPar(p1,p2),V.Par(_,v1,v2) -> 
      (match dynamic_match i p1 v1,dynamic_match i p2 v2 with 
         | Some l1,Some l2 -> Some (l1 @ l2)
         | _ -> None)
  | _ -> None 

let expect_sorts i msg expecteds found =
  let rec aux = function 
    | [] -> 
        sort_error i
          (fun () -> 
             Util.format "@[in %s:@ %s@ expected@ but@ %s@ found@]" msg
               (Misc.concat_list " or " (Safelist.map string_of_sort expecteds))
               (string_of_sort found))
    | h::t -> 
        if subsort found h then (h,found)
        else aux t in 
  aux expecteds
      
let rec expect_sorts_exp msg evs expecteds e =
  let i = info_of_exp e in
  let found_sort,new_e = check_exp evs e in 
  let f_sort,e_sort = expect_sorts i msg expecteds found_sort in 
  (f_sort,e_sort,new_e)

and expect_sort i msg expected found = 
  snd (expect_sorts i msg [expected] found)

and expect_sort_exp msg evs expected e = 
  let (_,e_sort,new_e) = expect_sorts_exp msg evs [expected] e in 
  (e_sort,new_e)

(* check expressions *)    
and check_exp ((tev,sev) as evs) e0 = match e0 with
  | EVar(i,q) ->
      begin match SCEnv.lookup sev q with
        | Some s -> (s,e0)
        | None -> 
            sort_error i 
              (fun () -> 
                Util.format "@[%s is not bound@]"
                  (string_of_qid q))
      end

  | EApp(i,e1,e2) -> 
      begin match check_exp evs e1 with
        | SFunction(param_sort,return_sort), new_e1 -> 
            let e2_sort,new_e2 = check_exp evs e2 in 
            let _ = expect_sort i "application" param_sort e2_sort in 
            let new_e0 = EApp(i, new_e1, new_e2) in 
            (return_sort, new_e0)
        | e1_sort,_ -> 
            sort_error i 
              (fun () -> 
                Util.format
                  "@[expected@ arrow@ sort@ in@ left-hand@ side@ of@ application@ but@ found %s@]"
                  (string_of_sort e1_sort))
      end

  | EFun(i,p,ret_sorto,body) ->
      let p_sort = sort_of_param p in
      let p_id = id_of_param p in
      let body_sev = SCEnv.update sev (qid_of_id p_id) p_sort in
      let body_sort,new_body = match ret_sorto with
        | Some s -> expect_sort_exp "function body" (tev,body_sev) s body
        | None   -> check_exp (tev,body_sev) body in        
      let e0_sort = SFunction(p_sort,body_sort) in 
      let new_e0 = EFun(i,p,Some body_sort,new_body) in 
      (e0_sort, new_e0)

  | ELet(i,b,e) ->
      let bevs,_,new_b = check_binding evs b in 
      let e0_sort,new_e = check_exp bevs e in 
      let new_e0 = ELet(i, new_b, new_e) in 
      (e0_sort, new_e0)

  | EUnit(_) -> 
      (SUnit,e0)

  | EPair(i,e1,e2) -> 
      let e1_sort,new_e1 = check_exp evs e1 in 
      let e2_sort,new_e2 = check_exp evs e2 in 
      let e0_sort = SProduct(e1_sort,e2_sort) in 
      let new_e0 = EPair(i,new_e1,new_e2) in 
      (e0_sort,new_e0)

  | ECase(i,e1,pl) -> 
      let err0 p = sort_error i (fun () -> Util.format p) in 
      let err2 p s1 s2 = sort_error i (fun () -> Util.format p s1 s2) in 
      let e1_sort,new_e1 = check_exp evs e1 in 
      let sorto,new_pl_rev = Safelist.fold_left 
        (fun (sorto,new_pl_rev) (pi,ei) -> 
           match static_match i tev pi e1_sort with 
             | None -> 
                 err2 "@[pattern %s and sort %s do not match@]" 
                   (string_of_pat pi) 
                   (string_of_sort e1_sort)
             | Some binds -> 
                 let ei_sev = Safelist.fold_left 
                   (fun ei_sev (xj,sj) -> SCEnv.update ei_sev (qid_of_id xj) sj)
                   sev binds in 
                 let ei_sort,new_ei = check_exp (tev,ei_sev) ei in
                 let sort = match sorto with 
                   | None -> ei_sort 
                   | Some sort -> 
                       (* check that branches have compatible sorts *)
                       try join i ei_sort sort with _ -> 
                         err2 "@[branches have different sorts: %s and %s@]"
                           (string_of_sort sort)
                           (string_of_sort ei_sort) in 
                 (Some sort,(pi,new_ei)::new_pl_rev))
        (None,[]) pl in 
      let e0_sort = match sorto with 
        | None -> err0 "@[empty sum@]"
        | Some s -> s in 
      let new_e0 = ECase(i,new_e1,Safelist.rev new_pl_rev) in 
        (e0_sort,new_e0)

  | EString(_,_) -> 
      (SString,e0)

  | ECSet(_) -> 
      (SRegexp,e0)

  | EUnion(i,e1,e2) ->
      let f1_sort,e1_sort,new_e1 = expect_sorts_exp "union" evs [SLens;SCanonizer] e1 in 
      let e2_sort,new_e2 = expect_sort_exp "union" evs f1_sort e2 in 
      let e0_sort = match join i e1_sort e2_sort with
        | SString -> SRegexp
        | s -> s in 
      let new_e0 = EUnion(i,new_e1,new_e2) in 
      (e0_sort,new_e0)

  | ECat(i,e1,e2) -> 
      let f1_sort,e1_sort,new_e1 = expect_sorts_exp "concat" evs [SLens;SCanonizer] e1 in 
      let e2_sort,new_e2 = expect_sort_exp "concat" evs f1_sort e2 in 
      let e0_sort = join i e1_sort e2_sort in 
      let new_e0 = ECat(i,new_e1,new_e2) in 
      (e0_sort,new_e0)

  | ETrans(i,e1,e2) -> 
      let e1_sort,new_e1 = expect_sort_exp "trans" evs SLens e1 in 
      let e2_sort,new_e2 = expect_sort_exp "trans" evs SString e2 in 
      let new_e0 = ETrans(i,new_e1,new_e2) in 
      (SLens,new_e0)

  | EStar(i,e) -> 
      let _,e_sort,new_e = expect_sorts_exp "kleene-star" evs [SLens;SCanonizer] e in 
      let new_e0 = EStar(i,new_e) in 
      (e_sort, new_e0) 

  | EDiff(i,e1,e2) -> 
      let e1_sort,new_e1 = expect_sort_exp "diff" evs SRegexp e1 in 
      let e2_sort,new_e2 = expect_sort_exp "diff" evs SRegexp e2 in       
      let new_e0 = EDiff(i,new_e1,new_e2) in 
      (SRegexp,new_e0)

  | EInter(i,e1,e2) -> 
      let e1_sort,new_e1 = expect_sort_exp "diff" evs SLens e1 in 
      let e2_sort,new_e2 = expect_sort_exp "diff" evs SLens e2 in       
      let new_e0 = EInter(i,new_e1,new_e2) in 
      (SRegexp,new_e0)

  | ECompose(i,e1,e2) -> 
      let e1_sort,new_e1 = expect_sort_exp "compose" evs SLens e1 in 
      let e2_sort,new_e2 = expect_sort_exp "compose" evs SLens e2 in       
      let new_e0 = ECompose(i,new_e1,new_e2) in 
      (SLens,new_e0)

  | EMatch(i,_,_) -> 
      (* fixme: check that this match is used consistently! *)
      (SLens,e0)
        
and check_binding ((tev,sev) as evs) = function
  | Bind(i,p,so,e) -> 
      let e_sort,new_e = match so with 
        | None -> check_exp evs e 
        | Some s -> expect_sort_exp "let" evs s e in 
      let bindso = static_match i tev p e_sort in 
      let bsev,xs_rev = match bindso with 
        | None -> sort_error i 
            (fun () -> Util.format "@[pattern %s and sort %s do not match@]" 
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
              None -> run_error i "check_decl"
                (fun () -> 
                  Util.format "@[declaration for %s missing@]"
                    (string_of_qid q))
            | Some q_rv ->
                let nq = splice_id_dot n q in
                (SCEnv.update n_sev nq (s_of_rv q_rv), nq::names))
        (sev,[])
        names in 
      let new_d = DMod(i,n,new_ds) in 
      ((m_tev,n_sev),Safelist.rev names_rev,new_d)

  | DType(i,x,sl) as d -> 
      let qx = qid_of_id x in 
      let sx = SVar qx in
      let new_sev = Safelist.fold_left 
        (fun sev (l,so) -> 
           let ql = qid_of_id l in 
           let s = match so with 
             | None -> sx
             | Some s -> SFunction (s,sx) in
             SCEnv.update sev ql s)
        sev sl in 
      let new_tev = TEnv.update tev qx sl in 
      ((new_tev,new_sev),[],d)
      
  | DTest(i,e1,tr) -> 
      let e1_sort,new_e1 = check_exp evs e1 in
      let new_tr = match tr with 
        | TestError -> tr
        | TestShow -> tr
        | TestValue e2 -> 
            let _,new_e2 = expect_sort_exp "test result" evs e1_sort e2 in 
              TestValue (new_e2) 
        | TestSort _ -> tr
        | TestLensType(e21o,e22o) -> 
            let chk_eo = function
              | None -> None
              | Some e -> 
                  let _,new_e = expect_sort_exp "test type" evs SRegexp e in 
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
(* helper to check assertions *)
let do_assert i msg rx s = 
  if (not (Prefs.read no_assert)) then
    match Erx.match_str rx s with
      | true -> ()
      | false -> run_error i "do_assert"
          (fun () -> 
            Util.format "%s: %s does not belong to"
              (Info.string_of_t i)
              (RS.string_of_t s);
            Erx.format rx)

(* helper to manage coercions for binary operations *)
let do_binop i msg merge (s1,v1) (s2,v2) = 
  let rec aux = function
    | [] -> run_error i "do_binop" 
        (fun () -> Util.format "no case for %s and %s in %s"
          (string_of_sort s1)
          (string_of_sort s2)
          msg)
    | (s1',s2',s,f)::rest -> 
        if subsort s1 s1' && subsort s2 s2' then (s,f i v1 v2)
        else aux rest in 
  aux merge
            
(* expressions *)
let rec compile_exp cev e0 = match e0 with
  | EVar(i,q) -> 
      begin match CEnv.lookup cev q with
        | Some(SRegexp,v) -> 
            let x = (string_of_qid q) in 
            let r' = Bregexp.set_str (Bvalue.get_r v i) x in
            let rv' = SRegexp, Bvalue.Rx(i,r') in 
            rv'
        | Some rv -> rv
        | None -> 
            run_error (info_of_exp e0) "compile_exp"
                (fun () -> 
                  Util.format "@[%s is not bound@]"
                    (string_of_qid q))
      end

  | EApp(i,e1,e2) ->
      let s1,v1 = compile_exp cev e1 in
      let s2,v2 = compile_exp cev e2 in
      begin match s1,v1 with
        | SFunction(_,ret_sort),Bvalue.Fun(_,_,_,f) -> 
            (ret_sort, (f i v2))
        | _   -> 
            run_error i "compile_exp"
              (fun () -> 
                Util.format
                  "@[expected function in left-hand side of application but found %s"
                  (string_of_sort s2))
      end

  | ELet(i,b,e) ->
      let bcev,_ = compile_binding cev b in       
      compile_exp bcev e
        
  | EFun(i,p,Some ret_sort,e) ->
      let p_sort = sort_of_param p in
      let f_sort = SFunction(p_sort,ret_sort) in
      let f_impl i v =
        let p_qid = qid_of_id (id_of_param p) in 
        let p_sort = sort_of_param p in
        let body_cev = CEnv.update cev p_qid (p_sort, v) in
          snd (compile_exp body_cev e) in 
      (f_sort, (Bvalue.Fun (i,p_sort,ret_sort,f_impl)))

  | EFun(i,_,_,_) -> 
      run_error i "compile_exp"
        (fun () -> 
          Util.format 
            "@[compiler bug: function has no sort!@]")

  | EUnit i -> (SUnit,Bvalue.Unt i)

  | EPair(i,e1,e2) -> 
      let s1,v1 = compile_exp cev e1 in 
      let s2,v2 = compile_exp cev e2 in 
      (SProduct(s1,s2),Bvalue.Par(i,v1,v2))

  | ECase(i,e1,pl) -> 
      let s1,v1 = compile_exp cev e1 in 
      let rec find_match = function
        | [] -> run_error i "compile_exp" (fun () -> Util.format "@[match@ failure@]")
        | (pi,ei)::rest -> 
            (match dynamic_match i pi v1 with 
               | None -> find_match rest
               | Some l -> l,ei) in 
      let binds,ei = find_match pl in 
      let ei_cev = Safelist.fold_left 
        (fun ei_cev (x,v) -> 
           CEnv.update ei_cev (qid_of_id x) (Bvalue.sort_of_t v,v))
        cev binds in 
      compile_exp ei_cev ei

  | EString(i,s) -> (SString,Bvalue.Str(i,s))

  | ECSet(i,true,cs) -> (SRegexp,Bvalue.Rx (i, R.set cs))

  | ECSet(i,false,cs) -> (SRegexp,Bvalue.Rx (i, R.negset cs))

  | ECat(i,e1,e2) ->
      let concat_merge = 
        [ (SString,SString,SString,
          (fun i v1 v2 -> 
            V.Str(i,RS.append (V.get_s v1 i) (V.get_s v2 i))))
        ; (SRegexp,SRegexp,SRegexp,
          (fun i v1 v2 -> 
            V.Rx(i,R.seq (V.get_r v1 i) (V.get_r v2 i))))
        ; (SLens,SLens,SLens,
          (fun i v1 v2 -> 
            V.Lns(i,L.concat i (V.get_l v1 i) (V.get_l v2 i)))) 
        ; (SCanonizer,SCanonizer,SCanonizer,
          (fun i v1 v2 -> 
            V.Can(i,C.concat i (V.get_c v1 i) (V.get_c v2 i)))) ] in 
      do_binop i "concat" concat_merge 
        (compile_exp cev e1)
        (compile_exp cev e2)

  | ETrans(i,e1,e2) -> 
      let _,v1 = compile_exp cev e1 in 
      let _,v2 = compile_exp cev e2 in 
      let set_name = "Prelude.set" in 
      let set_qid = V.parse_qid set_name in 
      let set_f = match CEnv.lookup cev set_qid with
        | Some (_,set_v) -> 
            V.get_f set_v i
        | None -> 
            run_error (info_of_exp e0) "compile_exp" 
              (fun () -> Util.format "@[%s is not bound@]" set_name) in 
      let set_f2 = V.get_f (set_f i v1) i in 
      let set_l3 = V.get_l (set_f2 i v2) i in 
      (SLens,V.Lns(i,set_l3))

  | EUnion(i,e1,e2) -> 
      let union_merge = 
        [ (SRegexp,SRegexp,SRegexp,
          (fun i v1 v2 -> 
            V.Rx(i,R.alt (V.get_r v1 i) (V.get_r v2 i))))            
        ; (SLens,SLens,SLens,
          (fun i v1 v2 -> 
            V.Lns(i,L.union i (V.get_l v1 i) (V.get_l v2 i))))
        ; (SCanonizer,SCanonizer,SCanonizer,
          (fun i v1 v2 -> 
            V.Can(i,C.union i (V.get_c v1 i) (V.get_c v2 i)))) ] in 
      let rec flatten_unions = function
        | EUnion(i,e1,e2) -> (flatten_unions e1) @ (flatten_unions e2)
        | e -> [e] in
      let rec split n l acc = match n,l with
        | 0,_ | _,[] -> (Safelist.rev acc,l)
        | _,h::t -> split (pred n) t (h::acc) in 
      let rec aux = function
        | [] -> assert false
        | [e1] -> compile_exp cev e1
        | l -> 
            let mid = (succ (List.length l)) / 2 in 
            let l1,l2 = split mid l [] in 
              do_binop i "union" union_merge
                (aux l1)
                (aux l2) in 
      aux (flatten_unions e0)

  | EStar(i,e1) -> 
      let s1,v1 = compile_exp cev e1 in 
      begin match s1 with
        | SString | SRegexp -> 
            (SRegexp,V.Rx(i,R.star (V.get_r v1 i)))
        | SLens -> 
            (SLens,V.Lns(i,L.star i (V.get_l v1 i)))
        | SCanonizer -> 
            (SCanonizer,V.Can(i,C.star i (V.get_c v1 i)))
        | _ -> 
            ignore (expect_sort i "kleene-star" SLens s1);
            assert false
      end

  | EMatch(i,t,q) -> 
      begin match CEnv.lookup cev q with
        | Some (_,v) -> 
            let l = Bvalue.get_l v i in 
            (SLens,V.Lns(i,L.smatch i t l))
        | None -> 
            run_error (info_of_exp e0) "compile_exp" 
              (fun () -> 
                Util.format "@[%s is not bound@]"
                  (string_of_qid q))
      end
      
  | ECompose(i,e1,e2) -> 
      let compose_merge = 
        [ (SLens, SLens,SLens,
          (fun i v1 v2 -> 
            V.Lns(i,L.compose i (V.get_l v1 i) (V.get_l v2 i)))) ] in 
    do_binop i "compose" compose_merge
      (compile_exp cev e1)
      (compile_exp cev e2)

  | EDiff(i,e1,e2) -> 
      let diff_merge = 
        [ (SRegexp, SRegexp, SRegexp, 
          (fun i v1 v2 -> 
            V.Rx(i,R.diff (V.get_r v1 i) (V.get_r v2 i)))) ] in 
      do_binop i "diff" diff_merge
        (compile_exp cev e1)
        (compile_exp cev e2)

  | EInter(i,e1,e2) -> 
      let inter_merge = 
        [ (SRegexp, SRegexp, SRegexp, 
          (fun i v1 v2 -> 
            V.Rx(i,R.inter (V.get_r v1 i) (V.get_r v2 i)))) ] in 
      do_binop i "inter" inter_merge
        (compile_exp cev e1)
        (compile_exp cev e2)

      
and compile_binding cev = function
  | Bind(i,p,so,e) -> 
      let s,v = compile_exp cev e in 
      let bindso = dynamic_match i p v in 
      let bcev,xs_rev = match bindso with 
        | None -> run_error i "compile binding" 
            (fun () -> Util.format "@[pattern %s and value %s do not match@]" 
               (string_of_pat p) 
               (V.string_of_t v))
        | Some binds -> 
            Safelist.fold_left 
              (fun (bcev,xs) (x,v) -> 
                 let qx = qid_of_id x in 
                 (CEnv.update bcev qx (V.sort_of_t v,v), qx::xs))
              (cev,[]) binds in 
      (bcev,Safelist.rev xs_rev)

let rec compile_decl cev ms = function
  | DLet(i,b) -> 
      let bcev,xs = compile_binding cev b in
      (bcev,xs)
  | DType(i,x,sl) -> 
      let qx = qid_of_id x in 
      let sx = SVar qx in 
      let new_cev = Safelist.fold_left 
        (fun cev (l,so) -> 
           let ql = qid_of_id l in 
           let rv = match so with 
             | None -> (sx,V.Vnt(i,qx,l,None))
             | Some s -> 
                 let sf = SFunction(s,sx) in 
                   (sf,V.Fun(i,s,sx,(fun i v -> V.Vnt(i,qx,l,Some v)))) in 
             CEnv.update cev ql rv)
        cev sl in 
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
                  run_error i "compile_decl" 
                    (fun () -> Util.format "@[compiled declaration for %s missing@]"
                      (string_of_qid q)))
          (cev,[])
          names in
        (n_cev, Safelist.rev names_rev)
  | DTest(i,e,tr) ->
      if check_test ms then 
        begin
          let vo = 
            try let s,v = compile_exp cev e in 
            OK(s,v)
            with (Error.Harmony_error(err)) -> Error err in 
          match vo,tr with 
            | OK (_,v), TestShow ->
                Util.format "Test result:@ "; 
                Bvalue.format v; 
                Util.format "@\n%!"
            | OK (s0,v), TestSort(Some s) -> 
                if not (subsort s0 s) then
                  test_error i
                    (fun () -> 
                       Util.format "@\nExpected@ "; format_sort s;
                       Util.format "@ but found@ "; format_sort s0; 
                       Util.format "@\n%!")
            | OK(s0,v), TestSort None -> 
                Util.format "Test sort:@ %s@\n%!" (string_of_sort s0);
            | OK(_,v), TestLensType(e1o,e2o) -> 
                if not (subsort (Bvalue.sort_of_t v) SLens) then 
                  test_error i 
                    (fun () -> 
                       Util.format "@\nExpected@ "; format_sort SLens;
                       Util.format "@ but found@ "; Bvalue.format v;
                       Util.format "@\n%!");
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
                        Util.format "Test type:@ ";
                        Util.format "@[<2>%s <-> %s@]" (R.string_of_t c) (R.string_of_t a);
                        Util.format "@\n%!"
                      end)
                  else
                    begin
                  test_error i 
                    (fun () -> 
                       Util.format "@\nExpected@ "; 
                       Util.format "@[<2>%s <-> %s@]" c_str a_str;
                       Util.format "@ but found@ "; 
                       Util.format "@[<2>%s <-> %s@]" (R.string_of_t c) (R.string_of_t a);
                       Util.format "@\n%!");
                    end                      
            | Error err, TestShow 
            | Error err, TestSort _ 
            | Error err, TestLensType _ -> 
                test_error i 
                  (fun () -> 
                    Util.format "Test result: error";
                    err (); 
                    Util.format "%!")
            | Error _, TestError -> ()
            | OK(_,v), TestValue res -> 
                let resv = snd (compile_exp cev res) in
                  if not (Bvalue.equal v resv) then
                    test_error i 
                      (fun () ->
                        Util.format "@\nExpected@ "; Bvalue.format resv;
                        Util.format "@ but found@ "; Bvalue.format v; 
                        Util.format "@\n%!")
            | Error err, TestValue res -> 
                let resv = snd (compile_exp cev res) in
                  test_error i 
                    (fun () ->
                      Util.format "@\nExpected@ "; Bvalue.format resv; 
                      Util.format "@ but found an error:@ "; 
                      err (); 
                      Util.format "@\n%!")
            | OK(_,v), TestError -> 
                test_error i 
                  (fun () ->
                    Util.format "@\nExpected an error@ "; 
                    Util.format "@ but found:@ "; 
                    Bvalue.format v; 
                    Util.format "@\n%!")
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
