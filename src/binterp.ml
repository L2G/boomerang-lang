(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /src/binterp.ml                                                            *)
(* Boomerang interpreter *)
(* $Id$ *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)
(* IMPORTS AND ABBREVIATIONS *)

open Bsyntax
open Bident
open Benv
open Bprint
open Bsubst
module V = Bvalue
module G = Bregistry

(* -------------------------------------------------------------------------- *)
(* ERRORS / DEBUGGING *)

let msg = Util.format

let test_error i msg_thk = 
  raise (Error.Harmony_error
           (fun () -> msg "@[%s: Unit test failed @ " (Info.string_of_t i); 
              msg_thk ();
              msg "@]"))

(* -------------------------------------------------------------------------- *)
(* UNIT TESTS *)

(* test results: success with a value, or failure with an error message *)
type testresult = OK of Bvalue.t | Error of (unit -> unit)

let tests = Prefs.createStringList
  "test"
  "run unit test for the specified module"
  "run unit tests for the specified module"
let _ = Prefs.alias tests "t"

let test_all = Prefs.createBool "test-all" false
  "run unit tests for all modules"
  "run unit tests for all modules"

(* [check_test m] returns [true] iff the command line arguments
   '-test-all' or '-test m' are set *)
let check_test ms = 
  Safelist.fold_left 
    (fun r qs -> r or (Qid.id_prefix (G.parse_qid qs) ms))
    (Prefs.read test_all)
    (Prefs.read tests)

(* -------------------------------------------------------------------------- *)
(* INTERPRETER *)

(* dynamic match: determine if a value *does* match a pattern; return
   the list of value bindings for variables. *)
let rec dynamic_match i p0 v0 = match p0,v0 with   
  | PWld _,_                   -> Some []
  | PVar(_,q,so),_             -> Some [(q,v0,so)]
  | PUnt _,V.Unt(_)            -> Some []
  | PInt(_,n1),V.Int(_,n2)     -> if n1=n2 then Some [] else None
  | PBol(_,b1),V.Bol(_,b2)     -> 
      if (b1 && (b2 = None)) || (not b1 && (b2 <> None)) then Some [] 
      else None
  | PCex(_,p),V.Bol(vi,Some s) -> dynamic_match i p (V.mk_s vi s)
  | PStr(_,s1),V.Str(_,s2)     -> if s1 = s2 then Some [] else None
  | PVnt(_,(_,li),pio),V.Vnt(_,_,lj,vjo) -> 
      if Id.equal li lj then 
        match pio,vjo with 
          | None,None -> Some []
          | Some pi,Some vj -> dynamic_match i pi vj
          | _ -> 
              Berror.run_error i 
                (fun () -> 
                   msg "@[wrong@ number@ of@ arguments@ to@ constructor@ %s@]" 
                     (Id.string_of_t li))
      else None
  | PPar(_,p1,p2),V.Par(_,v1,v2) -> 
      (match dynamic_match i p1 v1,dynamic_match i p2 v2 with 
         | Some l1,Some l2 -> Some (l1 @ l2)
         | _ -> None)
  | _ -> None 

(* ----- interpret casts ----- *)
(* precondition: f and t must be compatible. *)
let rec interp_cast cev i b f t e = 
  (* generates cast of e0 into the refinement (x:t where e) *)
  let cast_refinement f x t e e0 = 
    let v = interp_exp cev (ECast(i,f,t,b,e0)) in
    let cev' = CEnv.update cev (Qid.t_of_id x) (G.Unknown,v) in
    match V.get_x (interp_exp cev' e) with
      | None -> v
      | Some(cex) ->
	  let cex_s = if cex = "" then "" else "; counterexample: " ^ cex in
	  Berror.blame_error (info_of_blame b)
	    (fun () ->
	       (* TODO show bindings of free vars in e *)
	       Util.format "@[%s=%a@ did@ not@ satisfy@ %a%s@]"
		 (Id.string_of_t x)
		 (fun _ -> V.format) v
		 (fun _ -> format_exp) e
	         cex_s) in
  let res = 
    if Bcheck.trivial_cast f t then interp_exp cev e
    else
      match f,t with
      | SUnit,SUnit
      | SBool,SBool
      | SInteger,SInteger
      | SChar,SChar
      | SString,SString
      | SRegexp,SRegexp
      | SAregexp,SAregexp
      | SLens,SLens 
      | SCanonizer,SCanonizer 
      | SVar(_),SVar(_) -> 
          interp_exp cev e
      | SChar,SString -> 
          interp_exp cev (mk_string_of_char i e)
      | SChar,SRegexp -> 
          interp_exp cev (mk_regexp_of_string i (mk_string_of_char i e))
      | SChar,SAregexp -> 
          interp_exp cev (mk_aregexp_of_regexp i (mk_regexp_of_string i (mk_string_of_char i e)))
      | SChar,SLens -> 
          interp_exp cev
            (mk_lens_of_regexp i
               (mk_regexp_of_string i 
                  (mk_string_of_char i e)))
      | SString,SRegexp -> 
          interp_exp cev (mk_regexp_of_string i e)
      | SString,SAregexp -> 
          interp_exp cev (mk_aregexp_of_regexp i (mk_regexp_of_string i e))
      | SString,SLens -> 
          interp_exp cev (mk_lens_of_regexp i (mk_regexp_of_string i e))
      | SRegexp,SAregexp -> 
          interp_exp cev (mk_aregexp_of_regexp i e)
      | SRegexp,SLens -> 
          interp_exp cev (mk_lens_of_regexp i e)
      | SFunction(x,f1,f2), SFunction(y,t1,t2) -> 
          let fn = Id.mk i "fn" in 
          let qx = Qid.t_of_id x in 
	  let qy = Qid.t_of_id y in
          let qfn = Qid.t_of_id fn in 
          let e_x = EVar(i,qx) in
	  let e_y = EVar(i,qy) in
          let e_fn = EVar(i,qfn) in
          let e_fx = EApp(i,e_fn,e_x) in
          let c1 = ECast(i,t1,f1,invert_blame b,e_y) in 
          let c2 = ECast(i,f2,t2,b,e_fx) in
          let apped_cast = EApp(i,mk_fun i fn f 
                                  (mk_fun i y t1 
                                     (mk_let i x f1 c1 c2)),e) in
	  interp_exp cev apped_cast
      | SProduct(f1,f2), SProduct(t1,t2) ->
	  if syneq_sort f1 t1 && syneq_sort f2 t2
	  then interp_exp cev e
	  else
            let x = Id.mk i "x" in 
            let y = Id.mk i "y" in 
            let e_x = EVar(i,(Qid.t_of_id x)) in 
            let e_y = EVar(i,(Qid.t_of_id y)) in 
            let c1 = ECast(i,f1,t1,b,e_x) in
            let c2 = ECast(i,f2,t2,b,e_y) in
	    let px = PVar(i,x,Some f1) in
	    let py = PVar(i,y,Some f2) in
	    let cast_pair = ECase(i,e,[(PPar(i,px,py),EPair(i,c1,c2))],t) in
	    interp_exp cev cast_pair
      | SData(fl,x),SData(tl,y) when Qid.equal x y -> 
          let rec aux acc l1 l2 = acc && match l1,l2 with
            | [],[] -> true
            | h1::t1,h2::t2 -> aux (syneq_sort h1 h2) t1 t2 
            | _ -> false in 
          if aux true fl tl then interp_exp cev e
          else 
            let _,(svl,cl) = Bcheck.get_type (CEnv.lookup_type cev) i x in
            let fsubst = Safelist.combine svl fl in 
            let tsubst = Safelist.combine svl tl in 
            let cl_finst = Bcheck.inst_cases fsubst cl in 
            let cl_tinst = Bcheck.inst_cases tsubst cl in
            let x = Id.mk i "x" in 
            let qx = Qid.t_of_id x in 
            let y = Id.mk i "y" in 
            let qy = Qid.t_of_id y in 
            let pl = Safelist.map
              (fun ((li,fio),(_,tio)) -> 
                 match fio,tio with 
                   | None,None -> 
                       let pi = PVnt(i,li,None) in 
                       let ei = EVar(i,qx) in 
                         (pi,ei)
                   | Some fi,Some ti -> 
                       let li_f = 
                         Safelist.fold_right 
                           (fun tj acc -> ETyApp(i,acc,tj))
                           tl (EVar(i,li)) in 
                       let py = PVar(i,y,Some fi) in 
                       let pi = PVnt(i,li,Some py) in 
                         (* this cast must not be expanded! 
                            (it would loop on recursive data types) *)
                       let ei = EApp(i,li_f,ECast(i,fi,ti,b,EVar(i,qy))) in
                       (pi,ei)
                   | _ -> 
                       Berror.run_error i 
                         (fun () -> 
                            msg "@[cannot@ cast@ different@ datatypes@]"))
              (Safelist.combine cl_finst cl_tinst) in 
            interp_exp cev 
              (* !!! OPT *)
              (mk_let i x f e (ECase(i,EVar(i,qx),pl,t))) 
      | SRefine(x,t1,e1),SRefine(y,t2,e2) -> 
          if Id.equal x y && syneq_sort t1 t2 && syneq_exp e1 e2 
          then interp_exp cev e
          else cast_refinement f y t2 e2 e
      | _,SRefine(x,t2,e2) -> 
          cast_refinement f x t2 e2 e
      | SRefine(x,f1,e1),t1 -> 
          interp_cast cev i b f1 t1 e
      | SForall(x,f1),SForall(y,t1) ->
	  (* no need to freshen since compatibility substitutes *)
          let e_ex = ETyApp(i,e,SVar x) in
	  interp_exp cev (ETyFun(i,x,ECast(i,f1,t1,b,e_ex)))
      | _ -> 
          Berror.run_error i 
            (fun () -> 
               msg "@[cannot@ cast@ incompatible@ %s@ to@ %s@]"
                 (string_of_sort f) (string_of_sort t)) in 
  res

(* ----- interpret expressions ----- *)

and interp_exp cev e0 = match e0 with 
  | EVar(i,q) ->       
      begin 
        match CEnv.lookup cev q with
          | Some(_,v) -> v
          | None -> 
              Berror.run_error i 
                (fun () -> msg "@[%s unbound@]" 
                   (Qid.string_of_t q))
      end

  | EOver(i,op,_) -> 
      Berror.run_error i
        (fun () -> 
           msg "@[unresolved@ overloaded@ operator %s@]" 
             (string_of_op op))

  | EApp(_,e1,e2) ->
      let v1 = interp_exp cev e1 in 
      let v2 = interp_exp cev e2 in 
      (V.get_f v1) v2

  | ELet(_,b,e) -> 
      let bcev,_ = interp_binding cev b in
      interp_exp bcev e
          
  | EFun(i,p,_,e) ->
      let f v =        
        let qp = Qid.t_of_id (id_of_param p) in 
        let body_cev = CEnv.update cev qp (G.Unknown,v) in 
        interp_exp body_cev e in 
      V.Fun(i,f)

  | EPair(i,e1,e2) -> 
      let v1 = interp_exp cev e1 in 
      let v2 = interp_exp cev e2 in 
      V.Par(i,v1,v2)

  | ECase(i,e1,pl,_) -> 
      let v1 = interp_exp cev e1 in 
      let rec find_match = function
        | [] -> 
            Berror.run_error i
              (fun () -> 
                 msg "@[match@ failure@]")
        | (pi,ei)::rest -> 
            (match dynamic_match i pi v1 with 
               | None -> find_match rest
               | Some l -> l,ei) in 
      let binds,ei = find_match pl in 
      let qid_binds = Safelist.map 
        (fun (x,v,so) -> 
           let rs = match so with 
             | None -> G.Unknown 
             | Some s -> G.Sort s in 
           (Qid.t_of_id x,(rs,v))) binds in         
      let ei_cev = CEnv.update_list cev qid_binds in
      interp_exp ei_cev ei

  (* tyfuns are interpreted as functions; tyapps apply to unit *)
  | ETyFun(i,_,e) -> 
      interp_exp cev (EFun(i,Param(i,Id.wild,SUnit),None,e))

  | ETyApp(i,e1,s2) -> 
      interp_exp cev (EApp(i,e1,EUnit(i)))

  | EGrammar(i,ps) ->
     Berror.run_error i (fun () -> msg "@[unexpected@ grammar@ expression@ at@ interpreter@]")

  (* constants *)
  | EUnit(i)            -> V.Unt(i)
  | EBoolean(i,None)    -> V.Bol(i,None)
  | EBoolean(i,Some e1) -> V.Bol(i,Some (V.get_s (interp_exp cev e1)))
  | EInteger(i,n)       -> V.Int(i,n)
  | EChar(i,c)          -> V.Chr(i,c)
  | EString(i,s)        -> V.Str(i,s)
  | ECSet(i,pos,cs) -> 
      let csi = Safelist.map (fun (ci,cj) -> (Char.code ci, Char.code cj)) cs in
      let mk = if pos then Brx.mk_cset else Brx.mk_neg_ascii_cset in
      V.Rx(i,mk csi)

  | ECast(i,f,t,b,e) -> 
      interp_cast cev i b f t e

and interp_binding cev b0 = match b0 with
  | Bind(i,p,so,e) -> 
      let v = interp_exp cev e in 
      let xs_rev,bcev = match dynamic_match i p v with
        | None -> Berror.run_error i 
            (fun () -> msg "@[in let-binding: %s does not match %s@]"
               (string_of_pat p) (V.string_of_t v))
        | Some binds -> 
            Safelist.fold_left 
              (fun (xsi,cevi) (xi,vi,soi) -> 
                 let qxi = Qid.t_of_id xi in 
                 let rsi = match soi with 
                   | None -> G.Unknown 
                   | Some s -> G.Sort s in  
                 (qxi::xsi,CEnv.update cevi (Qid.t_of_id xi) (rsi,vi)))
              ([],cev) binds in 
      (bcev,Safelist.rev xs_rev)
      
let rec interp_decl cev ms d0 = match d0 with
  | DLet(i,b) ->
      interp_binding cev b
  | DType(i,svl,qx,cl) -> 
      let sx = SData(sl_of_svl svl,qx) in 
      let mk_univ s = 
        Safelist.fold_right 
          (fun svi acc -> SForall(svi,acc)) 
          svl s in 
      let mk_impl v = 
        Safelist.fold_right 
          (fun _ f -> V.Fun(i,(fun v -> f))) 
          svl v in 
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
      let m_cev, names = interp_mod_aux cev ms ds in
      let n_cev,names_rev = 
        Safelist.fold_left
          (fun (n_cev, names) q ->
             match CEnv.lookup m_cev q with
               | Some rv ->
                   let nq = Qid.splice_id_dot n q in
                   (CEnv.update n_cev nq rv, nq::names)
               | None -> 
                   Berror.run_error i 
                     (fun () -> msg "@[@ declaration@ for@ %s@ missing@]"
                        (Qid.string_of_t q)))
          (cev,[])
          names in
        (n_cev, Safelist.rev names_rev)

  | DTest(i,e,tr) ->
      begin 
        if check_test ms then 
          let vo = 
            try OK(interp_exp cev e)
            with (Error.Harmony_error(err)) -> Error err in 
          match vo,tr with 
            | OK (v),TestPrint ->
                msg "Test result:@ "; 
                V.format v; 
                msg "@\n%!"
            | OK (v),TestSortPrint(Some s) -> 
                msg "Test result:@ "; 
                format_sort s;
                msg "@\n%!"
            | OK (v),TestSortPrint(None) 
            | OK (v),TestSortEqual(None,_) -> 
                Berror.run_error i 
                  (fun () -> msg "@[unannotated@ unit@ test@]")
            | Error err, TestPrint 
            | Error err, TestSortPrint _ -> 
                test_error i 
                  (fun () -> 
                     msg "Test result: error@\n";
                     err (); 
                     msg "@\n%!")
            | Error _, TestError -> ()
            | OK v1, TestEqual e2 -> 
                let v2 = interp_exp cev e2 in
                  if not (V.equal v1 v2) then
                    test_error i 
                      (fun () ->
                         msg "@\nExpected@ "; V.format v2;
                         msg "@ but found@ "; V.format v1; 
                         msg "@\n%!")
            | OK v1, TestSortEqual(Some s1,s2) ->
                let err () = 
                  test_error i 
                    (fun () ->
                       msg "@\nExpected@ a@ value@ with@ sort@ "; 
                       format_sort s1;
                       msg "@ but found@ "; V.format v1;
                       msg "@ : "; format_sort s2;
                       msg "@\n%!") in 
                  if not (Bcheck.compatible s1 s2) then err ()
                  else 
                    begin
                      try ignore (interp_cast cev i (mk_blame i) s1 s2 e)
                      with Error.Harmony_error(e) -> err ()
                    end                      
            | Error err, TestEqual e2 -> 
                let v2 = interp_exp cev e2 in 
                  test_error i 
                    (fun () ->
                       msg "@\nExpected@ "; V.format v2; 
                       msg "@ but found an error:@ "; 
                       err (); 
                       msg "@\n%!")
            | Error err, TestSortEqual(_,s2) -> 
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
        
and interp_mod_aux cev ms ds = 
  Safelist.fold_left
    (fun (cev, names) di ->
      let m_cev, new_names = interp_decl cev ms di in
        (m_cev, names@new_names))
    (cev,[])
    ds

let interp_module m0 = match m0 with 
  | Mod(i,m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let nctx' = nctx in
      let cev = CEnv.set_ctx (CEnv.empty qm) (qm::nctx') in
      let new_cev,_ = interp_mod_aux cev [m] ds in
      G.register_env (CEnv.get_ev new_cev) nctx' m
