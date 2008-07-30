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
(* /boomerang/src/binterp.ml                                                   *)
(* Boomerang interpreter *)
(* $Id$ *)
(*******************************************************************************)

open Bsyntax
open Bident
open Benv
open Bprint
open Bsubst
module V = Bvalue
module G = Bregistry
module H = Bheap

(* --------------------------------------------------------------------------- *)
(* ERRORS / DEBUGGING *)

let msg = Util.format

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

(* --------------------------------------------------------------------------- *)
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

(* --------------------------------------------------------------------------- *)
(* CAST COMPILATION *)

(* !!! some of these helpers are duplicated work from bcompiler *)

let get_type lookup_type i qx = 
  match lookup_type qx with
    | None -> run_error i 
        (fun () -> msg "@[Unbound@ type@ %s@ --@ at@ runtime!@]" (Qid.string_of_t qx))
    | Some r -> r

(* helper: substitute for variables in data type constructors *)
let inst_cases subst cl = 
  Safelist.map 
    (fun (li,so) -> (li,Misc.map_option (subst_sort subst) so)) cl

let mk_app3 i e1 e2 e3 = 
  EApp(i,EApp(i,e1,e2),e3)

let mk_let i x s1 e1 e2 =
  let b = Bind(i,PVar(i,x,Some s1),None,e1) in 
  ELet(i,b,e2)

let mk_fun i x s e1 =
  let p = Param(i,x,s) in  
  EFun(i,p,None,e1)

let mk_if i e0 e1 e2 s =
  let bs = [(PBol(i,true),e1);(PBol(i,false),e2)] in 
  ECase(i,e0,bs,s)

let mk_native_prelude_var i s = 
  EVar(i,Qid.mk_native_prelude_t s)
let mk_bool_of_cex i e =
  EApp(i,EVar(i,Qid.mk_core_t "bool_of_cex"),e)
let mk_cex_of_bool i e =
  EApp(i,EVar(i,Qid.mk_core_t "cex_of_bool"),e)
let mk_string_of_char i e = 
  EApp(i,mk_native_prelude_var i "string_of_char",e)
let mk_regexp_of_string i e = 
  EApp(i,mk_native_prelude_var i "str",e)
let mk_lens_of_regexp i e = 
  EApp(i,mk_native_prelude_var i "copy",e)

(* --------------------------------------------------------------------------- *)
(* INTERPRETER *)

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
  | PStr(_,s1),V.Str(_,s2) -> if s1 = s2 then Some [] else None
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

(* precondition: f and t must be compatible. *)
let rec interp_cast cev i b f t e = 
  (* generates cast of e0 into the refinement (x:t where e) *)
  let cast_refinement f x t e e0 = 
    let v = interp_exp cev (ECast(i,f,t,b,e0)) in
    let cev' = CEnv.update cev (Qid.t_of_id x) (G.Unknown,v) in
    match V.get_x (interp_exp cev' e) with
      | None -> v
      | Some(cex) ->
	  let cex_s = if cex = "" then "" else "; counterexample: "^cex in
	  let Blame(b_info) = b in
	    Berror.blame_error b_info
	      (fun () ->
		 (* TODO show bindings of free vars in e *)
		 Util.format "@[%s=%a@ did@ not@ satisfy@ %a@ at@ %s%s]"
		   (Id.string_of_t x)
		   (fun _ -> V.format) v
		   (fun _ -> format_exp) e
		   (Info.string_of_t b_info)
	           cex_s) in
  let res = 
    if Prefs.read Bcheck.ignore_refinements && (not (Bcheck.may_coerce f t))
    then interp_exp cev e
    else if Bcheck.trivial_cast f t then interp_exp cev e
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
      | SVar(_),SVar(_) -> interp_exp cev e
      | SBool,SData([],q) when q = V.cex_qid -> interp_exp cev (mk_cex_of_bool i e)
      | SData([],q),SBool when q = V.cex_qid -> interp_exp cev (mk_bool_of_cex i e)
      | SChar,SString -> interp_exp cev (mk_string_of_char i e)
      | SChar,SRegexp -> interp_exp cev (mk_regexp_of_string i (mk_string_of_char i e))
      | SChar,SLens -> interp_exp cev (mk_lens_of_regexp i (mk_regexp_of_string i (mk_string_of_char i e)))
      | SString,SRegexp -> interp_exp cev (mk_regexp_of_string i e)
      | SString,SLens -> interp_exp cev (mk_lens_of_regexp i (mk_regexp_of_string i e))
      | SRegexp,SLens -> interp_exp cev (mk_lens_of_regexp i e)
      | SFunction(x,f1,ls1,f2), SFunction(y,t1,ls2,t2) -> 
          let fn = Id.mk i "fn" in 
          let qx = Qid.t_of_id x in 
          let qfn = Qid.t_of_id fn in 
          let e_x = EVar(i,qx) in
          let e_fn = EVar(i,qfn) in
          let e_fx = EApp(i,e_fn,e_x) in
          let c1 = ECast(i,t1,f1,invert_blame b,e_x) in 
          let c2 = ECast(i,f2,t2,b,e_fx) in
	  let alloc_c2 = EAlloc(i,ls1@ls2,c2) in
          let apped_cast = EApp(i,mk_fun i fn f (mk_fun i y t1 (mk_let i x f1 c1 alloc_c2)),e) in
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
	    let casted_pair = ECase(i,e,[ (PPar(i,px,py), EPair(i,c1,c2)) ],t) in
	      interp_exp cev casted_pair
      | SData(fl,x),SData(tl,y) when Qid.equal x y -> 
          let rec aux acc l1 l2 = acc && match l1,l2 with
            | [],[] -> true
            | h1::t1,h2::t2 -> aux (syneq_sort h1 h2) t1 t2 
            | _ -> false in 
          if aux true fl tl then interp_exp cev e
          else 
            let _,(svl,cl) = get_type (CEnv.lookup_type cev) i x in               
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
                       let ei = EVar(i,qx) in 
                         (pi,ei)
                   | Some fi,Some ti -> 
                       let li_f = 
                         Safelist.fold_right 
                           (fun tj acc -> ETyApp(i,acc,tj))
                           tl (EVar(i,li)) in 
                       let py = PVar(i,y,Some fi) in 
                       let pi = PVnt(i,li,Some py) in 
                         (* this cast cannot be expanded! (it would loop on recursive data types) *)
                       let ei = EApp(i,li_f,ECast(i,fi,ti,b,EVar(i,qy))) in
                         (pi,ei)
                   | _ -> run_error i 
                       (fun () -> msg "@[different@ datatypes@ in@ cast@ expression@]"))
              (Safelist.combine cl_finst cl_tinst) in 
              interp_exp cev (mk_let i x f e (ECase(i,EVar(i,qx),pl,t))) (* !!! OPT *)
      | SRefine(x,t1,e1),SRefine(y,t2,e2) -> 
          if Id.equal x y && syneq_sort t1 t2 && syneq_exp e1 e2 
	  then interp_exp cev e
          else cast_refinement f y t2 e2 e
      | _,SRefine(x,t2,e2) -> cast_refinement f x t2 e2 e
      | SRefine(x,f1,e1),t1 -> interp_cast cev i b f1 t1 e
      | SForall(x,f1),SForall(y,t1) ->
	  (* no need to freshen if compatibility substitutes appropriately *)
          let e_ex = ETyApp(i,e,SVar x) in
	  interp_exp cev (ETyFun(i,x,ECast(i,f1,t1,b,e_ex)))
      | _ -> 
          run_error i 
            (fun () -> 
               msg "@[cannot@ cast@ incompatible@ %s@ to@ %s@]"
                 (string_of_sort f)
                 (string_of_sort t)) in 
    res

(* expressions *)
and interp_exp cev e0 = 
(*   Util.format "INTERP_EXP: %s@\n%!" (Bprint.string_of_exp e0); *)
  match e0 with 
  | EVar(i,q) ->       
      begin match CEnv.lookup cev q with
        | Some(_,v) -> v
        | None -> run_error i (fun () -> msg "@[%s is not bound@]" (Qid.string_of_t q))
      end

  | EOver(i,op,_) -> 
      run_error i
        (fun () ->
           format_exp e0;
           msg "@[unresolved@ overloaded@ operator %s@]" (string_of_op op))

  | EApp(_,e1,e2) ->
      let v1 = interp_exp cev e1 in 
      let v2 = interp_exp cev e2 in 
      (V.get_f v1) v2

  | ELet(_,b,e) -> 
      let bcev,_ = interp_binding cev b in
      interp_exp bcev e
          
  | EFun(i,p,_,e) ->
      let f v =        
        let body_cev = 
          CEnv.update cev 
            (Qid.t_of_id (id_of_param p)) 
            (G.Unknown,v) in 
        interp_exp body_cev e in 
      V.Fun(i,f)

  | EPair(i,e1,e2) -> 
      let v1 = interp_exp cev e1 in 
      let v2 = interp_exp cev e2 in 
      V.Par(i,v1,v2)

  | ECase(i,e1,pl,_) -> 
      let v1 = interp_exp cev e1 in 
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
      interp_exp ei_cev ei

  (* we model tyfuns as functions that take unit to [whatever]; 
     tyapps just apply the tyfun to unit *)
  | ETyFun(i,_,e) -> 
      interp_exp cev (EFun(i,Param(i,Id.wild,SUnit),None,e))

  | ETyApp(i,e1,s2) -> 
      interp_exp cev (EApp(i,e1,EUnit(i)))

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
      let csi = Safelist.map (fun (ci,cj) -> (Char.code ci, Char.code cj)) cs in
      let mk = if pos then Bregexp.mk_cset else Bregexp.mk_neg_cset in 
      V.Rx(i,mk csi)

  | ECast(i,f,t,b,e) -> interp_cast cev i b f t e

  | ELoc(i,l) -> 
      begin match H.get i l with
	| H.Term(e1) ->
	    let v = interp_exp cev e1 in
	      H.update l v;
	      v
	| H.Value(v1) -> v1
      end

  | EAlloc(i,ls,e1) ->
      let fresh_e1 = H.alloc ls e1 in
	interp_exp cev fresh_e1

(*
and interp_cast cev i f t b v0 =
  if trivial_cast f t then v0
  else match f,t with
    | SChar,SString ->
	
(* TODO: we need conversion functions to handle these cases... *)

*)

and interp_binding cev b0 = 
  match b0 with
  | Bind(i,p,so,e) -> 
      Trace.debug "binds+" (fun () -> Util.format "compiling binding %s@\n%!" (string_of_pat p));
      let v = interp_exp cev e in 
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
      
let rec interp_decl cev ms d0 = 
(*   Util.format "INTERP_DECL: %s@\n%!" (Bprint.string_of_decl d0); *)
  match d0 with
  | DLet(i,b) ->
      let bcev,xs = interp_binding cev b in
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
                   let s = mk_univ (SFunction(Id.wild,s,[],sx)) in 
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
            try OK(interp_exp cev e)
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
                let v2 = interp_exp cev e2 in
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
                let v2 = interp_exp cev e2 in 
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
      let cev = CEnv.set_ctx (CEnv.empty qm) (qm::nctx@G.pre_ctx) in
      let new_cev,_ = interp_mod_aux cev [m] ds in
      G.register_env (CEnv.get_ev new_cev) m

let print_stats () = ()
