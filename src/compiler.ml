(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - Focal checker and interpreter                  *)
(****************************************************************)
(* $Id$ *)

open Syntax

(* --------------- Imports --------------- *)
let sprintf = Printf.sprintf  
let (@) = Safelist.append
let make_rv = Registry.make_rv
let sort_of_rv = Registry.sort_of_rv
let value_of_rv = Registry.value_of_rv

(* --------------- Unit tests --------------- *)
let tests = Prefs.createStringList
  "test"
  "run unit test for the specified module"
  "run unit tests for the specified module"
let _ = Prefs.alias tests "t"
  
let test_all = Prefs.createBool "test-all" false
  "run unit tests for all modules"
  "run unit tests for all modules"

let check_test m = 
  Safelist.fold_left 
    (fun r qs -> r or (Syntax.qid_prefix (Registry.parse_qid qs) m))
    (Prefs.read test_all)
    (Prefs.read tests)
    
(* --------------- Error Reporting --------------- *)
let debug s_thk = 
  Trace.debug "compiler" (fun () -> Printf.eprintf "%s\n%!" (s_thk ()))

let parse_error i msg_thk = 
  let msg = Printf.sprintf "Parse error: %s" (msg_thk ()) in
  raise (Error.Compile_error(i, Lexer.filename (), msg))
      
let sort_error i msg_thk = 
  let msg = Printf.sprintf "Sort checking error: %s" (msg_thk ()) in
  raise (Error.Compile_error(i, Lexer.filename (), msg))
    
let test_error i msg_thk = 
  let msg = Printf.sprintf "Unit test failed: %s" (msg_thk ()) in    
  raise (Error.Compile_error(i, Lexer.filename (), msg))

let run_error i msg_thk = 
  let msg = Printf.sprintf "Unexpected run error: %s" (msg_thk ()) in    
  raise (Error.Compile_error(i, Lexer.filename (), msg))
          
(* --------------- Environments --------------- *)
module type CommonEnvSig = sig
  type t 
  val empty : unit -> t
  val get_ev : t -> Registry.rv Env.t
  val set_ev : t -> Registry.rv Env.t -> t
  val get_ctx : t -> Syntax.qid list
  val set_ctx : t -> Syntax.qid list -> t
  val to_string : t -> string
end

(* compiling environments *)
module type CEnvSig = sig
  include CommonEnvSig
  val lookup : t -> Syntax.qid -> Registry.rv option
  val update : t -> Syntax.qid -> Registry.rv -> t
  val overwrite : t -> Syntax.qid -> Registry.rv -> t
end    

(* sort checking environments *)
module type SCEnvSig = sig
  include CommonEnvSig
  val add_rec_var : t -> Syntax.qid -> t
  val clear_rec_vars : t -> t
  val rec_var_ok : t -> Syntax.qid -> bool 
  val get_rec_vars : t -> Syntax.qid list
  val set_rec_vars : t -> Syntax.qid list -> t
  val lookup : t -> Syntax.qid -> Syntax.sort option
  val update : t -> Syntax.qid -> Syntax.sort -> t
end

module CEnv : CEnvSig = struct
  type t = Syntax.qid list * (Registry.rv Env.t)
      
  let empty () = ([], (Env.empty ()))    

  (* getters and setters *)
  let get_ev cev = let (_,ev) = cev in ev
  let set_ev cev ev = let (os,_) = cev in (os,ev)
  let get_ctx cev = let (os,_) = cev in os
  let set_ctx cev qs = let (os,ev) = cev in (qs,ev)
						  
  (* lookup from a cev, then from the library *)
  let lookup cev q = 
    match Env.lookup (get_ev cev) q with
	None -> Registry.lookup_library_ctx (get_ctx cev) q
      | Some rv -> Some rv
	  
  (* update a cev with a new rv *)
  let update cev q rv = 
    set_ev cev (Env.update (get_ev cev) q rv)
      
  (* overwrite a cev with a new value *)
  let overwrite cev q rv = 
    set_ev cev (Env.overwrite (get_ev cev) q rv)
      
  let to_string cev = 
    let (os,ev) = cev in 
      sprintf "{ctx={%s} env=%s}" 
	(Misc.concat_list ", " (Safelist.map string_of_qid os))
	(Env.to_string ev Registry.string_of_rv)
end
	
module SCEnv : SCEnvSig = struct
  type t = Syntax.qid list * CEnv.t
  let empty () = ([], CEnv.empty ())
    
  (* getters/setters for sort_checking_envs *)
  let get_ev sev = let (_,cev) = sev in CEnv.get_ev cev
  let set_ev sev ev = let (rs,cev) = sev in (rs,CEnv.set_ev cev ev)
  let get_ctx sev = let (_,cev) = sev in CEnv.get_ctx cev
  let set_ctx sev qs = let (rs,cev) = sev in (rs,CEnv.set_ctx cev qs) 

  (* add a qid to the list of not-recursive variables *)
  let add_rec_var sev q = let (rs,cev) = sev in (q::rs,cev)
						  
  (* check if a use of a variable is OK *)
  let rec_var_ok sev q = let (rs,_) = sev in 
    not (Safelist.exists (qid_equal q) rs)

  let get_rec_vars sev = let (rs,_) = sev in rs
  let set_rec_vars sev rs = let (_,cev) = sev in (rs,cev)
      
  (* clear the list of recursive variables (e.g., when we go under a lambda) *)
  let clear_rec_vars sev = let (_,cev) = sev in ([],cev)
						  
  let lookup sev q = 
    let (_,cev) = sev in
      match CEnv.lookup cev q with
	  None -> None
	| Some rv -> Some (Registry.sort_of_rv rv)
	      
  (* update a cev with a new sort *)
  let update sev q s = 
    let (rs,cev) = sev in
      (rs, (CEnv.update cev q (Registry.make_rv s (Value.dummy s))))
	
  let to_string sev = 
    let (rs,cev) = sev in 
      sprintf "{compile_env=%s, recursive_vars={%s}}"
	(CEnv.to_string cev)      	
	(Misc.concat_list ", " (Safelist.map string_of_qid rs))
end

(* --------------- Sort checker --------------- *)

(* utility functions *)

(* convert a list of parameters ps with sorts s1,..sn and a sort s 
 * to arrow sort s1 -> ... -> sn -> s *)
let sort_of_param_list i ps s = 
  Safelist.fold_right (fun p ts -> SArrow(sort_of_param p,ts))  ps s

let oper_of_xs xs = Safelist.fold_right
    (fun _ s -> STOper(SType, s)) xs SType

(* check that s matches expected sort es. *) 
let rec match_sort es s = 
  match (es,s) with
      (SName,SName) -> true
    | (SLens,SLens) -> true
    | (SType,SType) -> true
    | (SView,SView) -> true
    | (SArrow(es1,es2),SArrow(s1,s2)) ->
	let b1 = match_sort es1 s1 in
  	let b2 = match_sort es2 s2 in
 	  (b1 & b2)
    | (STOper(es1,es2),STOper(s1,s2)) ->
	let b1 = match_sort es1 s1 in
  	let b2 = match_sort es2 s2 in
 	  (b1 & b2)
    | _ -> false

let expect_sort msg sort check_term string_of_term term coercions =
  let term_sort, new_term = check_term term in
  let sorts_equal = match_sort sort term_sort in
  if sorts_equal then new_term else
    coercions sort term_sort new_term
	(fun () -> 
	   sprintf "Sort checking error in %s: %s expected but %s found\n\t%s"
	     msg
	     (string_of_sort sort)
	     (string_of_sort term_sort)
	     (string_of_term term)
	)	
      
let rec expect_sort_exp msg sev sort e = 
  expect_sort msg sort (check_exp sev) string_of_exp e
    (fun es s e msg ->
       let i = info_of_exp e in
       match es,s with
         | SType, SView -> EType(i,TT(TSingleton(i,e)))
         | _,_ -> sort_error i msg)

and expect_sort_ptypeexp msg sev sort pt = 
  expect_sort msg sort (check_ptypeexp sev) string_of_ptypeexp pt
    (fun es s pt msg ->
       let i = info_of_ptypeexp pt in
       match es,s with
         | SType, SView ->
             begin
               match pt with
                 | TVar(i',x) -> TSingleton(i,EVar(i',x))
                 | _ -> sort_error i msg
             end
         | _,_ -> sort_error i msg)

and expect_sort_typeexp msg sev sort t = 
  expect_sort msg sort (check_typeexp sev) string_of_typeexp t
    (fun es s t msg -> let i = info_of_typeexp t in sort_error i msg)

(* EXPRESSIONS *)    
and check_exp sev e0 = 
  (* let _ = debug (sprintf "checking expression %s" (string_of_exp e0)) in *)
    match e0 with
	EVar(i,q) ->
	  begin
	    (* look up in the environment; check that recursive uses OK *)
	    match (SCEnv.rec_var_ok sev q, SCEnv.lookup sev q) with
		(true, Some s) -> s, e0
	      | (false, Some _)  -> 
		  sort_error i 
		    (fun () -> 
		       sprintf  "%s may not be used recursively" (string_of_qid q))
	      | (_, None) -> 
		  sort_error i 
		    (fun () -> sprintf "%s is not bound" 
		       (string_of_qid q))
	  end
	    
      | EFun(i,[],_,_) -> run_error i (fun () -> sprintf "zero argument function")
	  
      | EFun(i,[p],return_sort_opt,body) ->
	  (* fully-annotated, simple lambda *)
	  let param_sort = sort_of_param p in
	  let param_id = id_of_param p in	  
	  let body_sev = (* check body in an env where param has its sort *)
	    SCEnv.clear_rec_vars
	      (SCEnv.update sev
		 (qid_of_id param_id)
		 param_sort)
	  in	    
	  let body_sort, new_body = match return_sort_opt with
	      None -> check_exp body_sev body
	    | Some return_sort -> 
		return_sort, 
		expect_sort_exp "function body" body_sev return_sort body
	  in
	  let fun_sort = SArrow(param_sort, body_sort) in
	  let new_e0 = EFun(i,[p], Some body_sort, new_body) in
	    fun_sort, new_e0
      | EFun(i,p1::p2::ps,return_sort_opt,body) ->
	  (* multi-parameter function; rewrite to simple lambda *)
	  let new_body = EFun(i,p2::ps,return_sort_opt,body) in
	    (* FIXME: do this better... it's going to re-flatten f many times *)
	    check_exp sev (EFun(i,[p1],None,new_body))	    
      | EMap(i,ms) ->	
	  let map_sort = SArrow(SName, SLens) in 
	  (* check that each element of ms is a name * lens pair *)
	  let new_ms = Safelist.map
	    (fun (i,n,l) -> 
	       let new_n = expect_sort_exp "map name" sev SName n in
	       let new_l = expect_sort_exp "map lens" (SCEnv.clear_rec_vars sev) SLens l in
		 (i, new_n, new_l))
	    ms
	  in 
	  let new_e0 = EMap(i, new_ms) in
	    map_sort, new_e0
	      
      | EApp(i,e1,e2) ->
	  begin
	    let e1_sort,new_e1 = check_exp sev e1 in
	      match e1_sort with
		  SArrow(param_sort,return_sort) ->
		    let new_e2 = 
		      expect_sort_exp "right-hand side of application" sev param_sort e2 
		    in
		    let new_e0 = EApp(i, new_e1, new_e2) in
		      return_sort, new_e0
		| _ -> 
		    sort_error i 
		      (fun () -> sprintf 
			 "expected function but found %s at left-hand side of application"		       
			 (string_of_sort e1_sort))
	  end		  	    
      | ELet(i,bs,e) ->
	  let old_rs = SCEnv.get_rec_vars sev in
	  let bsev,_,new_bs = check_bindings sev bs in
	  let bsev1 = SCEnv.set_rec_vars bsev old_rs in
	  let e_sort, new_e = check_exp bsev1 e in
	  let new_e0 = ELet(i, new_bs, new_e) in
	    (e_sort, new_e0)
	      
      | EName(i,x) -> (SName, e0)
	    
      | EType(i,t) ->
	  let new_t = expect_sort_typeexp "type expression" sev SType t in 
	  let new_e0 = EType(i, new_t) in
	    (SType, new_e0)
	    
      | EView(i,ks) ->
	  let new_ks = Safelist.map
	    (fun (i,ne,ve) -> 
	       let new_ne = expect_sort_exp "view expression" sev SName ne in 
	       (* ve can either be a view, or a name, which is shorthand for n={} *)
	       let ve_sort, new_ve0 = check_exp sev ve in 
	       let ve_i = info_of_exp new_ve0 in
	       let new_ve = match ve_sort with		   
		   SName -> EView(ve_i, [(ve_i, new_ve0, emptyView ve_i)])
		 | SView -> new_ve0
		 | s -> sort_error ve_i 
		     (fun () -> 
			sprintf "expected name or view but found %s"
			  (string_of_sort s))
	       in
		 (i, new_ne, new_ve))
	    ks
	  in		      
	  let new_e0 = EView(i, new_ks) in
	    (SView, new_e0)
	      
      | EConsView(i,e1,e2) ->
	  let e1_sort, new_e10 = check_exp sev e1 in 
	  let e1_i = info_of_exp new_e10 in
	  let new_e1 = match e1_sort with		   
	      SName -> EView(e1_i, [(e1_i, new_e10, emptyView e1_i)])
	    | SView -> new_e10
	    | s -> sort_error e1_i 
		(fun () -> 
		   sprintf "expected name or view but found %s"
		     (string_of_sort s))
	  in
	  let new_e2 = expect_sort_exp "cons list view expression" sev SView e2 in
	  let new_e0 = EConsView(i, new_e1, new_e2) in
	    (SView, new_e0)
	      
(* TYPE EXPRESSIONS *)
and check_typeexp sev t0 = 
  (* let _ = debug (sprintf "checking typeexpression %s" (string_of_typeexp t0)) in *)
    match t0 with
	Syntax.TT pt -> 
	  let pt_sort, new_pt = check_ptypeexp sev pt in
	  let new_t0 = Syntax.TT new_pt in
	    pt_sort, new_t0
      | Syntax.NT pt -> 
	  let pt_sort, new_pt = check_ptypeexp sev pt in
	  let new_t0 = Syntax.NT new_pt in
	    pt_sort, new_t0
	  
and check_ptypeexp sev pt0 = match pt0 with
    TEmpty(i) -> (SType, pt0)
  | TAny(i) -> (SType, pt0)
  | TVar(i,q) -> 
      begin
	(* look up in the environment; check that recursive uses OK *)
	match (SCEnv.rec_var_ok sev q, SCEnv.lookup sev q) with
	    (true, Some s) -> s, pt0
	  | (false, Some _)  -> 
	      sort_error i 
		(fun () -> 
		   sprintf "%s may not be used recursively" 
		     (string_of_qid q))
	  | (_,None) -> 
	      sort_error i 
		(fun () -> 
		   sprintf "%s is not bound"
		     (string_of_qid q))
      end
  | TApp(i,pt1,pt2) ->
      begin
	let pt1_sort, new_pt1 = check_ptypeexp sev pt1 in
	  match pt1_sort with
	      STOper(param_sort,return_sort) ->
		let new_pt2 = 
		  expect_sort_ptypeexp 
		    "right hand side of type application" 
		    sev param_sort pt2
		in
		let new_pt0 = TApp(i,new_pt1, new_pt2) in
		  return_sort, new_pt0
	    | pt1_sort -> 
		run_error i 
		  (fun () -> 
		     sprintf "expected type operator but found %s at right-hand side of type application"
		       (string_of_sort pt1_sort))
      end

  | TFun(i,xs,pt_sort,pt) -> 
      begin
	match xs with 
	    [] -> run_error i (fun () -> "zero-argument type function")
	  | [x] ->
	      let i = info_of_id x in
	      let pt_sev = SCEnv.clear_rec_vars (SCEnv.update sev (qid_of_id x) SType) in	
	      let new_pt = 
		expect_sort_ptypeexp "body of type operator" pt_sev pt_sort pt
	      in
	      let tfun_sort = STOper(SType,pt_sort) in
	      let new_pt0 = TFun(i,xs,pt_sort,new_pt) in
		tfun_sort, new_pt0
	  | (x::xrest) ->
	      let pt_body = TFun(i, xrest, pt_sort, pt) in
                check_ptypeexp sev (TFun(i,[x],STOper(SType, pt_sort),pt_body))
      end	  
        
  | TName(i,ne,pt) ->      
      let new_ne = expect_sort_exp "name type" sev SName ne in
      let new_pt = 
	expect_sort_ptypeexp "name type" (SCEnv.clear_rec_vars sev) SType pt in
      let new_pt0 = TName(i, new_ne, new_pt) in
	(SType, new_pt0)
	  
  | TBang(i,excls,pt) ->
      let new_excls = Safelist.map 
	(fun exi -> expect_sort_exp "exception list" sev SName exi) excls
      in
      let new_pt = 
	expect_sort_ptypeexp "!-type" (SCEnv.clear_rec_vars sev) SType pt 
      in
      let new_pt0 = TBang(i,new_excls, new_pt) in
	(SType, new_pt0)
  	  
  | TStar(i,excls,pt) ->
      let new_excls = 
	Safelist.map 
	  (fun exi -> expect_sort_exp "exception list" sev SName exi) 
	  excls
      in
      let new_pt = 
	expect_sort_ptypeexp "!-type" (SCEnv.clear_rec_vars sev) SType pt in
      let new_pt0 = TStar(i, new_excls, new_pt) in
	(SType, new_pt0)
	  
  | TCat(i,pts) ->
      let new_pts = Safelist.map
	(fun pti -> 
	   let i = info_of_ptypeexp pti in
	     expect_sort_ptypeexp "concatenated type" sev SType pti)
	pts
      in
      let new_pt0 = TCat(i, new_pts) in
	(SType, new_pt0)
	  
  | TUnion(i,pts) ->
      let new_pts = 
	Safelist.map
	  (fun pti -> 
	     let i = info_of_ptypeexp pti in
	       expect_sort_ptypeexp "union type" sev SType pti)
	  pts
      in
      let new_pt0 = TUnion(i, new_pts) in
	(SType, new_pt0)

  | TSingleton(i,e) ->
      let new_e = expect_sort_exp "singleton type" sev SView e in
        (SType, (TSingleton(i,new_e)))
	    	
and check_bindings sev bs = 
  (* let _ = debug (sprintf "checking bindings %s\n" (string_of_bindings bs)) in *)
  (* collect up a compile_env that includes recursive bindings *)
  let bsev =
    Safelist.fold_left
      (fun bsev (BDef(i,f,xs,so,e)) ->
	 let f_qid = qid_of_id f in
	   match so with
	       (* recursive bindings must give their return sort *)
	     | Some s -> 
		 let b_sort = (sort_of_param_list i xs s) in
		   SCEnv.add_rec_var (SCEnv.update bsev f_qid b_sort) f_qid
	     | None   -> bsev)
      sev
      bs
  in
  let rec check_binding sev bi =   
    (* let _ = debug (sprintf "checking binding %s\n" (string_of_binding bi)) in *)
       match bi with 
	Syntax.BDef(i,f,[],so,e) -> 
	  let f_qid = qid_of_id f in
	  let e_sort, new_e = 
	    match so with 
		None -> check_exp sev e 
	      | Some e_sort -> e_sort, expect_sort_exp "let binding" sev e_sort e
	  in
	  let new_bi = Syntax.BDef(i, f, [], Some e_sort, new_e) in
	    (SCEnv.update bsev f_qid e_sort, f_qid, new_bi)
      | Syntax.BDef(i,f,ps,so,e) -> 
	  let new_e = EFun(i,ps,so,e) in
	    (* rewrite bindings with parameters to plain ol' lambdas *)
	    check_binding sev (Syntax.BDef(i,f,[],None,new_e))
  in
  let bsev,names_rev,new_bs_rev = Safelist.fold_left 
    (fun (bsev, names_rev,new_bs_rev) bi ->  
       let bsev, f_qid, new_bi = check_binding bsev bi in
	 bsev, f_qid::names_rev, new_bi::new_bs_rev)
    (bsev,[],[])
    bs
  in
    (bsev, Safelist.rev names_rev, Safelist.rev new_bs_rev)
      
and check_typebindings sev tbs = 
  (* let _ = debug (sprintf "checking typebindings %s\n" (string_of_typebindings tbs)) in *)
  (* collect up a compile_env that includes recursive type bindings *)
  let tbsev, tb_sorts_rev = Safelist.fold_left
    (fun (tbsev, tb_sorts_rev) (x,xs,t) ->
       let i = info_of_id x in (* FIXME: merge info from x and xs *)
       let x_qid = qid_of_id x in
       let tb_sort = oper_of_xs xs in
       let new_sev = SCEnv.add_rec_var (SCEnv.update tbsev x_qid tb_sort) x_qid in
	 (new_sev, tb_sort::tb_sorts_rev))
    (sev,[])
    tbs
  in
  let tb_sorts = Safelist.rev tb_sorts_rev in
  let annotated_tbs = Safelist.combine tb_sorts tbs in
  let rec check_annotated_typebinding sev atbi = 
    (* let _ = debug (sprintf "checking typebinding %s\n" (string_of_typebinding (snd atbi))) in *)
      match atbi with
	  (t_sort,(x,[],t)) ->
	    let x_qid = qid_of_id x in
	    let new_t = expect_sort_typeexp "type binding" sev t_sort t in 
	    let new_tbi = (x,[],new_t) in
	      (sev, x_qid, new_tbi)
	| (t_sort,(x,xs,t)) -> 
	    let i = info_of_id x in
	    let new_t = match t with 
		TT pt -> TT (TFun(i,xs,SType,pt))
	      | NT pt -> TT (TFun(i,xs,SType,pt))
	    in
	      (* rewrite bindings with parameters to plain ol' lambdas *)
	      check_annotated_typebinding sev (t_sort, (x,[],new_t))
  in
  let tbsev,names_rev,new_tbs_rev = Safelist.fold_left
    (fun (tbsev,names_rev,new_tbs_rev) atbi ->  
       let tbsev, x_qid, new_tbi = check_annotated_typebinding tbsev atbi in
	 tbsev, x_qid::names_rev, new_tbi::new_tbs_rev)
    (tbsev,[],[])
    annotated_tbs
  in
    (tbsev, Safelist.rev names_rev, Safelist.rev new_tbs_rev)
      
(* type check a single declaration *)
let rec check_decl sev m di = 
  (* let _ = debug_decl (sprintf "checking declaration %s\n" (string_of_decl di)) in *)
  let old_rec_vars = SCEnv.get_rec_vars sev in
  match di with
    | DLet(i,bs) -> 
	let new_sev, names, new_bs = check_bindings sev bs in
	let new_di = DLet(i, new_bs) in
	  (SCEnv.set_rec_vars new_sev old_rec_vars, names, new_di)

    | DType(i,tbs) ->      
	let new_sev, names, new_tbs = check_typebindings sev tbs in
	let new_di = DType(i, new_tbs) in
	  (SCEnv.set_rec_vars new_sev old_rec_vars, names, new_di)  
    | DMod(i,n,ds) ->
	let n_qid = qid_of_id n in	
	let mn = Syntax.dot m n_qid in
	let m_sev,names,new_ds = check_module_aux sev mn ds in
	let new_sev, names_rev = Safelist.fold_left 
	  (fun (new_sev, names) q -> 
	     match Env.lookup (SCEnv.get_ev m_sev) q with
		 None -> run_error i 
		   (fun () -> 
		      sprintf "the just-compiled declaration, %s, went missing"
			(string_of_qid q))
	       | Some q_rv ->
		   let nq_dot_q = dot n_qid q in
		     (SCEnv.update sev nq_dot_q (sort_of_rv q_rv), nq_dot_q::names))
	  (sev,[])
	  names
	in
	let new_di = DMod(i,n,new_ds) in
	  (SCEnv.set_rec_vars new_sev old_rec_vars, Safelist.rev names_rev, new_di)
    | DTest(i,e,reso) ->	
	if not (check_test m) then (sev, [], di)
	else
	  begin
	    let new_e = expect_sort_exp "test left-hand side" sev SView e in
	    let new_reso = 
	      match reso with 
		  None     -> None 
		| Some res -> Some (expect_sort_exp "test result" sev SView res)
	    in
	    let new_di = DTest(i,new_e, new_reso) in
	      (sev, [], new_di)
	  end
    | DTestSync(i,lo,la,lb,ty,orig,result) -> 
	if not (check_test m) then (sev, [], di)
	else
	  begin
	    let new_lo = expect_sort_exp "test archive lens sync" sev SLens lo in
	    let new_la = expect_sort_exp "test replica A lens sync" sev SLens la in
	    let new_lb = expect_sort_exp "test replica B lens sync" sev SLens lb in
	    let new_ty = expect_sort_ptypeexp "test sync type" sev SType ty in
	    let new_orig = expect_sort_exp "test sync orig view" sev SView orig in
	    let new_result = expect_sort_exp "test sync result view" sev SView result in
	    let new_di = DTestSync(i,new_lo,new_la,new_lb,new_ty,new_orig,new_result) in
	      (sev, [], new_di)
	  end
	
(* type check a module *)
and check_module_aux sev m ds = 
  let new_sev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sev, names, new_ds_rev) di ->
	 let new_sev, new_names, new_di = check_decl sev m di in
	   new_sev, names@new_names, new_di::new_ds_rev)
      (sev,[],[])
      ds
  in
    (new_sev, names, Safelist.rev new_ds_rev)
      
let check_module m0 = 
  (* let _ = debug (sprintf "checking module %s" (string_of_module m0)) in *)
  let (Syntax.MDef(i,m,nctx,ds)) = m0 in
  let sev = SCEnv.set_ctx (SCEnv.empty ()) (nctx@Registry.pre_ctx) in
  let _,_,new_ds = check_module_aux sev (Syntax.qid_of_id m) ds in 
  let new_m0 = Syntax.MDef(i,m,nctx,new_ds) in
    new_m0

(* --------------- Compiler --------------- *)

(* EXPRESSIONS *)
let rec compile_exp cev e0 =
  (* let _ = debug (sprintf "compiling expression %s" (string_of_exp e0)) in *)
    match e0 with
	EVar(i,q) ->
	  begin
	    (* look up in the environment *)
	    match CEnv.lookup cev q with
		Some rv -> rv
	      | None -> run_error i
		  (fun () -> 
		     sprintf "%s is not bound"
		       (string_of_qid q))
	  end
	  	  
      | EFun(i,[p],Some ret_sort,e) ->
	  (* fully-annotated, simple lambda *)
	  (* the actual implementation of f *)
	  let param_qid = qid_of_id (id_of_param p) in
	  let param_sort = sort_of_param p in
	  let f_impl v =
	    let body_cev = CEnv.update cev param_qid (make_rv param_sort v) in
	      value_of_rv (compile_exp body_cev e)
	  in
	  let fun_sort = SArrow(param_sort, ret_sort) in
	  let fun_value = Value.F(f_impl) in
	    make_rv fun_sort fun_value
      | EFun(i,_,Some _, _) -> run_error i (fun () -> "unflattened function")
      | EFun(i,_,_, _) -> run_error i (fun () -> "unannotated function")
      | EMap(i,ms) ->
	  let id_exp = EVar(i, Registry.parse_qid "Native.Prelude.id") in
	  (* maps are implemented as functions from names -> lenses *)
	  let map_impl = Safelist.fold_left
	    (fun f (i,n,l) ->
	       let n2 = compile_exp_name cev n in
		 fun v -> match v with
		     Value.N n1 ->
		       if (n1 = n2) then value_of_rv (compile_exp cev l)
		       else f v
		   | _ -> run_error i (fun () -> "argument to map is not a name"))
	    (fun _ -> value_of_rv (compile_exp cev id_exp))
	    ms
	  in	    
	  let map_sort = SArrow(SName, SLens) in
	  let map_value = Value.F(map_impl) in
	    make_rv map_sort map_value	    
    | EApp(i,e1,e2) ->
	begin
	  let e1_rv = compile_exp cev e1 in
	    match sort_of_rv e1_rv, value_of_rv e1_rv with
	      | SArrow(_,return_sort), Value.F f ->
		  let e2_rv = compile_exp cev e2 in
		    make_rv return_sort (f (value_of_rv e2_rv))
	      | _ -> run_error i 
		  (fun () -> sprintf "expected function but found %s at left-hand side of application"
		     (string_of_sort (sort_of_rv e1_rv)))
	end
    | ELet(i,bs,e) ->
	let bcev,_ = compile_bindings cev bs in
	  compile_exp bcev e	    
    | EName(i,x) -> make_rv SName (Value.N (name_of_id x))	  
    | EType(i,t) -> compile_typeexp cev t	    
    | EView(i,ks) ->
	let rec compile_viewbinds cev ks = match ks with
	    []               -> V.empty
	  | (i,ne,ve)::krest ->
	      let n = compile_exp_name cev ne in
	      let v = compile_exp_view cev ve in
	      let vrest = compile_viewbinds cev krest in
		match (V.get vrest n) with
		    None   -> V.set vrest n (Some v)
		  | Some _ -> run_error i (fun () -> sprintf "name %s is repeated in view" n)
	in
	let view_value = (Value.V (compile_viewbinds cev ks)) in
	  make_rv SView view_value
	    
    | EConsView(i,e1,e2) ->
	let v1 = compile_exp_view cev e1 in
	let v2 = compile_exp_view cev e2 in
	let v = V.cons v1 v2 in
	let cons_value = Value.V (v) in
	  make_rv SView cons_value
	    
and compile_exp_name cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    match value_of_rv (e_rv) with
      | Value.N n -> n
      | _ -> run_error i (fun () -> sprintf "name expected but %s found" 
			    (string_of_sort (sort_of_rv e_rv)))
	  
and compile_exp_view cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    match value_of_rv (e_rv) with
      | Value.V v -> v
      | _ -> run_error i (fun () -> sprintf "view expected but %s found" 
			    (string_of_sort (sort_of_rv e_rv)))

and compile_exp_lens cev e = 
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    match value_of_rv (e_rv) with
      | Value.L l -> l
      | _ -> run_error i (fun () -> sprintf "lens expected but %s found" 
			    (string_of_sort (sort_of_rv e_rv)))
  
and compile_typeexp cev t0 =
  (* let _ = debug (sprintf "compiling typeexpression %s" (string_of_typeexp t0)) in *)
    match t0 with
	Syntax.TT pt ->
	  let pt_sort, it_val = compile_ptypeexp cev pt in
	    make_rv pt_sort (Value.T (Type.TT(Type.mk_ptype it_val)))
      | Syntax.NT pt ->
	  let pt_sort, it_val = compile_ptypeexp cev pt in
	    make_rv pt_sort (Value.T (Type.NT(Type.mk_ptype it_val)))
	      
and compile_ptypeexp cev pt0 = 
  let rec force_it it = match it with
      Type.Var(_,_,thk) -> force_it (Type.it_of_pt (thk ()))
    | Type.App(_,_,_,thk) -> force_it (Type.it_of_pt (thk ()))
    | _                 -> it
  in
  let ptype2thunk cev pt () = 
    debug (fun () -> sprintf "forced thunk for %s\n" (string_of_ptypeexp pt));
    match pt with 
	TVar(i,q) -> 
	  begin 
	    match CEnv.lookup cev q with
		Some rv -> 
		  begin
		    match value_of_rv rv with 
			Value.T (Type.TT pt) -> pt
		      | _ -> run_error i 
			  (fun () -> sprintf "type variable %s is not bound to a non-negative type" 
			     (string_of_qid q))
		  end
	      | None -> run_error i
		  (fun () -> sprintf "type variable %s is not bound" 
		     (string_of_qid q))
	  end
      | TApp(i,pt1,pt2) ->
	  begin
	    match compile_ptypeexp cev pt1 with
		STOper(_),ptv1 -> 
		  let f = match force_it ptv1 with
		      Type.Fun(_,f) -> f
		    | _        -> run_error 
			(Type.info_of_it ptv1)
			  (fun () -> sprintf "type operator expected at left-hand side of type application")
		  in
		  let _,ptv2 = compile_ptypeexp cev pt2 in		      
		    (Type.mk_ptype (f ptv2)) (* FIXME: hack! *)
	      | s,_         -> run_error i
		  (fun () -> sprintf "type operator expected at left-hand side of type application but %s found in %s"
		     (string_of_sort s) (string_of_ptypeexp pt))
	  end
      | _ -> assert false
  in match pt0 with
      TEmpty(i) -> (SType, Type.Empty(i))
    | TAny(i)   -> (SType, Type.Any(i))
    | TVar(i,q) ->
	begin
	  (* look up in the environment; check that recursive uses OK *)
	  match (CEnv.lookup cev q) with
	      Some rv ->
		let tsort = sort_of_rv rv in
		let tval = Type.Var (i,q,ptype2thunk cev pt0) in
		  tsort, tval
	    | None ->
		run_error i
		  (fun () -> sprintf "type variable %s is not bound" (string_of_qid q))
	end
    | TApp(i,pt1,pt2) ->
	begin
	  match compile_ptypeexp cev pt1 with
	      STOper(_,rs), pt1_val ->
		let _, pt2_val = compile_ptypeexp cev pt2 in
		  rs, Type.App(i, pt1_val, pt2_val, ptype2thunk cev pt0)
	    | s, _ ->
		run_error i
		  (fun () -> sprintf "type operator expected but %s found at left-hand side of type application"
		     (string_of_sort s))
	end
	  
    | TFun(i,[x],f_sort,pt) ->
	begin
	  let x_qid = qid_of_id x in
	  let f_impl it_arg =
	    let x_rv = make_rv SType (Value.T(Type.TT(Type.mk_ptype it_arg))) in
	    let body_cev = CEnv.update cev x_qid x_rv in
	    let _,body_val = compile_ptypeexp body_cev pt in
	      body_val
	  in
	  let fun_sort = STOper(SType,f_sort) in
	  let fun_value = Type.Fun(i,f_impl) in
	    fun_sort, fun_value
	end
    | TFun(i,_,_,_) -> run_error i (fun () -> "unflattened type function")
        
    | TName(i,e,pt) ->
	let n = compile_exp_name cev e in
	let _, t_val = compile_ptypeexp cev pt in
      	  (SType, (Type.Name(i,n,Type.mk_ptype t_val)))
	    
    | TBang(i,excl,pt) ->
	let excl_ns = List.map (compile_exp_name cev) excl in
	let _, t_val = compile_ptypeexp cev pt in 
	  (SType, (Type.Bang(i,excl_ns,Type.mk_ptype t_val)))
	    
    | TStar(i,excl,pt) ->
	let excl_ns = List.map (compile_exp_name cev) excl in
	let _, t_val = compile_ptypeexp cev pt in 
	  (SType, (Type.Star(i,excl_ns,Type.mk_ptype t_val)))
    | TCat(i,pts) ->
	let pt_vals = Safelist.map
	  (fun pti -> let _, ti_val = compile_ptypeexp cev pti in
	     Type.mk_ptype ti_val)
	  pts
	in
	  (SType, (Type.Cat(i,pt_vals)))
    | TUnion(i,pts) ->
	let pt_vals = Safelist.map
	  (fun pti -> let _, ti_val = compile_ptypeexp cev pti in
	     Type.mk_ptype ti_val)
	  pts
	in
	  (SType, (Type.Union(i,pt_vals)))
    | TSingleton(i,e) ->
        let v = compile_exp_view cev e in
          (SType,Type.Singleton(i,v))
	      
and compile_bindings cev bs =
  (* let _ = debug (sprintf "compiling bindings %s\n" (string_of_bindings bs)) in *)
  (* collect up a compile_env that includes recursive bindings *)
  let bcev =
    Safelist.fold_left
      (fun bcev ((BDef(i,f,xs,so,e) as bi)) ->
	 let f_qid = qid_of_id f in
	   match so with
	       (* recursive bindings must give their return sort *)
	     | Some s ->
		 let b_sort = (sort_of_param_list i xs s) in
		 let dummy = Value.dummy ~msg:(sprintf "DUMMY for %s" (string_of_binding bi)) b_sort in
		   CEnv.update bcev f_qid (make_rv b_sort dummy)
	     | None   -> bcev)
      cev
      bs
  in
  let rec compile_binding cev bi =
    (* let _ = debug (sprintf "compiling binding %s\n" (string_of_binding bi)) in *) 
    match bi with 
      Syntax.BDef(i,f,[],so,e) ->
	let f_qid = qid_of_id f in
	let e_rv = compile_exp cev e in	
	let memoized_v = Value.memoize (value_of_rv e_rv) in
	let memoized_rv = make_rv (sort_of_rv e_rv) memoized_v in
	  (f_qid, CEnv.overwrite bcev f_qid memoized_rv)
      | Syntax.BDef(i,_,_,_,_) -> run_error i (fun () -> "unflattened binding")
  in
  let bcev,names_rev = Safelist.fold_left
    (fun (bcev,names_rev) bi ->
       let f_qid, bcev = compile_binding bcev bi in
	 bcev, f_qid::names_rev)
    (bcev,[])
    bs
  in
    bcev, Safelist.rev names_rev
      
and compile_typebindings cev tbs =
  (* let _ = debug (sprintf "compiling typebindings %s\n" (string_of_typebindings tbs)) in *)
  (* collect up a compile_env that includes recursive type bindings *)
  let tbcev = Safelist.fold_left
    (fun tbcev ((x,xs,t) as ti) ->
       let i = info_of_id x in (* FIXME: merge info from x and xs *)
       let x_qid = qid_of_id x in
       let b_sort = oper_of_xs xs in
       let dummy = Value.dummy ~msg:(sprintf "DUMMY for %s" (string_of_typebinding ti)) b_sort in
       	 CEnv.update tbcev x_qid (make_rv b_sort dummy))
    cev
    tbs
  in
  let rec compile_typebinding cev tbi =
    (* let _ = debug (sprintf "compiling typebinding %s\n" (string_of_typebinding tbi)) in *)
      match tbi with
	  (x,[],t) ->
	    let x_qid = qid_of_id x in
	    let t_rv = compile_typeexp cev t in
	      (x_qid, CEnv.overwrite cev x_qid t_rv)
	| (x,_,_) -> run_error (info_of_id x) (fun () -> "unflattened type binding")
  in
  let tbcev,names_rev = Safelist.fold_left
    (fun (tbcev,names_rev) tbi ->
       let x_qid, tbcev = compile_typebinding tbcev tbi in
	 tbcev, x_qid::names_rev)
    (tbcev,[])
    tbs
  in
    tbcev, Safelist.rev names_rev

type testresult = OK of V.t | Error of string

(* type check a single declaration *)
let rec compile_decl cev m di = 
  (* let _ = debug_decl (sprintf "compiling declaration %s\n" (string_of_decl di)) in *)
  match di with
  | DLet(i,bs) -> compile_bindings cev bs 
  | DType(i,tbs) -> compile_typebindings cev tbs
  | DMod(i,n,ds) ->
      let nq = qid_of_id n in
      let mn = dot m nq in
      let m_cev, names = compile_module_aux cev mn ds in
      (* insert all the just-compiled names into the environment *)
      let new_cev, names_rev =
	Safelist.fold_left
	  (fun (cev, names) q ->
	     match Env.lookup (CEnv.get_ev m_cev) q with
		 Some rv ->
		   let nq_dot_q = dot nq q in
		     (CEnv.update cev nq_dot_q rv, nq_dot_q::names)
	       | None -> run_error i
		   (fun () -> sprintf "the just-compiled declaration, %s, went missing"
		      (string_of_qid q)))
	  (cev,[])
	  names
      in
	new_cev, Safelist.rev names_rev
  | DTest(i,e,reso) ->
      if check_test m then
	begin
	  let vo = 
	    try
	      OK (compile_exp_view cev e)
	    with (Error.Native_error(m)) -> Error m
	      | V.Illformed(s,vl) -> Error (V.format_msg_as_string ([`String s] @ (Safelist.map (fun v -> `View v) vl)))
	  in
	    match vo, reso with 
		Error _, None -> ()
	      | OK v, Some res -> 
		  let resv = compile_exp_view cev res in
		    if not (V.equal v resv) then
		      test_error i 
			(fun () -> 
			   sprintf "expected %s, found %s"
			     (V.string_of_t resv)
			     (V.string_of_t v))
	      | Error m, Some res -> 
		  let resv = compile_exp_view cev res in
		    test_error i 
		      (fun () -> 
			 sprintf "expected %s, found error\n%s"
			   (V.string_of_t resv) m)
	      | OK v, None -> 
		  test_error i 
		    (fun () -> 
		       sprintf "expected error, found %s"
			 (V.string_of_t v))		    
	end;
      (cev, [])	
  | DTestSync(i,lo,la,lb,typ,orig,result) ->
      if check_test m then
        begin
          let lo = compile_exp_lens cev lo
          and la = compile_exp_lens cev la
          and lb = compile_exp_lens cev lb
          and typ = match Registry.value_of_rv (compile_typeexp cev (Syntax.TT typ)) with
              Value.T t -> t
            | _ -> assert false 
          and orig = compile_exp_view cev orig
          and result = compile_exp_view cev result in
          let co,ca,cb =
            try
              (V.get_required orig "O", V.get_required orig "A", V.get_required orig "B")
            with
                Not_found -> 
    	      test_error i 
    		(fun () -> 
    		   sprintf "(sync): the initial view should have children 'O', 'A', and 'B'\n%s"		     
    		     (V.format_msg_as_string [`View orig]))
          in
          let ao,aa,ab =
            try
              (Lens.get lo co, Lens.get la ca, Lens.get lb cb)
            with
                Error.Native_error m | Error.Compile_error (_,_,m) | Error.Fatal_error m ->
    	      test_error i 
    		(fun () -> 
    		   sprintf "(sync): error when applying the lenses in the get direction\n%s"
    		     m)
          in
          let _,ao',aa',ab' = Sync.sync typ (Some ao) (Some aa) (Some ab) in
          let (ao',aa',ab') = match (ao',aa',ab') with Some(ao'),Some(aa'),Some(ab') -> (ao',aa',ab') | _ -> assert false in
          let co',ca',cb' =
            try
              (Lens.put lo ao' (Some co), Lens.put la aa' (Some ca), Lens.put lb ab' (Some cb))
            with
                Error.Native_error m ->
    		    test_error i 
    		      (fun () -> 
    			 sprintf "(sync): error when applying the lenses in the put direction\n%s"
    			   m)
          in
          let reso,resa,resb =
            if Name.Set.equal (V.dom result) (Name.Set.add "O" (Name.Set.add "A" (Name.Set.add "B" Name.Set.empty))) then
              (V.get_required result "O", V.get_required result "A", V.get_required result "B")
            else
              result,result,result
          in
          let report_error exp res msg =
            test_error i (fun () -> 
    			sprintf "(sync): expected %s %s, found %s"
                              msg
    			  (V.string_of_t exp)
    			  (V.string_of_t res))
          in
    	if not (V.equal reso co') then report_error reso co' "archive";
        if not (V.equal resa ca') then report_error resa ca' "replica A";
        if not (V.equal resb cb') then report_error resb cb' "replica B"
      end;
      (cev, [])
	  
and compile_module_aux cev m ds = Safelist.fold_left
  (fun (cev, names) di ->
     let new_cev, new_names = compile_decl cev m di in
       new_cev, names@new_names)
  (cev,[])
  ds

let compile_module m0 =
  (* let _ = debug (sprintf "compiling module %s" (string_of_module m0)) in *)
  let (Syntax.MDef(i,m,nctx,ds)) = m0 in
  let mq = qid_of_id m in
  let cev = CEnv.set_ctx (CEnv.empty ()) (nctx@Registry.pre_ctx) in
  let new_cev,_ = compile_module_aux cev mq ds in
    Registry.register_env (CEnv.get_ev new_cev) mq
	      
(* --------------- Exported functions --------------- *)

(* parse an AST from a lexbuf *)
let parse_lexbuf lexbuf = 
  try 
    let ast = Parser.modl Lexer.main lexbuf in
      ast
  with Parsing.Parse_error ->
    parse_error (Lexer.info lexbuf) 
      (fun () -> "Syntax error")
      
(* parse an AST from a string *)
let parse_string str = 
  parse_lexbuf (Lexing.from_string str)
    
(* parse an AST from a channel *)
let parse_chan fc = 
  parse_lexbuf (Lexing.from_channel fc)

(* end-to-end compilation of strings *)
let compile_string fake_name str = 
  let _ = Lexer.setup fake_name in
  let ast = parse_string str in  
  let ast = check_module ast in 
    compile_module ast 
      
(* end-to-end compilation of files *)
let compile_file fn n =
  let _ = Lexer.setup fn in  
  let fchan = open_in fn in
  let ast = parse_chan fchan in  
  let _ = close_in fchan in
  let m_str = string_of_id (id_of_modl ast) in
  let _ =
    if (n <> m_str) then
      sort_error 
	(info_of_module ast)
	(fun () -> sprintf "module %s must appear in a file named %s.fcl"
	   m_str (String.uncapitalize m_str))
  in
  let ast = check_module ast in 
  let _ = compile_module ast in
    Lexer.finish ()

(* FIXME: ugly backpatch hack! *)
let _ = Registry.compile_file_impl := compile_file
