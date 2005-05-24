(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - Focal checker and interpreter                  *)
(****************************************************************)
(* $Id$ *)

open Syntax

(* imported functions *)
let sprintf = Printf.sprintf  

let (@) = Safelist.append

let make_rv = Registry.make_rv
let sort_of_rv = Registry.sort_of_rv
let value_of_rv = Registry.value_of_rv

(* low-level compiler debugging *)
let compiler_debug = Prefs.createBool "debug-compiler" false 
  "print debugging information during Focal compilation"
  "print debugging information during Focal compilation"

let debug s_thk = 
  if Prefs.read compiler_debug 
  then 
    begin 
      prerr_string (sprintf "%s\n" (s_thk ()));
      flush stderr
    end
      
(* error reporting *)
let file_name = ref "" 
let failAt i mesg_thk =
  Printf.eprintf "File \"%s\", %s:\n%s\n" (!file_name) (Info.string_of_t i) (mesg_thk ());
  exit 1

(* general helpers *)
let oper_of_xs xs i = Safelist.fold_right
    (fun _ s -> STOper(i,SType(i), s)) xs (SType(i))


(* compiling environments comprise:
 *  - a qid list: the naming context, a list of open modules 
 *  - a Registry.env: mapping from qids to Registry.rv 
 *)
type compile_env = Syntax.qid list * Registry.env

let string_of_cev cev = 
  let (os,ev) = cev in 
    sprintf "{opens={%s} env=%s}" 
      (Pretty.concat_list ", " (Safelist.map string_of_qid os))
      (Registry.string_of_env ev)

(* the empty compilation enviroment*)
let empty_cev () = ([], Registry.empty ())
  
(* getters/setters for compile_envs *)
let ev_of_cev cev = let (_,ev) = cev in ev
let set_cev_ev cev ev = let (os,_) = cev in (os,ev)
let ctx_of_cev cev = let (os,_) = cev in os
let set_cev_ctx cev qs = let (os,ev) = cev in (qs,ev)
 						
(* helper functions for environments **) 
let lookup_cev cev q = 
  match (Registry.lookup_library cev ev_of_cev ctx_of_cev q) with
      None -> None 
    | Some rv -> Some rv
	
(* update a cev with a new rv *)
let update_cev cev q rv = 
  Registry.update cev ev_of_cev set_cev_ev q rv

(* overwrite a cev with a new value *)
let overwrite_cev cev q rv = Registry.overwrite cev ev_of_cev set_cev_ev q rv
    
(* sort checking environments comprise:
 *  - a qid list: a list of variables that may NOT be used recursively
 *  - a compile env
 *)
type sort_env = Syntax.qid list * compile_env

let string_of_sev sev = 
  let (rs,cev) = sev in 
    sprintf "{recursive_vars={%s} compile_env=%s}" 
      (Pretty.concat_list ", " (Safelist.map string_of_qid rs))
      (string_of_cev cev)      
    
(* the empty sort checking enviroment*)
let empty_sev () = ([], empty_cev ())
  
(* getters/setters for compile_envs *)
let ev_of_sev sev = let (_,cev) = sev in ev_of_cev cev
let set_sev_ev sev ev = let (rs,cev) = sev in (rs,set_cev_ev cev ev)
let ctx_of_sev sev = let (_,cev) = sev in ctx_of_cev cev
let set_sev_ctx sev qs = let (rs,cev) = sev in (rs,set_cev_ctx cev qs) 

(* helper functions for sort checking **) 
(* returns a sort *)
let lookup_sev sev q = 
  match (Registry.lookup_library sev ev_of_sev ctx_of_sev q) with
      None -> None 
    | Some rv -> Some (sort_of_rv rv)
	
(* update a cev with a new sort *)
let update_sev sev q s = 
  Registry.update sev ev_of_sev set_sev_ev q (make_rv s (Value.dummy s))
    
let set_rec_vars sev qs = let (rs,cev) = sev in (qs,cev)
let get_rec_vars sev = let (rs,_) = sev in rs

(* add a qid to the list of not-recursive variables *)
let add_rec_var q sev = let (rs,cev) = sev in (q::rs,cev)

(* check if a use of a variable is OK *)
let rec_var_ok sev q = let (rs,_) = sev in 
  not (Safelist.exists (qid_equal q) rs)
    
(* clear the list of recursive variables (e.g., when we go under a lambda) *)
let clear_rec_vars sev = let (_,cev) = sev in ([],cev)
						
(* convert a list of parameters ps with sorts s1,..sn and a sort s 
 * to arrow sort s1 -> ... -> sn -> s *)
let sort_of_param_list i ps s = 
  Safelist.fold_right (fun p ts -> SArrow(i,sort_of_param p,ts))  ps s

(* check that s matches expected sort es. *) 
let rec check_sort es s = 
  match (es,s) with
      (SName _,SName _) -> (true, s)
    | (SLens _,SLens _) -> (true, s)
    | (SType _,SType _) -> (true, s)
    | (SView _,SView _) -> (true, s)
    | (SArrow(_,es1,es2),SArrow(i,s1,s2)) ->
	let b1,f1 = check_sort es1 s1 in
  	let b2,f2 = check_sort es2 s2 in
 	  ((b1 & b2), SArrow(i, f1,f2))
    | (STOper(_,es1,es2),STOper(i,s1,s2)) ->
	let b1,f1 = check_sort es1 s1 in
  	let b2,f2 = check_sort es2 s2 in
 	  ((b1 & b2), STOper(i, f1,f2))
    | _ -> (false, s)

(***
 ***
 *** SORT CHECKER
 ***
 ***)

(* EXPRESSIONS *)
let expect_sort msg sort term2info check_term string_of_term term =
  let i = term2info term in
  let term_sort, new_term = check_term term in
  let term_ok, actual_term_sort = check_sort sort term_sort in
  let _ = 
    if (not term_ok) then 
      failAt i 
	(fun () -> 
	   sprintf "Sort checking error in %s: %s expected but %s found\n\t%s"
	     msg
	     (string_of_sort sort)
	     (string_of_sort actual_term_sort)
	     (string_of_term term)
	)	
  in
    new_term
      
let rec expect_sort_exp msg sev sort e = 
  expect_sort msg sort info_of_exp (check_exp sev) string_of_exp e

and expect_sort_ptypeexp msg sev sort pt = 
  expect_sort msg sort info_of_ptypeexp (check_ptypeexp sev) string_of_ptypeexp pt

and expect_sort_typeexp msg sev sort t = 
  expect_sort msg sort info_of_typeexp (check_typeexp sev) string_of_typeexp t
    
and check_exp sev e0 = 
  (* let _ = debug (sprintf "checking expression %s" (string_of_exp e0)) in *)
    match e0 with
	EVar(i,q) ->
	  begin
	    (* look up in the environment; check that recursive uses OK *)
	    match (rec_var_ok sev q, lookup_sev sev q) with
		(true, Some s) -> s, e0
	      | (false, Some _)  -> 
		  failAt i 
		    (fun () -> 
		       sprintf  "Sort checking error at variable: %s may not be used recursively" (string_of_qid q))
	      | (_, None) -> 
		  failAt i 
		    (fun () -> sprintf "Sort checking error at variable: %s is not bound" 
		       (string_of_qid q))
	  end
      | EFun(i,[],_,_) -> failAt i (fun () -> sprintf "Fatal error: zero argument function")
	  
      | EFun(i,[p],return_sort_opt,body) ->
	  (* fully-annotated, simple lambda *)
	  let param_sort = sort_of_param p in
	  let param_id = id_of_param p in	  
	  let body_sev = (* check body in an env where param has its sort *)
	    clear_rec_vars
	      (update_sev sev
		 (qid_of_id param_id)
		 param_sort)
	  in	    
	  let body_sort, new_body = match return_sort_opt with
	      None -> check_exp body_sev body
	    | Some return_sort -> 
		return_sort, 
		expect_sort_exp "function body" body_sev return_sort body
	  in
	  let fun_sort = SArrow(i,param_sort, body_sort) in
	  let new_e0 = EFun(i,[p], Some body_sort, new_body) in
	    fun_sort, new_e0
      | EFun(i,p1::p2::ps,return_sort_opt,body) ->
	  (* multi-parameter function; rewrite to simple lambda *)
	  let new_body = EFun(i,p2::ps,return_sort_opt,body) in
	    (* FIXME: do this better... it's going to re-flatten f many times *)
	    check_exp sev (EFun(i,[p1],None,new_body))	    
      | EMap(i,ms) ->	
	  let map_sort = SArrow(i, SName(i), SLens(i)) in
	  (* check that each element of ms is a name * lens pair *)
	  let new_ms = Safelist.map
	    (fun (i,n,l) -> 
	       let new_n = expect_sort_exp "map name" sev (SName(i)) n in
	       let new_l = expect_sort_exp "map lens" (clear_rec_vars sev) (SLens(i)) l in
		 (i, new_n, new_l))
	    ms
	  in 
	  let new_e0 = EMap(i, new_ms) in
	    map_sort, new_e0
	      
      | EApp(i,e1,e2) ->
	  begin
	    let e1_sort,new_e1 = check_exp sev e1 in
	      match e1_sort with
		  SArrow(_,param_sort,return_sort) ->
		    let new_e2 = 
		      expect_sort_exp "right-hand side of application" sev param_sort e2 
		    in
		    let new_e0 = EApp(i, new_e1, new_e2) in
		      return_sort, new_e0
		| _ -> 
		    failAt i 
		      (fun () -> sprintf 
			 "Sort checking error in left-hand side of application: expected function but found %s"		       
			 (string_of_sort e1_sort))
	  end		  	    
      | ELet(i,bs,e) ->
	  let old_rs = get_rec_vars sev in
	  let bsev,_,new_bs = check_bindings sev bs in
	  let bsev1 = set_rec_vars bsev old_rs in
	  let e_sort, new_e = check_exp bsev1 e in
	  let new_e0 = ELet(i, new_bs, new_e) in
	    e_sort, new_e0
	      
      | EName(i,x) -> (SName(i)), e0
	    
      | EType(i,t) ->
	  let t_sort, new_t = check_typeexp sev t in
	  let t_ok, actual_t_sort = 
	    check_sort (SType(i)) t_sort
	  in
	  let _ = if (not t_ok) then 
	    failAt (info_of_typeexp t)
	      (fun () -> 
		 sprintf "Sort checking error in type expression: expected type but found %s"
		   (string_of_sort actual_t_sort))
	  in
	  let new_e0 = EType(i, new_t) in
	    t_sort, new_e0
	    
      | EView(i,ks) ->
	  let new_ks = Safelist.map
	    (fun (i,ne,ve) -> 
	       let new_ne = expect_sort_exp "view expression" sev (SName(i)) ne in
	       (* ve can either be a view, or a name, which is shorthand for n={} *)
	       let ve_sort, new_ve0 = check_exp sev ve in 
	       let ve_i = info_of_exp new_ve0 in
	       let new_ve = match ve_sort with		   
		   SName(_) -> EView(ve_i, [(ve_i, new_ve0, emptyView ve_i)])
		 | SView(_) -> new_ve0
		 | s -> failAt ve_i 
		     (fun () -> 
			sprintf "Sort checking error in view binding: expected name or view but found %s"
			  (string_of_sort s))
	       in
		 (i, new_ne, new_ve))
	    ks
	  in		      
	  let new_e0 = EView(i, new_ks) in
	    (SView(i)), new_e0
	      
      | EConsView(i,e1,e2) ->
	  let e1_sort, new_e10 = check_exp sev e1 in 
	  let e1_i = info_of_exp new_e10 in
	  let new_e1 = match e1_sort with		   
	      SName(_) -> EView(e1_i, [(e1_i, new_e10, emptyView e1_i)])
	    | SView(_) -> new_e10
	    | s -> failAt e1_i 
		(fun () -> 
		   sprintf "Sort checking error in list view binding: expected name or view but found %s"
		     (string_of_sort s))
	  in
	  let new_e2 = expect_sort_exp "cons list view expression" sev (SView(i)) e2 in
	  let new_e0 = EConsView(i, new_e1, new_e2) in
	    (SView(i)), new_e0
	      
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
    TEmpty(i) -> (SType (i), pt0)
  | TAny(i) -> (SType(i), pt0)
  | TVar(i,q) -> 
      begin
	(* look up in the environment; check that recursive uses OK *)
	match (rec_var_ok sev q, lookup_sev sev q) with
	    (true, Some s) -> s, pt0
	  | (false, Some _)  -> 
	      failAt i 
		(fun () -> 
		   sprintf "Sort checking error at type variable: %s may not be used recursively" 
		     (string_of_qid q))
	  | (_,None) -> 
	      failAt i 
		(fun () -> 
		   sprintf "Sort checking error at type varibale: %s is not bound"
		     (string_of_qid q))
      end
  | TApp(i,pt1,pt2) ->
      begin
	let pt1_sort, new_pt1 = check_ptypeexp sev pt1 in
	  match pt1_sort with
	      STOper(_,param_sort,return_sort) ->
		let new_pt2 = 
		  expect_sort_ptypeexp 
		    "right hand side of type application" 
		    sev param_sort pt2
		in
		let new_pt0 = TApp(i,new_pt1, new_pt2) in
		  return_sort, new_pt0
	    | pt1_sort -> 
		failAt i 
		  (fun () -> 
		     sprintf "Sort checking error in right-hand side of type application: expected type operator expected but found %s"
		       (string_of_sort pt1_sort))
      end

  | TFun(i,xs,pt_sort,pt) -> 
      begin
	match xs with 
	    [] -> failAt i (fun () -> "Fatal error: zero-argument type function")
	  | [x] ->
	      let i = info_of_id x in
	      let pt_sev = clear_rec_vars (update_sev sev (qid_of_id x) (SType(i))) in	
	      let new_pt = 
		expect_sort_ptypeexp "body of type operator" pt_sev pt_sort pt
	      in
	      let tfun_sort = STOper(i,SType(i),pt_sort) in
	      let new_pt0 = TFun(i,xs,pt_sort,new_pt) in
		tfun_sort, new_pt0
	  | (x::xrest) ->
	      let pt_body = TFun(i, xrest, pt_sort, pt) in
                check_ptypeexp sev (TFun(i,[x],STOper(i, SType(i), pt_sort),pt_body))
      end	  
        
  | TName(i,ne,pt) ->      
      let new_ne = expect_sort_exp "atomic name type" 
	sev (SName(i)) ne 
      in
      let new_pt = expect_sort_ptypeexp "atomic named type" 
	(clear_rec_vars sev) (SType(i)) pt 
      in
      let new_pt0 = TName(i, new_ne, new_pt) in
	SType(i), new_pt0
	  
  | TBang(i,excls,pt) ->
      let new_excls = Safelist.map 
	(fun exi -> expect_sort_exp "exception list" sev (SName(i)) exi) excls
      in
      let new_pt = expect_sort_ptypeexp "atomic !-type" 
	(clear_rec_vars sev) (SType(i)) pt 
      in
      let new_pt0 = TBang(i,new_excls, new_pt) in
	SType(i), new_pt0
  	  
  | TStar(i,excls,pt) ->
      let new_excls = Safelist.map 
	(fun exi -> expect_sort_exp "exception list" sev (SName(i)) exi) excls
      in
      let new_pt = expect_sort_ptypeexp "atomic !-type" 
	(clear_rec_vars sev) (SType(i)) pt
      in
      let new_pt0 = TStar(i, new_excls, new_pt) in
	SType(i), new_pt0
	  
  | TCat(i,pts) ->
      let new_pts = Safelist.map
	(fun pti -> 
	   let i = info_of_ptypeexp pti in
	     expect_sort_ptypeexp "concatenated type" sev (SType(i)) pti)
	pts
      in
      let new_pt0 = TCat(i, new_pts) in
	SType(i), new_pt0
	  
  | TUnion(i,pts) ->
      let new_pts = Safelist.map
	(fun pti -> 
	   let i = info_of_ptypeexp pti in
	     expect_sort_ptypeexp "union type" sev (SType(i)) pti)
	pts
      in
      let new_pt0 = TUnion(i, new_pts) in
	SType(i), new_pt0
	    	
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
		   add_rec_var f_qid (update_sev bsev f_qid b_sort)
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
	    (update_sev bsev f_qid e_sort, f_qid, new_bi)
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
       let tb_sort = oper_of_xs xs i in
       let new_sev = add_rec_var x_qid (update_sev tbsev x_qid tb_sort) in
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
		TT pt -> TT (TFun(i,xs,SType(i),pt))
	      | NT pt -> TT (TFun(i,xs,SType(i),pt))
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
let rec check_decl sev di = 
  (* let _ = debug_decl (sprintf "checking declaration %s\n" (string_of_decl di)) in *)
  let old_rec_vars = get_rec_vars sev in
  match di with
    | DLet(i,bs) -> 
	let new_sev, names, new_bs = check_bindings sev bs in
	let new_di = DLet(i, new_bs) in
	  (set_rec_vars new_sev old_rec_vars, names, new_di)

    | DType(i,tbs) ->      
	let new_sev, names, new_tbs = check_typebindings sev tbs in
	let new_di = DType(i, new_tbs) in
	  (set_rec_vars new_sev old_rec_vars, names, new_di)  
    | DMod(i,n,ds) ->
	let n_qid = qid_of_id n in	
	let m_sev,names,new_ds = check_module_aux sev ds in
	let new_sev, names_rev = Safelist.fold_left 
	  (fun (new_sev, names) q -> 
	     match Registry.lookup m_sev ev_of_sev q with
		 None -> failAt i 
		   (fun () -> 
		      sprintf "Fatal error: the impossible happened; a just-compiled declaration, %s, was not found"
			(string_of_qid q))
	       | Some q_rv ->
		   let nq_dot_q = dot n_qid q in
		     (update_sev sev nq_dot_q (sort_of_rv q_rv), nq_dot_q::names))
	  (sev,[])
	  names
	in
	let new_di = DMod(i,n,new_ds) in
	  (set_rec_vars new_sev old_rec_vars, Safelist.rev names_rev, new_di)
	    
(* type check a module *)
and check_module_aux sev ds = 
  let new_sev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sev, names, new_ds_rev) di ->
	 let new_sev, new_names, new_di = check_decl sev di in
	   new_sev, names@new_names, new_di::new_ds_rev)
      (sev,[],[])
      ds
  in
    (new_sev, names, Safelist.rev new_ds_rev)
      
let check_module m0 = 
  (* let _ = debug (sprintf "checking module %s" (string_of_module m0)) in *)
  let (Syntax.MDef(i,m,nctx,ds)) = m0 in
  let sev = set_sev_ctx (empty_sev ()) (nctx@Registry.pre_ctx) in
  let _,_,new_ds = check_module_aux sev ds in 
  let new_m0 = Syntax.MDef(i,m,nctx,new_ds) in
    new_m0

(***
 ***
 *** COMPILER
 ***
 ***)

(* EXPRESSIONS *)
let rec compile_exp cev e0 =
  (* let _ = debug (sprintf "compiling expression %s" (string_of_exp e0)) in *)
    match e0 with
	EVar(i,q) ->
	  begin
	    (* look up in the environment *)
	    match lookup_cev cev q with
		Some rv -> rv
	      | None -> failAt i
		  (fun () -> 
		     sprintf "Run error at variable: %s is not bound"
		       (string_of_qid q))
	  end
	  	  
      | EFun(i,[p],Some ret_sort,e) ->
	  (* fully-annotated, simple lambda *)
	  (* the actual implementation of f *)
	  let param_qid = qid_of_id (id_of_param p) in
	  let param_sort = sort_of_param p in
	  let f_impl v =
	    let body_cev = update_cev cev param_qid (make_rv param_sort v) in
	      value_of_rv (compile_exp body_cev e)
	  in
	  let fun_sort = SArrow(i,param_sort, ret_sort) in
	  let fun_value = Value.F(f_impl) in
	    make_rv fun_sort fun_value
      | EFun(i,_,Some _, _) -> failAt i (fun () -> "Run error: unflattened function")
      | EFun(i,_,_, _) -> failAt i (fun () -> "Run error: unannotated function")
      | EMap(i,ms) ->
	  let id_exp = EVar(i, Registry.parse_qid "Native.id") in
	  (* maps are implemented as functions from names -> lenses *)
	  let map_impl = Safelist.fold_left
	    (fun f (i,n,l) ->
	       let n2 = compile_exp_name cev n in
		 fun v -> match v with
		     Value.N n1 ->
		       if (id_equal n1 n2) then value_of_rv (compile_exp cev l)
		       else f v
		   | _ -> failAt i (fun () -> "Run error: argument to map is not a name"))
	    (fun _ -> value_of_rv (compile_exp cev id_exp))
	    ms
	  in	    
	  let map_sort = SArrow(i, SName(i), SLens(i)) in
	  let map_value = Value.F(map_impl) in
	    make_rv map_sort map_value	    
    | EApp(i,e1,e2) ->
	begin
	  let e1_rv = compile_exp cev e1 in
	    match sort_of_rv e1_rv, value_of_rv e1_rv with
	      | SArrow(_,_,return_sort), Value.F f ->
		  let e2_rv = compile_exp cev e2 in
		    make_rv return_sort (f (value_of_rv e2_rv))
	      | _ -> failAt i 
		  (fun () -> sprintf "Run error: function expected at left-hand side of application but %s found"
		     (string_of_sort (sort_of_rv e1_rv)))
	end
    | ELet(i,bs,e) ->
	let bcev,_ = compile_bindings cev bs in
	  compile_exp bcev e	    
    | EName(i,x) -> make_rv (SName(i)) (Value.N (name_of_id x))	  
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
		  | Some _ -> failAt i (fun () -> sprintf "Run error: name %s is repeated in view" n)
	in
	let view_value = (Value.V (compile_viewbinds cev ks)) in
	  make_rv (SView(i)) view_value
	    
    | EConsView(i,e1,e2) ->
	let v1 = compile_exp_view cev e1 in
	let v2 = compile_exp_view cev e2 in
	let v = V.cons v1 v2 in
	let cons_value = Value. V (v) in
	  make_rv (SView(i)) cons_value
	    
and compile_exp_name cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    match value_of_rv (e_rv) with
      | Value.N n -> n
      | _ -> failAt i (fun () -> sprintf "Run error: name expected but %s found" 
			 (string_of_sort (sort_of_rv e_rv)))

and compile_exp_view cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    match value_of_rv (e_rv) with
      | Value.V v -> v
      | _ -> failAt i (fun () -> sprintf "Run error: view expected but %s found" 
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
	    match lookup_cev cev q with
		Some rv -> 
		  begin
		    match value_of_rv rv with 
			Value.T (Type.TT pt) -> pt
		      | _ -> failAt i 
			  (fun () -> sprintf "Run error: type variable %s is not bound to a non-negative type" 
			     (string_of_qid q))
		  end
	      | None -> failAt i
		  (fun () -> sprintf "Run error: unbound type variable %s" 
		     (string_of_qid q))
	  end
      | TApp(i,pt1,pt2) ->
	  begin
	    match compile_ptypeexp cev pt1 with
		STOper(_),ptv1 -> 
		  let f = match force_it ptv1 with
		      Type.Fun(_,f) -> f
		    | _        -> failAt (Type.info_of_it ptv1)
			(fun () -> sprintf "Fatal error: type operator expected at left-hand side of type application")
		  in
		  let _,ptv2 = compile_ptypeexp cev pt2 in		      
		    (Type.mk_ptype (f ptv2)) (* FIXME: hack! *)
	      | s,_         -> failAt i
		  (fun () -> sprintf "Run error: type operator expected at left-hand side of type application but %s found in %s"
		     (string_of_sort s) (string_of_ptypeexp pt))
	  end
      | _ -> assert false
  in match pt0 with
      TEmpty(i) -> (SType(i), Type.Empty(i))
    | TAny(i) -> (SType(i), Type.Any(i))
    | TVar(i,q) ->
	begin
	  (* look up in the environment; check that recursive uses OK *)
	  match (lookup_cev cev q) with
	      Some rv ->
		let tsort = sort_of_rv rv in
		let tval = Type.Var (i,q,ptype2thunk cev pt0) in
		  tsort, tval
	    | None ->
		failAt i
		  (fun () -> sprintf "Run error: unbound type variable %s" (string_of_qid q))
	end
    | TApp(i,pt1,pt2) ->
	begin
	  match compile_ptypeexp cev pt1 with
	      STOper(_,_,rs), pt1_val ->
		let _, pt2_val = compile_ptypeexp cev pt2 in
		  rs, Type.App(i, pt1_val, pt2_val, ptype2thunk cev pt0)
	    | s, _ ->
		failAt i
		  (fun () -> sprintf "Run error: type operator expected at left-hand side of type application but %s found in %s"
		     (string_of_sort s) (string_of_ptypeexp pt0))
	end
	  
    | TFun(i,[x],f_sort,pt) ->
	begin
	  let x_qid = qid_of_id x in
	  let f_impl it_arg =
	    let x_rv = make_rv (SType(i)) (Value.T(Type.TT(Type.mk_ptype it_arg))) in
	    let body_cev = update_cev cev x_qid x_rv in
	    let _,body_val = compile_ptypeexp body_cev pt in
	      body_val
	  in
	  let fun_sort = STOper(i,SType(i),f_sort) in
	  let fun_value = Type.Fun(i,f_impl) in
	    fun_sort, fun_value
	end
    | TFun(i,_,_,_) -> failAt i (fun () -> "Run error: unflattened type function")
        
    | TName(i,e,pt) ->
	let n = compile_exp_name cev e in
	let _, t_val = compile_ptypeexp cev pt in
      	  SType(i), (Type.Name(i,n,Type.mk_ptype t_val))
	    
    | TBang(i,excl,pt) ->
	let excl_ns = List.map (compile_exp_name cev) excl in
	let _, t_val = compile_ptypeexp cev pt in 
	  SType(i), (Type.Bang(i,excl_ns,Type.mk_ptype t_val))
	    
    | TStar(i,excl,pt) ->
	let excl_ns = List.map (compile_exp_name cev) excl in
	let _, t_val = compile_ptypeexp cev pt in 
	  SType(i), (Type.Star(i,excl_ns,Type.mk_ptype t_val))
    | TCat(i,pts) ->
	let pt_vals = Safelist.map
	  (fun pti -> let _, ti_val = compile_ptypeexp cev pti in
	     Type.mk_ptype ti_val)
	  pts
	in
	  SType(i), (Type.Cat(i,pt_vals))
	    
    | TUnion(i,pts) ->
	let pt_vals = Safelist.map
	  (fun pti -> let _, ti_val = compile_ptypeexp cev pti in
	     Type.mk_ptype ti_val)
	  pts
	in
	  SType(i), (Type.Union(i,pt_vals))
	      
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
		   update_cev bcev f_qid (make_rv b_sort dummy)
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
	  (f_qid, overwrite_cev bcev f_qid memoized_rv)
      | Syntax.BDef(i,_,_,_,_) -> failAt i (fun () -> "Run error: unflattened binding")
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
       let b_sort = oper_of_xs xs i in
       let dummy = Value.dummy ~msg:(sprintf "DUMMY for %s" (string_of_typebinding ti)) b_sort in
       	 update_cev tbcev x_qid (make_rv b_sort dummy))
    cev
    tbs
  in
  let rec compile_typebinding cev tbi =
    (* let _ = debug (sprintf "compiling typebinding %s\n" (string_of_typebinding tbi)) in *)
      match tbi with
	  (x,[],t) ->
	    let x_qid = qid_of_id x in
	    let t_rv = compile_typeexp cev t in
	      (x_qid, overwrite_cev cev x_qid t_rv)
	| (x,_,_) -> failAt (info_of_id x) (fun () -> "Run error: unflattened type binding")
  in
  let tbcev,names_rev = Safelist.fold_left
    (fun (tbcev,names_rev) tbi ->
       let x_qid, tbcev = compile_typebinding tbcev tbi in
	 tbcev, x_qid::names_rev)
    (tbcev,[])
    tbs
  in
    tbcev, Safelist.rev names_rev

(* type check a single declaration *)
let rec compile_decl cev di = 
  (* let _ = debug_decl (sprintf "compiling declaration %s\n" (string_of_decl di)) in *)
  match di with
  | DLet(i,bs) -> compile_bindings cev bs 
  | DType(i,tbs) -> compile_typebindings cev tbs
  | DMod(i,n,ds) ->
      let nq = qid_of_id n in
      let m_cev, names = compile_module_aux cev ds in
      (* insert all the just-compiled names into the environment *)
      let new_cev, names_rev =
	Safelist.fold_left
	  (fun (cev, names) q ->
	     match Registry.lookup m_cev ev_of_cev q with
		 Some rv ->
		   let nq_dot_q = dot nq q in
		     (update_cev cev nq_dot_q rv, nq_dot_q::names)
	       | None -> failAt i
		   (fun () -> sprintf "Fatal error: the impossible happened; a just-compiled declaration, %s, was not found"
		      (string_of_qid q)))
	  (cev,[])
	  names
      in
	new_cev, Safelist.rev names_rev
	  
and compile_module_aux cev ds = Safelist.fold_left
  (fun ((cev, names):compile_env * qid list) (di:decl) ->
     let new_cev, new_names = compile_decl cev di in
       new_cev, names@new_names)
  (cev,[])
  ds

let compile_module m0 =
  (* let _ = debug (sprintf "compiling module %s" (string_of_module m0)) in *)
  let (Syntax.MDef(i,m,nctx,ds)) = m0 in
  let cev = set_cev_ctx (empty_cev ()) (nctx@Registry.pre_ctx) in
  let new_cev,_ = compile_module_aux cev ds in
    Registry.register_env (ev_of_cev new_cev) (Syntax.qid_of_id m)
	      
let compile_file fn n =
  (* let _ = debug (sprintf "compiling file %s" fn) in *)
  let old_file_name = !file_name in
  let _ = file_name := fn in
  let fchan = open_in fn in
  let lexbuf = Lexing.from_channel fchan in
  let _ = Lexer.reset () in
  let ast =
    try Parser.modl Lexer.token lexbuf
    with Parsing.Parse_error ->
      failAt (Lexer.info lexbuf) (fun () -> "Syntax error")
  in
  (* check that module is in correct file *)
  let m = id_of_modl ast in
  let n_str = string_of_id n in
  let m_str = string_of_id m in
  let _ = if (n_str <> m_str) then
    failAt (info_of_module ast)
      (fun () -> sprintf "Module %s must appear in a file named %s.fcl"
	 m_str (String.uncapitalize m_str))
  in
  let ast = check_module ast in
  let _ = compile_module ast in
  let _ = file_name := old_file_name in
    ()
      
let _ = Registry.compile_file_impl := compile_file
