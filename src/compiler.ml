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
let mk_rv = Registry.make_rv
let s_of_rv = Registry.sort_of_rv
let v_of_rv = Registry.value_of_rv

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
    (fun r qs -> r or (Syntax.qid_prefix (Value.parse_qid qs) m))
    (Prefs.read test_all)
    (Prefs.read tests)
    
(* --------------- Error Reporting --------------- *)
let debug s_thk = 
  Trace.debug "compiler" (fun () -> Printf.eprintf "%s\n%!" (s_thk ()))

let parse_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "%s: Parse error @\n %s" (Info.string_of_t i) (msg_thk ())))
      
let sort_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "%s: Sort checking error @\n %s" (Info.string_of_t i) (msg_thk ())))
    
let test_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "%s: Unit test failed @ " (Info.string_of_t i); (msg_thk ())))
    
let run_error i msg_thk = 
  raise (Error.Harmony_error
	   (fun () -> Format.printf "%s: Unexpected run-time error @\n %s" (Info.string_of_t i) (msg_thk ())))
          
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
	| Some rv -> Some (s_of_rv rv)
	      
  (* update a cev with a new sort *)
  let update sev q s = 
    let (rs,cev) = sev in
      (rs, (CEnv.update cev q (mk_rv s (Value.dummy s))))
	
  let to_string sev = 
    let (rs,cev) = sev in 
      sprintf "{compile_env=%s, recursive_vars={%s}}"
	(CEnv.to_string cev)      	
	(Misc.concat_list ", " (Safelist.map string_of_qid rs))
end

(* --------------- Sort checker --------------- *)

(* utility functions *)
(* [ends_with_type s] is [true] iff [s] is [type] or is of the form
   [s_1 -> ... s_n -> type] *)
let rec ends_with_type = function
    (* N.B.: a tree is a value, so we don't need to delay computations
       involving trees *)
    SSchema       -> true
  | SArrow(_,s) -> ends_with_type s
  | _           -> false

(* convert a list of parameters ps with sorts s1,..sn and a sort s 
 * to arrow sort s1 -> ... -> sn -> s *)
let sort_of_param_list i ps s = 
  Safelist.fold_right (fun p ts -> SArrow(sort_of_param p,ts))  ps s

(* check if one sort is a subsort of another *)
let rec subsort u v = match u,v with	
    SArrow(s11,s12),SArrow(s21,s22) -> 
      let b1 = subsort s21 s11  in
      let b2 = subsort s12 s22 in 
	b1 && b2
  | STree,SSchema  -> true
  | _ when u = v -> true
  | _            -> false

let rec expect_sort_exp msg sev expected_sort e =
  let i = info_of_exp e in
  let e_sort, new_e = check_exp sev e in
    if subsort e_sort expected_sort then 
      (e_sort,new_e)
    else
      sort_error i
	(fun () -> 
	   sprintf "in %s: %s expected but %s found\n\t%s"
	     msg
	     (string_of_sort expected_sort)
	     (string_of_sort e_sort)
	     (string_of_exp e))
	    
(* EXPRESSIONS *)    
and check_exp sev e0 = 
  (* let _ = debug (sprintf "checking expression %s" (string_of_exp e0)) in *)
  match e0 with
      (* applications *)
      EApp(i,e1,e2) -> begin
	match check_exp sev e1 with
	    SArrow(param_sort,return_sort),new_e1 -> 
	      let e2_sort,new_e2 = expect_sort_exp "application" sev param_sort e2 in
	      let new_e0 = EApp(i, new_e1, new_e2) in 
		(return_sort, new_e0)
	  | e1_sort,_ -> 
	      sort_error i 
		(fun () -> sprintf 
		   "expected function type in application but found %s"
		   (string_of_sort e1_sort))
	end
	  
    (* type atoms *)
    | EAtom(i,e1,e2) ->
	let _,new_e1 = expect_sort_exp "atom" sev SName e1 in
	let e2_sort, new_e2 = expect_sort_exp "atom" sev SSchema e2 in
	let new_e0 = EAtom(i, new_e1, new_e2) in
	  (e2_sort, new_e0)
	    
    | EBang(i,es,e) ->
	let new_es = Safelist.map 
	  (fun ei -> snd (expect_sort_exp "exception list" sev SName ei))
	  es 
	in
	let _,new_e = expect_sort_exp "!-type" sev SSchema e in
	let new_e0 = EBang(i,new_es,new_e) in
	  (SSchema, new_e0)

    | ECat(i,es) ->
	let e0_sort,new_es_rev = Safelist.fold_left
	  (fun (es_sort,new_es_rev) ei -> 
	     let ei_sort, new_ei = expect_sort_exp "concatenation" sev SSchema ei in
	     let new_sort = match ei_sort,es_sort with
		 STree,STree -> STree
	       | _           -> SSchema in
	       (new_sort,new_ei::new_es_rev))
	  (STree,[])
	  es in
	let new_e0 = ECat(i, Safelist.rev new_es_rev) in
	  (e0_sort, new_e0)
	    
    | ECons(i,e1,e2) -> 
	let e1_sort, new_e1 = expect_sort_exp "cons" sev SSchema e1 in
	let e2_sort, new_e2 = expect_sort_exp "cons" sev SSchema e2 in
	let e0_sort = 
	  match e1_sort,e2_sort with
	      STree,STree -> STree
	    | _           -> SSchema in
	let new_e0 = ECons(i, new_e1, new_e2) in
	  (e0_sort, new_e0)
	    
    | EFun(i,[],_,_) -> 
	run_error i (fun () -> sprintf "zero argument function")
	  
    | EFun(i,p1::p2::ps,return_sort_opt,body) ->
	(* multi-parameter function; rewrite to simple lambda *)
	let new_body = EFun(i,p2::ps,return_sort_opt,body) in	    
	  check_exp sev (EFun(i,[p1],None,new_body))	    
	    
    | EFun(i,[p],return_sort_opt,body) ->
	(* simple lambda *)
	let param_sort = sort_of_param p in
	let param_id = id_of_param p in	  
	let body_sev = 
	  (* check body in an env where param has its sort *)
	  SCEnv.clear_rec_vars
	    (SCEnv.update sev
	       (qid_of_id param_id)
	       param_sort)
	in	    
	let body_sort, new_body = 
	  match return_sort_opt with
	      Some return_sort -> expect_sort_exp "function body" body_sev return_sort body
	    | None             -> check_exp body_sev body
	in
	let e0_sort = SArrow(param_sort, body_sort) in
	let new_e0 = EFun(i,[p], Some body_sort, new_body) in
	  (e0_sort, new_e0)
	    
    | ELet(i,bs,e) ->
	let old_rs = SCEnv.get_rec_vars sev in
	let bsev,_,new_bs = check_bindings sev bs in
	let bsev1 = SCEnv.set_rec_vars bsev old_rs in
	let e0_sort, new_e = check_exp bsev1 e in
	let new_e0 = ELet(i, new_bs, new_e) in
	  (e0_sort, new_e0)
	    
    | EMap(i,ms) ->	
	(* check that each element of ms is a name * lens pair *)
	let new_ms = Safelist.map
	  (fun (n,l) -> 
	     let _,new_n = expect_sort_exp "map name" sev SName n in
	     let _,new_l = expect_sort_exp "map lens" (SCEnv.clear_rec_vars sev) SLens l in
	       (new_n, new_l))
	  ms
	in
	let e0_sort = SArrow(SName, SLens) in 
	let new_e0 = EMap(i, new_ms) in
	  (e0_sort, new_e0)
	    
    | EName(i,x) -> (SName, EName(i, x))
	
    | ENil(i) -> (STree, ENil(i))

    | EStar(i,es,e) ->
	let new_es = Safelist.map 
	  (fun ei -> snd (expect_sort_exp "exception list" sev SName ei))
	  es 
	in
	let _,new_e = expect_sort_exp "*-type" sev SSchema e in
	let new_e0 = EStar(i,new_es,new_e) in
	  (SSchema, new_e0)

    | EUnion(i,es) ->
	let new_es = Safelist.map
	  (fun ei -> 
	     let _,new_ei = 
	       expect_sort_exp "union" sev SSchema ei in
	       new_ei)
	  es in
	let new_e0 = EUnion(i, new_es) in
	  (SSchema, new_e0)
	    
    | EVar(i,q) ->
	begin
	  (* look up in the environment; check that recursive uses OK *)
	  match (SCEnv.rec_var_ok sev q, SCEnv.lookup sev q) with
	      (true, Some s)      -> (s,e0)
	    | (false, Some SSchema) -> (SSchema, e0) (* OK, types are lazy *)
	    | (false, Some SLens) -> (SSchema, e0) (* OK, lenses are values *)
	    | (false, Some _)     -> 
		sort_error i 
		  (fun () -> 
		     sprintf  "%s may not be used recursively" (string_of_qid q))
	    | (_, None)           -> 
		sort_error i 
		  (fun () -> sprintf "%s is not bound" 
		     (string_of_qid q))
	end
	  
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
	      | Some e_sort -> expect_sort_exp "let binding" sev e_sort e
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
            
(* type check a single declaration *)
let rec check_decl sev m di = 
  (* let _ = debug_decl (sprintf "checking declaration %s\n" (string_of_decl di)) in *)
  let old_rec_vars = SCEnv.get_rec_vars sev in
  match di with
    | DLet(i,bs) -> 
	let new_sev, names, new_bs = check_bindings sev bs in
	let new_di = DLet(i, new_bs) in
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
		      sprintf "a just-checked declaration of %s went missing"
			(string_of_qid q))
	       | Some q_rv ->
		   let nq_dot_q = dot n_qid q in
		     (SCEnv.update sev nq_dot_q (s_of_rv q_rv), nq_dot_q::names))
	  (sev,[])
	  names
	in
	let new_di = DMod(i,n,new_ds) in
	  (SCEnv.set_rec_vars new_sev old_rec_vars, Safelist.rev names_rev, new_di)
    | DTest(i,e,reso) ->	
	if not (check_test m) then (sev, [], di)
	else
	  begin
	    let _,new_e = expect_sort_exp "test expression" sev STree e in
	    let new_reso = 
	      match reso with 
		  None     -> None 
		| Some res -> 
		    let _,new_res = expect_sort_exp "test result" sev STree res in
		      Some(new_res)
	    in
	    let new_di = DTest(i,new_e, new_reso) in
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
  (* force evaluation of type applications, variables *)
  let rec force v = match v with
      Value.T (Value.App(_,_,_,thk)) -> force (thk ())
    | Value.T (Value.Var(_,_,thk))   -> force (thk ())
    | _                              -> v    
  in
    (* let _ = debug (sprintf "compiling expression %s" (string_of_exp e0)) in *)
    match e0 with
      | EApp(i,e1,e2) -> 	
	  let e1_rv = compile_exp cev e1 in
	  let e2_rv = compile_exp cev e2 in
	  let v1 = v_of_rv e1_rv in
	  let v2 = v_of_rv e2_rv in
	    begin 
	      match s_of_rv e1_rv with
		| SArrow(_,return_sort) -> 
		    if (ends_with_type return_sort) then 
		      let thunk () = 
			let f = match force v1 with
			    Value.F(_,f) -> f
			  | _   -> run_error i 
			      (fun () -> "expected function at left-hand side of application")
			in
			  f v2
		      in
			mk_rv return_sort (Value.T(Value.App(i,v1,v2,thunk)))
		    else
		      begin match v1 with
			  Value.F(_,f) -> mk_rv return_sort (f v2)
			| _   -> run_error i (fun () -> "expected function at left-hand side of application")
		      end
		| _ -> run_error i 
		    (fun () -> sprintf "expected function sort but found %s at left-hand side of application"
		       (string_of_sort (s_of_rv e1_rv)))
	    end

      | EAtom(i,e1,e2) ->
	  let n = compile_exp_name cev e1 in
	  let e2_rv = compile_exp cev e2 in 
	    begin match v_of_rv e2_rv with
		Value.V v -> 
		  mk_rv
		    STree
		    (Value.V (V.set V.empty n (Some v)))
	      | Value.T t ->
		  mk_rv 
		    SSchema
		    (Value.T (Value.Atom(i,n,t)))
	      | _ -> assert false
	    end
		    
      | EBang(i,es,e) ->
	  let ns = Safelist.map (compile_exp_name cev) es in
	  let t = compile_exp_type cev e in 
	    mk_rv 
	      SSchema
	      (Value.T (Value.Bang(i,ns,t)))

      | ECat(i, es) ->
	  let e0_sort, vs_rev = 
	    Safelist.fold_left 
	      (fun (s,vs_rev) ei -> 		 
		 let v = v_of_rv (compile_exp cev ei) in
		   match s, v with
		       STree, (Value.V _) -> 
			 (s, v::vs_rev)
		     | STree, (Value.T _) ->
			 (SSchema, 
			  v::(Safelist.map (fun v -> Value.T (Value.get_type i v)) vs_rev))
		     | SSchema, (Value.V _) 
		     | SSchema, (Value.T _) ->
			 (s, (Value.T (Value.get_type i v))::vs_rev)			 
		     | _ -> run_error i (fun () -> 
					   sprintf "bogus value in ECat: %s %s"
					     (string_of_sort s) (Value.string_of_t v)))
	      (STree,[])
	      es
	  in 
	    begin
	      match e0_sort with
		  STree -> 
		    let vi = Safelist.fold_left
		      (fun vacc v -> V.concat vacc (Value.get_tree i v))
		      V.empty 
		      (Safelist.rev vs_rev)
		    in
		      mk_rv STree (Value.V(vi))		      
		| SSchema -> 
		    let ts = Safelist.rev_map (Value.get_type i) vs_rev in
		      mk_rv SSchema (Value.T (Value.Cat(i,ts)))
		| _ -> assert false
	    end			
      | ECons(i,e1,e2) -> 
	  let e1_v = v_of_rv (compile_exp cev e1) in 
	  let e2_v = v_of_rv (compile_exp cev e2) in    
	    begin match e1_v, e2_v with
		Value.V v1, Value.V v2     -> 
		  mk_rv STree (Value.V (V.cons v1 v2))
	      | Value.V _, Value.T t ->
	      	  mk_rv 
		    SSchema 
		    (Value.T (Type.mk_cons i 
				(Value.get_type i e1_v)
				t))
	      | Value.T t, Value.V _ ->
	      	  mk_rv 
		    SSchema 
		    (Value.T (Type.mk_cons i 
				t
				(Value.get_type i e2_v)))
	      | Value.T t1, Value.T t2 -> 
		  mk_rv 
		    SSchema
		    (Value.T (Type.mk_cons i t1 t2))
	      | _ -> run_error i (fun () -> "expected tree or type in atom")
	    end

      | EFun(i,[p],Some ret_sort,e) ->
	  (* fully-annotated, simple lambda *)
	  (* the actual implementation of f *)
	  let param_qid = qid_of_id (id_of_param p) in
	  let param_sort = sort_of_param p in
	  let f_sort = SArrow(param_sort, ret_sort) in
	  let f_impl v =
	    let body_cev = 
	      CEnv.update cev param_qid (mk_rv param_sort v) in
	      v_of_rv (compile_exp body_cev e)
	  in
	    mk_rv 
	      f_sort
	      (Value.F (f_sort, f_impl))

      | EFun(i,_,_,_) -> 
	  run_error i (fun () -> sprintf "unflattened or unannotated function")
	    
      | ELet(i, bs,e) ->
	  let bcev,_ = compile_bindings cev bs in
	    compile_exp bcev e	    
	      
      (* maps are implemented as functions from names -> lenses *)
      | EMap(i, ms) ->
	  let id_exp = EVar(i, Value.parse_qid "Native.Prelude.id") in	    	  	    
	  let map_impl = Safelist.fold_left
	    (fun f (ne,le) ->
	       let n = compile_exp_name cev ne in
		 begin
		   function (Value.N n1 as v) ->
		     if (n1 = n) then v_of_rv (compile_exp cev le)
		     else f v
		     | _ -> run_error i (fun () -> "argument to map is not a name")
		 end)
	    (fun _ -> v_of_rv (compile_exp cev id_exp))
	    ms
	  in	    
	  let map_sort = SArrow(SName, SLens) in
	  let map_value = Value.F(map_sort, map_impl) in
	    mk_rv 
	      map_sort
	      map_value

      | EName(i, x) -> 
	  mk_rv 
	    SName 
	    (Value.N (name_of_id x))
	    
      | ENil(i) -> 
	  mk_rv
	    STree
	    (Value.V (V.empty_list))

      | EStar(i,es,e) ->
	  let ns = Safelist.map (compile_exp_name cev) es in
	  let t = compile_exp_type cev e in
      	    mk_rv 
	      SSchema
	      (Value.T (Value.Star(i,ns,t)))
	      
      | EUnion(i, es) ->
	  let ts = Safelist.map (fun ei -> compile_exp_type cev ei) es in	  
	    mk_rv 
	      SSchema
	      (Value.T (Value.Union(i,ts)))
	      
      | EVar(i,q) -> 
	  let rv = match CEnv.lookup cev q with
	      Some rv -> rv 
	    | None -> run_error 
		(info_of_exp e0)
		  (fun () -> 
		     Printf.sprintf "Cannot compile expression %s"
		       (string_of_exp e0)) in
	    match s_of_rv rv with
		(* lenses *)
		SLens -> 
		  let fetch_lens () = 			     
		    match CEnv.lookup cev q with 
			Some rv -> Value.get_lens i (v_of_rv rv)
		      | None -> run_error i 
			  (fun () -> sprintf "lens %s went missing" (* "can't" happen :-) *)
			     (string_of_qid q))
		  in		    
		  let get c = Lens.get (fetch_lens ()) c in
		  let put a co = Lens.put (fetch_lens ()) a co in
		    mk_rv 
		      SLens 
		      (Value.L (Lens.native get put))
		      
	      (* types *)
	      | s when ends_with_type s ->
		  let thunk () = 
		    match CEnv.lookup cev q with
			Some rv -> v_of_rv rv	
		      | None -> run_error i
			  (fun () -> sprintf "%s became unbound" (string_of_qid q))
		  in
		    mk_rv s (Value.T(Value.Var(i,q,thunk)))			
		      
	      (* everything else *)
	      | _ -> rv
		  
(* compilation helpers when we know the sort *)	  	  	    
and compile_exp_name cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_name i (v_of_rv e_rv)
      
and compile_exp_tree cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_tree i (v_of_rv e_rv)
      
and compile_exp_type cev e =
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_type i (v_of_rv e_rv)
	    
and compile_exp_lens cev e = 
  let i = info_of_exp e in
  let e_rv = compile_exp cev e in
    Value.get_lens i (v_of_rv e_rv)

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
		   CEnv.update bcev f_qid (mk_rv b_sort dummy)
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
	let memoized_v = Value.memoize (v_of_rv e_rv) in
	let memoized_rv = mk_rv (s_of_rv e_rv) memoized_v in
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
      
type testresult = OK of V.t | Error of (unit -> unit)

(* type check a single declaration *)
let rec compile_decl cev m di = 
  (* let _ = debug_decl (sprintf "compiling declaration %s\n" (string_of_decl di)) in *)
  match di with
  | DLet(i,bs) -> compile_bindings cev bs 
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
		   (fun () -> sprintf "a just-compiled declaration of %s went missing"
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
	      OK (compile_exp_tree cev e)
	    with (Error.Harmony_error(m)) -> Error m
	  in
	    match vo, reso with 
		Error _, None -> ()
	      | OK v, Some res -> 
		  let resv = compile_exp_tree cev res in
		    if not (V.equal v resv) then
		      test_error i 
			(fun () -> 
			   V.format_msg
			     [`String "expected:"; `Space;
			      `Tree resv;`Space;
			      `String "found:"; `Space;
			      `Tree v])
	      | Error m, Some res -> 
		  let resv = compile_exp_tree cev res in
		    test_error i 
		      (fun () -> 
			 V.format_msg
			   [`String "expected:"; `Space;
 			    `Tree resv; `Space;
			    `String "found error:"; `Space];
			 m ())
	
	      | OK v, None -> 
		  test_error i 
		    (fun () -> 
		       V.format_msg
			 [`String "expected error, found"; `Break;
 			  `Tree v;])
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
	(fun () -> sprintf "module %s must appear in a file named %s.fcl."
	   m_str (String.uncapitalize m_str))
  in
  let ast = check_module ast in 
  let _ = compile_module ast in
    Lexer.finish ()

(* FIXME: ugly backpatch hack! *)
let _ = Registry.compile_file_impl := compile_file
