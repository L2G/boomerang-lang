(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - Focal interpreter                              *)
(*                                                              *)
(* $Id$           *)
(*                                                              *)
(****************************************************************)

open Syntax

let (@) = Safelist.append (* use a stack-safe append *)
  
(* compilation environments comprise:                       
 *  - a qid list: variables that may NOT be used recursively 
 *  - a qid list: the naming context, a list of open modules 
 *  - a Registry.env: mapping from qids to Registry.rv 
 *)
type compile_env = Syntax.qid list * Syntax.qid list * Registry.env
    
(* the empty enviroment*)
let empty = ([], [], Registry.empty)

(* getters/setters for compile_envs *)
let get_ev cev = let (_,_,ev) = cev in ev
let set_ev cev ev = let (rs,os,_) = cev in (rs,os,ev)
let get_ctx cev = let (_,os,_) = cev in os
let set_ctx cev qs = let (rs,os,ev) = cev in (rs,qs@os,ev)

(* helper functions for environments **) 
(* lookup a qid in an sev *)
let make_rv = Registry.make_rv
let sort_of_rv = Registry.sort_of_rv
let value_of_rv = Registry.value_of_rv

let lookup cev q = 
  match (Registry.lookup_library cev get_ev get_ctx q) with
    | None -> None
    | Some rv -> Some rv
	
(* update a cev with a new rv *)
let update cev q rv = 
  Registry.update cev get_ev set_ev q rv

(* overwrite a cev with a new sort *)
let overwrite cev q rv = 
  Registry.overwrite cev get_ev set_ev q rv
    
(* add a qid to the list of not-recursive variables *)
let add_rec_var q cev = let (rs,os,ev) = cev in (q::rs,os,ev)

(* check if a use of a variable is OK *)
let rec_var_ok cev q = let (rs,_,_) = cev in not (Safelist.mem q rs)

(* clear the list of recursive variables (e.g., when we go under a lambda) *)
let clear_rec_vars cev = let (_,os,ev) = cev in ([],os,ev)

(* convert a list of parameters ps with sorts s1,..sn and a sort s 
 * to arrow sort s1 -> ... -> sn -> s *)
let sort_of_param_list i ps s = 
  Safelist.fold_right (fun p ts -> SArrow(i,sort_of_param p,ts))  ps s
  
(* helper function for printing run errors *) 
let sprintf = Printf.sprintf  
let run_error error_string = raise (Error.Run_error error_string)
  
(* check that s matches expected sort es. Print an error message if not *)
let rec expect_sort es s i = 
  match (es,s) with
    | (SName _,SName _) -> es
    | (SLens _,SLens _) -> es
    | (SType _,SType _) -> es
    | (SView _,SView _) -> es
    | (SArrow(_,es1,es2),SArrow(_,s1,s2)) ->
	let _ = expect_sort es1 s1 i in
  	let _ = expect_sort es2 s2 i in
 	  s1
    | (STOper(_,es1,es2),STOper(_,s1,s2)) ->
	let _ = expect_sort es1 s1 i in
  	let _ = expect_sort es2 s2 i in
 	  s1
    | _ -> 
	run_error (sprintf 
		     "The expression at %s has sort %s but is used with sort %s"
		     (Info.string_of_t i)
		     (string_of_sort s)		     
		     (string_of_sort es))

(* EXPRESSIONS *)
let rec compile_exp cev e0 = match e0 with
  | EVar(i,q) ->
      begin
	(* look up in the environment; check that recursive uses OK *)
	match (rec_var_ok cev q, lookup cev q) with
	  | (true, Some rv) -> rv
	  | (false, Some _)  -> 
	      run_error (sprintf 
			   "Variable %s may not be used recursively at %s"
			   (string_of_qid q)			
			   (Info.string_of_t i))
	  | (_, None) ->
	      run_error (sprintf "Unbound variable %s at %s"
			   (string_of_qid q)
			   (Info.string_of_t i))
      end
	
  | EFun(i,[],_,_) -> run_error (sprintf "Zero argument function at %s"
				   (Info.string_of_t i))
  | EFun(i,[p],rso,e) ->
      (* fully-annotated, simple lambda *)
      let param_sort = sort_of_param p in
      let param_id = id_of_param p in
	
      (* compile the body in a dummy environment to check it *)
      let dummy_cev = clear_rec_vars
	(update cev
	   (qid_of_id param_id)
	   (make_rv param_sort (Value.dummy param_sort)))
      in
      let body_rv = compile_exp dummy_cev e in 
      let ret_sort = match rso with 
	  None -> sort_of_rv body_rv
	| Some s -> expect_sort s (sort_of_rv body_rv) (info_of_exp e)
      in
      (* the actual implementation of f *)
      let f_impl v =
	let fcev = clear_rec_vars 
	  (update cev
	     (qid_of_id param_id)
	     (make_rv param_sort v))
	in
	  value_of_rv (compile_exp fcev e)
      in
      let fun_sort = SArrow(i,param_sort, ret_sort) in
      let fun_value = Value.F(f_impl) in
	make_rv fun_sort fun_value
  | EFun(i,p1::p2::ps,so,e) ->
      (* multi-parameter function; rewrite to simple lambda *)
      let body = EFun(i,p2::ps,so,e) in
	(* FIXME: do this better... it's going to re-flatten f many times *)
	compile_exp cev (EFun(i,[p1],None,body))
	  
  | EMap(i,ms) ->
      let id_exp = EVar(i, Registry.parse_qid "Pervasives.Native.id") in	    
      let map_sort = SArrow(i, SName(i), SLens(i)) in
      (* check that each element of ms is a name * lens pair *)
      let _ = Safelist.map
	(fun (i,n,l) ->
	   let n_sort = sort_of_rv (compile_exp cev n) in 
	   let l_sort = sort_of_rv (compile_exp (clear_rec_vars cev) l) in	   
	   let _ = expect_sort (SName(i)) n_sort i in
	   let _ = expect_sort (SLens(i)) l_sort i in
	     ())	
	ms
      in 
      (* maps are implemented as functions from names -> lenses *)
      let map_impl = Safelist.fold_left 
	(fun f (i,n,l) -> 
	   fun v -> 
	     match v with 
		Value.N n1 -> 
		   begin 
		     let v2 = value_of_rv (compile_exp cev n) in
		       match v2 with
			   Value.N n2 -> 
			     if (id_equal n1 n2) then
			       value_of_rv (compile_exp cev l)
			     else
			       f v
			 | _ -> 
			     run_error (sprintf 
					  "Name expected in map at %s" 
					  (Info.string_of_t i))
		   end
	       | _ -> run_error (sprintf "Argument to map is not a name"))
	(fun _ -> value_of_rv (compile_exp cev id_exp))
	ms
      in
      let map_value = Value.F(map_impl) in
	make_rv map_sort map_value
		   
  | EApp(i,e1,e2) ->
      begin
	let rv1 = compile_exp cev e1 in	
	  match value_of_rv rv1, sort_of_rv rv1 with
	    | Value.F f, SArrow(_,ps,rs) ->
		let rv2 = compile_exp cev e2 in
		let _ = expect_sort ps (sort_of_rv rv2) (info_of_exp e2) in
		  make_rv rs (f (value_of_rv rv2))
	    | _ -> 
		run_error 
		  "Function expected at left-hand side of application at %s"
		  (Info.string_of_t i)
      end		  

  | ELet(i,bs,e) ->
      let bcev,_ = compile_bindings cev bs in
	compile_exp bcev e
	  
  | EName(i,x) -> 
      make_rv 
	(SName(i))
	(Value.N (name_of_id x))
	
  | EType(i,t) ->
      let trv = compile_typeexp cev t in
      let _ = expect_sort (SType(i)) (sort_of_rv trv) (info_of_typeexp t) in
	trv
	  
  | EView(i,ks,is_list) ->
      make_rv 
	(SView(i))
	(Value.V (compile_viewbinds cev ks is_list))

(* HELPERS *)
and compile_exp_name cev e = 
  let e_rv = compile_exp cev e in
  let e_sort = sort_of_rv e_rv in
  let e_value = value_of_rv e_rv in
    match e_sort, e_value with 
      | SName(_), Value.N n -> n
      | _   -> run_error 
	  (sprintf "Name expected at %s, found %s"
	     (Info.string_of_t (Syntax.info_of_exp e))
	     (string_of_sort e_sort))

and compile_exp_view cev e = 
  let e_rv = compile_exp cev e in
  let e_sort = sort_of_rv e_rv in
  let e_value = value_of_rv e_rv in
    match e_sort, e_value with 
      | SView(_), Value.V v -> v
      | _   -> run_error 
	  (sprintf "View expected at %s, found %s"
	     (Info.string_of_t (Syntax.info_of_exp e))
	     (string_of_sort e_sort))
	    
(* TYPE EXPRESSIONS *)
and compile_typeexp cev t = match t with
    Syntax.TT pt -> 
      let pt_sort, pt_val = compile_ptypeexp cev pt in
	make_rv pt_sort (Value.T (Type.TT pt_val))
  | Syntax.NT pt -> 
      let pt_sort, pt_val = compile_ptypeexp cev pt in
	make_rv pt_sort (Value.T (Type.NT pt_val))
	  
and compile_ptypeexp cev t = match t with
    TEmpty(i) -> (SType i), Type.Empty
  | TVar(i,q) -> 
      begin
	(* look up in the environment; check that recursive uses OK *)
	match (rec_var_ok cev q, lookup cev q) with
	    (true, Some rv) -> 
	      let tsort = sort_of_rv rv in
	      let tval = Type.Var (q,(get_ctx cev)) in
		tsort, tval
		  
	  | (false, Some _)  -> 
	      run_error (sprintf 
			   "Type variable %s may not be used recursively at %s"
			   (string_of_qid q)			
			   (Info.string_of_t i))
	  | (_, None) ->
	      run_error (sprintf "Unbound type variable %s at %s"
			   (string_of_qid q)
			   (Info.string_of_t i))
      end
  | TApp(i,pt1,pt2) ->
      begin
	match compile_ptypeexp cev pt1 with
	  | STOper(_,ps,rs), pt1_val ->
	      let pt2_sort, pt2_val = compile_ptypeexp cev pt2 in
	      let _ = expect_sort (SType(i)) pt2_sort (info_of_ptypeexp pt2) in
		rs, Type.App(pt1_val, pt2_val)
	  | _ -> run_error 
	      (sprintf 
		 "Type operator expected at left-hand side of type application at %s"
		 (Info.string_of_t i))
      end

  | TFun(i,xs,pt) -> 
      begin
	match xs with 
	    [] -> run_error (sprintf 
			       "Zero-argument type function at %s"
			       (Info.string_of_t i))
	  | [x] ->
	      let tsort = SType(i) in
	      let x_qid = qid_of_id x in
	      let dummy_cev = clear_rec_vars
		(update cev x_qid (make_rv tsort (Value.dummy tsort)))
	      in
		(* check body *)
	      let pt_sort,_ = compile_ptypeexp dummy_cev pt in
	      let _ = expect_sort tsort pt_sort in
	      let f_impl pt_arg =
		let fcev = clear_rec_vars 
		  (update cev
		     x_qid
		     (make_rv tsort (Value.T(Type.TT(pt_arg)))))
		in
		let _,body_val = compile_ptypeexp fcev pt in
		  body_val	    
	      in
	      let fun_sort = STOper(i,tsort,tsort) in
	      let fun_value = Type.Fun(f_impl) in
		fun_sort, fun_value
	  | (x::xrest) ->
	      let tbody = TFun(i, xrest, pt) in
                compile_ptypeexp cev (TFun(i,[x],tbody))
      end	  
        
  | TName(i,e,pt) ->      
      let n = compile_exp_name cev e in	
      let pt_sort, t_val = compile_ptypeexp (clear_rec_vars cev) pt in
      let _ = expect_sort (SType(i)) pt_sort (info_of_ptypeexp pt) in
	SType(i), (Type.Name(n,t_val))
	  
  | TBang(i,excl,pt) ->
      let excl_ns = List.map (compile_exp_name cev) excl in
      let pt_sort, t_val = compile_ptypeexp (clear_rec_vars cev) pt in
      let _ = expect_sort (SType(i)) pt_sort (info_of_ptypeexp pt) in
	SType(i), (Type.Bang(excl_ns,t_val))
	  
  | TStar(i,excl,pt) ->
      let excl_ns = List.map (compile_exp_name cev) excl in
      let pt_sort, t_val = compile_ptypeexp (clear_rec_vars cev) pt in
      let _ = expect_sort (SType(i)) pt_sort (info_of_ptypeexp pt) in
	SType(i), (Type.Star(excl_ns,t_val))
	  
  | TCat(i,pts) ->
      let pt_vals =
	Safelist.map
	  (fun pti ->
	     let pti_sort, ti_val = compile_ptypeexp cev pti in
	     let _ = expect_sort (SType(i)) pti_sort (info_of_ptypeexp pti) in
	       ti_val)
	  pts	      
      in
	SType(i), (Type.Cat(pt_vals))
	  
  | TUnion(i,pts) ->
      let pt_vals =
	Safelist.map
	  (fun pti ->
	     let pti_sort, ti_val = compile_ptypeexp cev pti in
	     let _ = expect_sort (SType(i)) pti_sort (info_of_ptypeexp pti) in
	       ti_val)
	  pts	      
      in
	SType(i), Type.Union(pt_vals)

and compile_viewbinds cev ks is_list = match ks with
  | []               -> V.empty
  | (i,ne,ve)::krest ->
      let n = compile_exp_name cev ne in
      let v = compile_exp_view cev ve in
      let vrest = compile_viewbinds cev krest is_list in	
	if (is_list) then 
	  V.cons (V.set (V.empty) n (Some v)) vrest
	else
	  match (V.get vrest n) with	      
	    | None   -> V.set vrest n (Some v)	      		
	    | Some _ -> run_error 
		(sprintf "Repeated name in view at %s" (Info.string_of_t i))
	    	  
and compile_bindings cev bs = 
  (* collect up a compile_env that includes recursive bindings *)
  let bcev =
    Safelist.fold_left
      (fun bcev (BDef(i,f,xs,so,e)) ->
	 let f_qid = qid_of_id f in
	   match so with
	       (* recursive bindings must give their return sort *)
	     | Some s -> 
		 let b_sort = (sort_of_param_list i xs s) in
		 let dummy = Value.dummy b_sort in
		   add_rec_var f_qid (update bcev f_qid (make_rv b_sort dummy))
	     | None   -> bcev)
      cev
      bs
  in
  let rec compile_binding cev = function      
      Syntax.BDef(i,f,[],so,e) -> 
	let f_qid = qid_of_id f in
	let r = compile_exp cev e in
	let r_sort = Registry.sort_of_rv r in
	let _ = match so with 
	  | None -> r_sort
	  | Some s -> expect_sort s r_sort i 
	in
	let v = Value.memoize (Registry.value_of_rv r) in
	  (f_qid, overwrite bcev f_qid (Registry.make_rv r_sort v))
    | Syntax.BDef(i,f,ps,so,e) -> 
	(* rewrite bindings with parameters to plain ol' lambdas *)
	compile_binding cev (Syntax.BDef(i,f,[],None,(EFun(i,ps,so,e))))
  in
  let bcev,names_rev = Safelist.fold_left 
    (fun (bcev,names_rev) bi ->  
       let f_qid, bcev = compile_binding bcev bi in
	 bcev, f_qid::names_rev)
    (bcev,[])
    bs
  in
    bcev, Safelist.rev names_rev
	   
(* type check all type defn's mutually, assume that each param has sort SType *)
let compile_typebindings cev tbs =
  (* helper for constructing arrow sorts *)
  let arrow_of_xs xs i =
    Safelist.fold_right
      (fun _ s -> STOper(i,SType(i), s))
      xs
      (SType(i))
  in
  (* collect an environment that records the sorts of all the type defns *)
  let tcev,tb_sorts_rev =
    Safelist.fold_left
      (fun (tcev,tb_sorts_rev) (x,xs,_) -> 
	 let x_qid = qid_of_id x in
	 let x_sort = arrow_of_xs xs (info_of_id x) in
	   (add_rec_var x_qid (update tcev x_qid (make_rv x_sort (Value.dummy x_sort))),
	    x_sort::tb_sorts_rev))
      (cev,[])
      tbs
  in
  let annotated_tbs = Safelist.combine (Safelist.rev tb_sorts_rev) tbs in
  let rec compile_typebinding cev (x_sort,(x,xs,t)) =
    match xs with 
	[] -> 
	  let x_qid = qid_of_id x in
	  let t_rv = compile_typeexp cev t in
	  let t_sort = sort_of_rv t_rv in
	  let t_val = value_of_rv t_rv in
	  let _ = expect_sort x_sort t_sort in
	    (x_qid, overwrite cev x_qid (make_rv t_sort t_val))	      
      | _  -> 
	  let i = info_of_typeexp t in 
	  let t_body = match t with
	      Syntax.TT pt -> Syntax.TT (TFun(i,xs,pt))
	    | Syntax.NT pt -> Syntax.NT (TFun(i,xs,pt))
	  in
	    compile_typebinding cev (x_sort,(x,[],t_body))
  in
  let tcev, names_rev = Safelist.fold_left
    (fun (tcev, names_rev) tbi -> 
       let x_qid, tcev = compile_typebinding tcev tbi in
	 tcev, x_qid::names_rev)
    (tcev, [])
    annotated_tbs
  in
    tcev, Safelist.rev names_rev
      
(* type check a single declaration *)
let rec compile_decl cev = function
  | DLet(i,bs) -> 
      let new_cev, names = compile_bindings cev bs in
	clear_rec_vars new_cev, names
  | DType(i,tbs) ->      
      let new_cev, names = compile_typebindings cev tbs in
	clear_rec_vars new_cev, names	  
  | DMod(i,n,ds) ->
      let nq = qid_of_id n in	
      let mcev, names = compile_module_aux cev ds in	
      let new_cev, names_rev = 
	Safelist.fold_left 
	  (fun (cev, names) q -> 
	     match Registry.lookup mcev get_ev q with
		 None -> run_error
		   (sprintf "The impossible happened! A just-compiled declaration, %s, is missing"
		      (string_of_qid q))
	       | Some rv ->
		   let nq_dot_q = dot nq q in
		     (update cev nq_dot_q rv, nq_dot_q::names))
	  (cev,[])
	  names
      in
	clear_rec_vars new_cev, Safelist.rev names_rev
	  
(* type check a module *)
and compile_module_aux cev ds =
  Safelist.fold_left 
    (fun (cev, names) di ->
       let new_cev, new_names = compile_decl cev di in
	 new_cev, names@new_names)
    (cev,[])
    ds

(* compile_module :: Syntax.modl -> unit *)
let compile_module (Syntax.MDef(i,m,nctx,ds)) = 
  let cev = set_ctx empty (nctx@Registry.pre_ctx) in
  let new_cev,_ = compile_module_aux cev ds in 
    Registry.register_env (get_ev new_cev) (Syntax.qid_of_id m) 
      
(* compile_file :: string -> unit *)
let compile_file fn n = 
  let fchan = open_in fn in
  let lexbuf = Lexing.from_channel fchan in
  let ast = 
    try Parser.modl Lexer.token lexbuf  
    with Parsing.Parse_error -> 
      raise (Error.Run_error ("Parse error at: " ^ Info.string_of_t (Lexer.info lexbuf)))
  in 
    (* check that module is in correct file *)
  let m = Syntax.id_of_modl ast in
  let _ = if (Syntax.id_equal m n) then 
    raise (Error.Sort_error(("Module " 
			     ^ (Syntax.string_of_id m) 
			     ^ " must appear in a file named " 
			     ^ (Syntax.string_of_id m) ^ ".fcl"), 
			    Syntax.info_of_modl ast))
  in  
  let _ = compile_module ast in
(*  let _ = if false then prerr_string (Registry.string_of_env (Registry.get_library ())) in *)
    ()
      
let _ = Registry.compile_file_impl := compile_file
