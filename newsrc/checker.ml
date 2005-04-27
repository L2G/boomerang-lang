(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* checker.ml - simple sort checker used in the Focal compiler  *)
(*                                                              *)
(* $Id$     *)
(*                                                              *)
(****************************************************************)

open Syntax

(* the checker performs the following tasks:
   - simple sort checking
   - annotates decls with their computed sorts, for easy 
     registration during compilation
   - flattens types so that under each type constructor, we have a
     variable. As a result, sort checking introduces new type
     definition.
   - replaces multi-parameter functions with single-parameter ones
   - rewrites let-defs and type-defs with arguments into simple
     binders and introduces functions to handle the arguments
*)

let (@) = Safelist.append (* use a stack-safe append *)
  
(** sort checking environments **)
(* comprise:                       
   - a qid list: variables that may NOT be used recursively 
   - a qid list: list of open modules 
   - a Registry.env: mapping from qids to Registry.rv (includes a sort)
*)
type sc_env = Syntax.qid list * Syntax.qid list * Registry.env

(* the empty enviroment*)
let empty = ([], [], Registry.empty)

(* getters/setters for sc_envs *)
let get_ev sev = let (_,_,ev) = sev in ev
let set_ev sev ev = let (rs,os,_) = sev in (rs,os,ev)
let get_ctx sev = let (_,os,_) = sev in os
let set_ctx sev qs = let (rs,os,ev) = sev in (rs,qs@os,ev)

(** helper functions for environments **) 
(* lookup a qid in an sev *)
let lookup sev q = 
  match (Registry.lookup_library sev get_ev get_ctx q) with
    | None -> None
    | Some r -> Some (Registry.sort_of_rv r)

(* update an sev with a new sort *)
let update sev q s = 
  Registry.update sev get_ev set_ev q (Registry.make_rv s Value.dummy)

(* add a qid to the list of recursive variables *)
let add_rec_var q sev = let (rs,os,ev) = sev in (q::rs,os,ev)

(* check if a recursive use of a variable is OK *)
let rec_var_ok sev q = let (rs,_,_) = sev in not (Safelist.mem q rs)

(* clear the list of recursive variables (e.g., when we go under a lambda) *)
let clear_rec_vars sev = let (_,os,ev) = sev in ([],os,ev)

(* update an sev with a parameter *)
let update_param sev p = update sev (qid_of_id (id_of_param p)) (sort_of_param p)

(* convert a list of parameters ps and a sort s to arrow sort (ps -> s) *)
let sort_of_param_list i ps s = 
  Safelist.fold_right (fun p ts -> SArrow(i,sort_of_param p,ts))  ps s
    
(* check that s matches expected sort es. Print a nice error message if not *)
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
    | _ -> 
	raise (Error.Sort_error ("The expression at "
				 ^ (Info.string_of_t i)
				 ^ " has sort: "
				 ^ (Syntax.string_of_sort s)
				 ^ ", but is used with sort: "
				 ^ (Syntax.string_of_sort es),i))
	  
(** poor man's gensym **) 
(* - used to obtain fresh type variables   
   - assumes that user type variables don't start with '_' we should
     make front-end enforce this policy
*)
let fresh_counter = ref 0
let fresh_var () =
  let n = !fresh_counter in
  let _ = fresh_counter := n + 1 in
  let i = n / 26 in
  let ns = Char.escaped (char_of_int (n mod 26 + 65)) in
  let is = if (i = 0) then "" else "_" ^ (string_of_int i) in
    "_" ^ ns ^ is
      
(** expressions **)
(* returns a triple: a sort, a list of new type declarations, a new expression *)
let rec sc_exp sev e0 = match e0 with
    EVar(i,q) ->
      (* variables: just look up in the environment; check that recursive uses OK *)
      begin
	match (rec_var_ok sev q, lookup sev q) with
	  | (true, Some s) -> s, [], e0
	  | (false, Some _)  -> 
	      raise (Error.Sort_error("Variable "
				      ^(Syntax.string_of_qid q)
				      ^ " may not be used recursively at "
				      ^ Info.string_of_t i,i))		
	  | (_, None) ->
	      raise (Error.Sort_error("Unbound variable: "
				      ^(Syntax.string_of_qid q)
				      ^ " at "
				      ^(Info.string_of_t i)
				      ,i))
      end
	
  | EFun(i,[],_,_) -> raise (Error.Run_error("Zero argument function at " 
					     ^ Info.string_of_t i))					       
      
  | EFun(i,[p],so,e) ->
      (* functions: check the body in a context extended with
	 parameter's name and sort *)
      (* under a lambda, all vars may be used recursively *)
      let esev = clear_rec_vars (update_param sev p) in 
      let es,new_tds,new_e = sc_exp esev e in
      let _ = match so with
	| None -> es
 	| Some s -> expect_sort s es i
      in
      let fs = SArrow(i, sort_of_param p, es) in
      let new_f = EFun(i, [p], Some es, new_e) in
	fs, new_tds, new_f
	  
  | EFun(i,(p1::p2::ps),so,e) ->
      (* rewrite multi-argument functions to plain ol' lambdas *)
      let fev = update_param sev p1 in
      let body_sort,new_tds,body_exp = sc_exp fev (EFun(i,p2::ps,so,e)) in
      let fs = SArrow(i, sort_of_param p1, body_sort) in
      let new_f = EFun(i,[p1],Some body_sort, body_exp) in
	fs, new_tds, new_f
	  
  | EMap(i,ms) ->
      (* finite maps from names to lenses: just check that each lens
	 expression has the right sort *)
      let new_tds,new_ms,new_sev = Safelist.fold_right
	(fun (x,l) (new_tds1,new_ms1,sev) ->
	   (* under a lambda, all vars may be used recursively *)
	   let esev = clear_rec_vars (sev) in
	   let ls,new_tds2,new_l = sc_exp esev l in
	   (* check that l has sort SLens *)
	   let _ = expect_sort (SLens(i)) ls in
	     (new_tds2 @ new_tds1), (x, new_l)::new_ms1, sev)
	ms
	([],[],sev)
      in
	(SArrow(i, SName(i), SLens(i)), new_tds, EMap(i, new_ms))
	  
  | EApp(i,e1,e2) ->
      (* applications: check that e1 has a function sort and that t2
	 has the correct param type *)
      let e1s,new_tds1,new_e1 = sc_exp sev e1 in
	begin
	  match e1s with
	    | SArrow(_,sa1,sa2) ->
		let e2s,new_tds2,new_e2 = sc_exp sev e2 in
		let _ = expect_sort sa1 e2s i in
		  sa2,(new_tds1@new_tds2),EApp(i,new_e1,new_e2)
	    | _ ->
		raise (Error.Sort_error("Expected an arrow sort at "
					^ (Info.string_of_t i)
					^ ", found "
					^ (Syntax.string_of_sort e1s), i))
	end
	  
  | ELet(i,bs,e) ->
      (* lets: check e in an environment extended with
	 sorts for bindings *)
      let bev,new_tds1,new_bs,_ = sc_bindings sev bs in
      let es,new_tds2,new_e = sc_exp bev e in
	es, (new_tds1 @ new_tds2), ELet(i,new_bs,new_e)
	  
  | EName(i,id) -> 
      (* name constants: trivially have sort SName *)
      SName(i), [], e0
	
  | EType(i,t) ->
      (* types: use auxiliary sc_typeexp, check that result is SType *)
      let ts,new_tds,new_t = sc_typeexp sev t in
      let _ = expect_sort (SType(i)) ts in
	ts,new_tds,EType(i,new_t)

  | EView(i,ks,is_list) ->
      (* views: check that dom is SNames, range SViews *)
      let new_tds, new_ks = Safelist.fold_right
	(fun (i,n,v) (new_tds1,new_ks1) ->
 	   (* check that n has sort SName *)
	   let ns,new_tds2,new_n = sc_exp sev n in
	   let _ = expect_sort (SName(i)) ns in
	     (* check that v has sort SView *)
	   let vs,new_tds3,new_v = sc_exp sev v in
	   let _ = expect_sort (SView(i)) vs in
	     (new_tds2 @ new_tds3 @ new_tds1), (i,new_n, new_v)::new_ks1)
	ks
	([],[])
      in
	SView(i),new_tds,EView(i,new_ks, is_list)
	  
(** type expressions **)
(* returns its sort, new_tds, and a new type expression *)
and sc_typeexp ev t =
  (* helper, introduce fresh variables for nested type expressions *)
  let typeexp2var t =
    match t with
      | TExp(i, EVar(_,q)) -> [], t
      | _ ->
	  let i = info_of_typeexp t in
	  let xid = id_of_string i (fresh_var ()) in
	  let qid = qid_of_id xid in
	  let new_decl = (xid,[],t) in
	  let new_t = TExp(i, EVar(i,qid)) in
	    (* t was already checked, no need to re-check *)
	    [new_decl], new_t 
  in
    match t with
	TEmpty(i) -> SType(i),[],t
	  
      | TName(i,e,t) ->
	  let es,new_tds1,new_e = sc_exp ev e in
	  let _ = expect_sort (SName(i)) es i in
	  let ts,new_tds2,new_t = sc_typeexp (clear_rec_vars ev) t in 
	  let _ = expect_sort (SType(i)) ts i in
	  let new_tds3,xt = typeexp2var new_t in
	    SType(i), (new_tds1 @ new_tds2 @ new_tds3), TName(i,new_e,xt)
	      
      | TBang(i,excl,t) ->
	  let ts,new_tds1,new_t = sc_typeexp (clear_rec_vars ev) t in 
	  let new_tds2,xt = typeexp2var new_t in
	  let _ = expect_sort (SType(i)) ts i in
	    SType(i), (new_tds1 @ new_tds2), TBang(i,excl,xt)
	      
      | TStar(i,excl,t) ->
	  let ts,new_tds1, new_t = sc_typeexp (clear_rec_vars ev) t in 
	  let new_tds2,xt = typeexp2var new_t in
	  let _ = expect_sort (SType(i)) ts i in
	    SType(i), (new_tds1 @ new_tds2), TStar(i,excl,xt)
	      
      | TCat(i,ts) ->
	  let new_tds, new_ts_rev =
	    Safelist.fold_left
	      (fun (new_tds1, new_ts) ti ->
		 let tis,new_tds2, new_ti = sc_typeexp ev ti in
		 let _ = expect_sort (SType(i)) tis i in
		   (new_tds2@new_tds1, new_ti::new_ts))
	      ([],[])
	      ts	      
	  in
	    SType(i), new_tds, TCat(i,Safelist.rev new_ts_rev)
	      
      | TUnion(i,ts) ->
	  let new_tds, new_ts =
	    Safelist.fold_left
	      (fun (new_tds1, new_ts) ti ->
		 let tis,new_tds2, new_ti = sc_typeexp ev ti in
		 let _ = expect_sort (SType(i)) tis i in
		   (new_tds2@new_tds1, new_ti::new_ts))
	      ([],[])
	      ts
	  in
	    SType(i), new_tds, TUnion(i,new_ts)
		  
      | TDiff(i,t1,t2) ->
	  let t1s,new_tds1,new_t1 = sc_typeexp ev t1 in
	  let _ = expect_sort (SType(i)) t1s i in
	  let t2s,new_tds2,new_t2 = sc_typeexp ev t2 in
	  let _ = expect_sort (SType(i)) t2s i in
	    SType(i), (new_tds1 @ new_tds2), TInter(i,new_t1,new_t2)

      | TInter(i,t1,t2) ->
	  let t1s,new_tds1,new_t1 = sc_typeexp ev t1 in
	  let _ = expect_sort (SType(i)) t1s i in
	  let t2s,new_tds2,new_t2 = sc_typeexp ev t2 in
	  let _ = expect_sort (SType(i)) t2s i in
	    SType(i), (new_tds1 @ new_tds2), TInter(i,new_t1,new_t2)
		
      | TExp(i,e) ->
	  let es, new_tds,new_e = sc_exp ev e in
	    es,new_tds,TExp(i,new_e)

and sc_bindings sev bs = 
  (* collect an sc_env that records the sorts for bindings that may be
     used recursively *)
  (* also returns a list of bindings checked *)
  let bev =
    Safelist.fold_left
      (fun bev (BDef(i,f,xs,so,e)) ->
	 let fq = qid_of_id f in
	   match so with
	     (* bindings used recursively must give their return sort *)
	     | Some s -> add_rec_var fq (update bev fq (sort_of_param_list i xs s))
	     | None   -> bev)
      sev
      bs
  in

  (* sort checking/rewrite a single binding *)
  let rec sc_binding ev b = 
    match b with
      | BDef(i,f,[],so,e) ->
	  let es,new_tds,new_e = sc_exp ev e in
	  let _ = match so with
	    | None -> es
	    | Some s -> expect_sort s es i
	  in
	    ((Syntax.qid_of_id f,es),
	     new_tds,
	     BDef(i,f,[],Some es,new_e))
	      
      (* rewrite bindings with parameters to plain ol' lambdas *)
      | BDef(i,f,ps,so,e) -> sc_binding ev (BDef(i,f,[],None,(EFun(i,ps,so,e))))
  in
  let (new_sev, new_tds, new_bs_rev, names_rev) = 
    Safelist.fold_left
      (fun (sev_rest,tds_rest,binds_rest,names_rest) bi ->
	 let (q,s),new_tds,new_bi = sc_binding sev_rest bi in
	 let new_sev = add_rec_var q (update sev_rest q s) in
	   new_sev, tds_rest@new_tds, new_bi::binds_rest, q::names_rest)
      (bev, [], [], [])
      bs
  in
    (new_sev, new_tds, Safelist.rev new_bs_rev, Safelist.rev names_rev)
      
(* type check all type defn's mutually, assume that each param has sort SType *)
let sc_typebindings i ev tbs =
  (* helper for constructing arrow sorts *)
  let arrow_of_xs xs i =
    Safelist.fold_right
      (fun _ s -> SArrow(i,SType(i), s))
      xs
      (SType(i))
  in
  (* collect an environment that records the sorts of all the type defns *)
  let tev,(tsorts_rev : Syntax.sort list),names_rev = 
    Safelist.fold_left
      (fun (tev,sorts,names) (x,xs,_) -> 
	 let xq = qid_of_id x in 
	 let xsort = arrow_of_xs xs i in
	   add_rec_var xq (update tev xq xsort), xsort::sorts, xq::names)
      (ev,[],[])
      tbs
  in    
  (* type bindings, annotated with their sorts *)
  let annot_tbs = Safelist.combine (Safelist.rev tsorts_rev) tbs in
    
  (* helper for checking a single binding *)
  let rec sc_typebinding ev annot_tb =
    match annot_tb with
      | xsort, (x,[],t) ->
	  let ts,new_tds,new_t = sc_typeexp ev t in	    
	  let _ = expect_sort xsort ts i in
	    new_tds@[(x,[],new_t)] (* FIXME: replace append *)
      | xsort, (x,xs,t) ->
	  let ps = 
	    Safelist.map (fun xi -> let i = info_of_id xi in PDef(i,xi,SType(i))) xs 
	  in
	  let i = info_of_typeexp t in
	  let new_t = TExp(i, EFun(i, ps, Some (SType(i)), EType(i,t))) in
	    sc_typebinding ev (xsort,(x,[],new_t))
  in
  let new_tbs = 
    (* FIXME: DV says this is inscrutable! Make this clearer *)
    Safelist.fold_left 
      (fun new_tbs annot_tbi -> new_tbs@(sc_typebinding tev annot_tbi))
      [] 
      annot_tbs
  in
    tev, new_tbs, Safelist.rev names_rev
      
(* type check a single declaration *)
let rec sc_decl sev = function
  | DLet(i,bs) ->
      let bev, new_tds, new_bs, names = sc_bindings sev bs in	
	clear_rec_vars bev, new_tds, DLet(i,new_bs), names
	  
  | DType(i,tbs) ->
      let tev,new_tbs,names = sc_typebindings i sev tbs in
	clear_rec_vars tev, [], DType(i,new_tbs), names
	  
  | DMod(i,n,ds) ->
      let nq = Syntax.qid_of_id n in	
      let new_sev, new_ds, names = sc_modl_aux sev i ds in	
      let new_sev = Safelist.fold_left 
	(fun sev q -> 
	   (* patch up the environment with correct names for the rest of _this_ module *)
	   match Registry.lookup new_sev get_ev q with
	     | None -> 
		 raise (Error.Run_error("The impossible happened! A just checked declaration vanished at " 
					^ Info.string_of_t i))
	     | Some r -> Registry.update sev get_ev set_ev (dot nq q) r)
	sev
	names
      in
	clear_rec_vars new_sev, [], DMod(i, n, new_ds), names
	  
(* type check a module *)
and sc_modl_aux sev i ds : sc_env * Syntax.decl list * Syntax.qid list =
  let new_sev,new_ds_rev,names = 
    Safelist.fold_left
      (fun (new_sev,ds,names) di ->
	 let new_sev, new_tds, new_di, new_names = sc_decl new_sev di in
	   (new_sev, 
	    (if new_tds = [] then (new_di::ds)
	     else (new_di::(DType(i,new_tds))::ds)),
	    names@ new_names))
      (sev,[],[])
      ds
  in
    new_sev, Safelist.rev new_ds_rev, names
      
let sc_module = function MDef(i,m,o,ds) ->   
  let mqn = Syntax.qid_of_id m in
  let sev = set_ctx empty (o@Registry.pre_ctx) in
  let _,new_ds,_ = sc_modl_aux sev i ds in
    MDef(i,m,o,new_ds)
