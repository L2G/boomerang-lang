(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* checker.ml - simple sort checker used in the Focal compiler  *)
(*                                                              *)
(* $Id: checker.ml,v 1.4 2005/04/20 19:25:02 jnfoster Exp $     *)
(*                                                              *)
(****************************************************************)

open Syntax

(* the checker performs the following tasks:
   - simple sort checking
   - annotates decls with their computed sorts, for easy 
     registration during compilation
   - flattens types, introducing new definitions as needed, again
     for easy compilation to Type.t *)
  
(* sort checking environments *)
type sc_env = Syntax.qid list * Syntax.qid list * Registry.env
    
let empty = ([],[],Registry.empty)
let get_ev sev = let (_,_,ev) = sev in ev
let set_ev sev ev = let (rs,os,_) = sev in (rs,os,ev)
let get_ctx sev = let (_,os,_) = sev in os
let set_ctx sev qs = let (rs,os,ev) = sev in (rs,qs@os,ev)

let lookup sev q = 
  match (Registry.lookup_library sev get_ev get_ctx q) with
    | None -> None
    | Some r -> Some (Registry.sort_of_rv r)
let update sev q s = Registry.update sev get_ev set_ev q (Registry.make_rv s Value.dummy)
let update_param sev p = update sev (qid_of_id (id_of_param p)) (sort_of_param p)
  
let add_rec_var q sev = let (rs,os,ev) = sev in (q::rs,os,ev)
let rec_var_ok sev q = let (rs,_,_) = sev in not (List.mem q rs)
let clear_rec_vars sev = let (_,os,ev) = sev in ([],os,ev)

				   
(* convert a list of parameters ps and a sort s to arrow sort (ps -> s) *)
let sort_of_param_list i ps s = 
  List.fold_right (fun p ts -> SArrow(i,sort_of_param p,ts))  ps s
    
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
    | _ -> 
	raise (Error.Sort_error ("The expression at "
			   ^ (Info.string_of_t i)
			   ^ " has sort: "
			   ^ (Syntax.string_of_sort s)
			   ^ ", but is used with sort: "
			   ^ (Syntax.string_of_sort es),i))
	  
(* poor man's gensym - 
   - used for type variables only
   - assumes that user type variables don't start with _ we should either 
   (1) implement a real gensym, or 
   (2) make front-end enforce this policy *)
let fresh_counter = ref 0
let fresh_var () =
  let n = !fresh_counter in
  let _ = fresh_counter := n + 1 in
  let i = n / 26 in
  let ns = Char.escaped (char_of_int (n mod 26 + 65)) in
  let is = if (i = 0) then "" else "_" ^ (string_of_int i) in
    "_" ^ ns ^ is
      
(* sort check expressions *)
(* returns a triple: a sort, a list of new type declarations, a new expression, and a new environment *)
let rec sc_exp (sev:sc_env) e0 = match e0 with
    EVar(i,q) ->
      begin
	match (rec_var_ok sev q, lookup sev q) with
	  | (true, Some s) -> s, [], e0
	  | (false, Some _)  -> 
	      raise (Error.Sort_error("Non-contractive use of:"
				      ^(Syntax.string_of_qid q)
				      ^ " at "
				      ^(Info.string_of_t i),i))		
	  | (_, None) ->
	      raise (Error.Sort_error("Unbound variable: "
				      ^(Syntax.string_of_qid q)
				      ^ " at "
				      ^(Info.string_of_t i),i))
      end

  | EFun(i,[],_,_) -> raise (Error.Run_error("Zero argument function at " 
					     ^ Info.string_of_t i))					       
      
  | EFun(i,[p],so,e) ->
      (* plain ol' lambdas *)      
      let esev = clear_rec_vars (update_param sev p) in (* under a lambda, all vars may be used recursively *)
      let es,new_tds,new_e = sc_exp esev e in
      let _ = match so with
	| None -> es
 	| Some s -> expect_sort s es i
      in
        SArrow(i, sort_of_param p, es), new_tds, EFun(i, [p], Some es, new_e)

  | EFun(i,(p1::p2::ps),so,e) ->
      (* rewrite multi-argument functions to plain ol' lambdas *)
      let fev = update_param sev p1 in
      let body_sort,new_tds,body_exp = sc_exp fev (EFun(i,p2::ps,so,e)) in
	SArrow(i, sort_of_param p1, body_sort), new_tds, EFun(i,[p1],Some body_sort, body_exp)
	  
  | EMap(i,ms) ->
      let new_tds,new_ms,new_sev = List.fold_right
	(fun (x,l) (new_tds1,new_ms1,sev) ->
	   (* check that l has sort SLens *)
	   let ls,new_tds2,new_l = sc_exp sev l in
	   let _ = expect_sort (SLens(i)) ls in
	     (new_tds2 @ new_tds1), (x, new_l)::new_ms1, sev)
	  ms
	([],[],sev)
      in
	(SArrow(i, SName(i), SLens(i)), new_tds, EMap(i, new_ms))
	  
  | EApp(i,e1,e2) ->
      (* check that e1 has a function sort and that t2 has the correct param type *)
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
      let bev,new_tds1,new_bs = sc_bindings sev bs in
      let es,new_tds2,new_e = sc_exp bev e in
	es, (new_tds1 @ new_tds2), ELet(i,new_bs,new_e)
	  
  | EName(i,id) -> SName(i), [], e0
	  
  | EType(i,t) ->
      let ts,new_tds,new_t = sc_typeexp sev t in
      let _ = expect_sort (SType(i)) ts in
	ts,new_tds,EType(i,new_t)

  | EView(i,ks) ->
      let new_tds, new_ks = List.fold_right
	(fun (i,n,v) (new_tds1,new_ks1) ->
	   (* check that n has sort SName *)
	   let ns,new_tds2,new_n = sc_exp sev n in
	   let _ = expect_sort (SName(i)) ns in
	     (* check that n has sort SView *)
	   let vs,new_tds3,new_v = sc_exp sev v in
	   let _ = expect_sort (SView(i)) vs in
	     (new_tds2 @ new_tds3 @ new_tds1), (i,new_n, new_v)::new_ks1)
	ks
	([],[])
      in
	SView(i),new_tds,EView(i,new_ks)

(* sort checks a type expression *)
(* returns its sort, new_tds, and a new type expression *)
and sc_typeexp ev t =
  let typeexp2var t =
    match t with
      | TExp(i, EVar(_,q)) -> [], t
      | _ ->
	  let i = info_of_typeexp t in
	  let x = fresh_var () in
	  let xid = (i,x) in
	  let qid = qid_of_id xid in
	  let new_decl = (xid,[],t) in
	  let new_t = TExp(i, EVar(i,qid)) in
	    [new_decl], new_t (* no new ev is needed -- t already checked *)
  in
    match t with
	TEmpty(i) -> SType(i),[],t
	  
      | TName(i,e,t) ->
	  let es,new_tds1,new_e = sc_exp ev e in
	  let _ = expect_sort (SName(i)) es i in
	  let ts,new_tds2,new_t = sc_typeexp (clear_rec_vars ev) t in (* FIX LATER: clear ev here? *)
	  let _ = expect_sort (SType(i)) ts i in
	  let new_tds3,xt = typeexp2var new_t in
	    SType(i), (new_tds1 @ new_tds2 @ new_tds3), TName(i,new_e,xt)

      | TBang(i,excl,t) ->
	  let ts,new_tds1,new_t = sc_typeexp (clear_rec_vars ev) t in (* FIX LATER: clear ev here? *)
	  let new_tds2,xt = typeexp2var new_t in
	    expect_sort (SType(i)) ts i, (new_tds1 @ new_tds2), TBang(i,excl,xt)
	      
      | TStar(i,excl,t) ->
	  let ts,new_tds1, new_t = sc_typeexp (clear_rec_vars ev) t in (* FIX LATER: clear ev here? *)
	  let new_tds2,xt = typeexp2var new_t in
	    expect_sort (SType(i)) ts i, (new_tds1 @ new_tds2), TStar(i,excl,xt)
	      
      (* n-ary type constructors *)
      | TCat(i,ts) ->
	  let new_tds, new_ts =
	    List.fold_right
	      (fun ti (new_tds1, new_ts) ->
		 let tis,new_tds2, new_ti = sc_typeexp ev ti in
		 let _ = expect_sort (SType(i)) tis i in
		   (new_tds2@new_tds1, new_ti::new_ts))
	      ts
	      ([],[])
	  in
	    SType(i), new_tds, TCat(i,new_ts)
	      
      | TUnion(i,ts) ->
	  let new_tds, new_ts =
	    List.fold_right
	      (fun ti (new_tds1, new_ts) ->
		 let tis,new_tds2, new_ti = sc_typeexp ev ti in
		 let _ = expect_sort (SType(i)) tis i in
		   (new_tds2@new_tds1, new_ti::new_ts))
	      ts
	      ([],[])
	  in
	    SType(i), new_tds, TUnion(i,new_ts)
		  
      (* binary type constructors *)
      | TDiff(i,t1,t2) ->
	  let t1s,new_tds1,new_t1 = sc_typeexp ev t1 in
	  let t2s,new_tds2,new_t2 = sc_typeexp ev t2 in
	  let _ = expect_sort (SType(i)) t1s i in
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

	    
(* we re-write bindings as we check them, annotating with sorts for easy compilation later 
   also turn parameters into functions *)
and sc_bindings sev bs = 
  (* collect sorts for bindings that may be used recursively *)
  let bev =
    List.fold_left
      (fun bev (BDef(i,f,xs,so,e)) ->
	 let fq = qid_of_id f in
	 match so with
	     (* bindings used recursively must give their return sort *)
	   | Some s -> add_rec_var fq (update bev fq (sort_of_param_list i xs s))
	   | None   -> bev)
      sev
      bs
  in

  (* helper for sort checking/rewriting single bindings *)
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
      | BDef(i,f,ps,so,e) -> 	
	  let res = sc_binding ev (BDef(i,f,[],None,(EFun(i,ps,so,e)))) in (* the None gets overwritten *)
	  let (_,_,newb) = res in
	    res
  in
    List.fold_right
      (fun bi (sev_rest,tds_rest,binds_rest) ->
	 let (q,s),new_tds,new_binding = sc_binding sev_rest bi in
	 let new_sev = add_rec_var q (update sev_rest q s) in
	   new_sev, (new_tds@tds_rest), (new_binding::binds_rest))
      bs
      (bev, [], [])
      
(* type check all type defn's mutually, assume that each param has sort SType *)
let sc_typebindings i ev tbs =
  (* helper for constructing arrow sorts *)
  let arrow_of_xs xs i =
    List.fold_right
      (fun _ s -> SArrow(i,SType(i), s))
      xs
      (SType(i))
  in
  (* collect all the (possibly recursive) type defns *)
  let tev = 
    List.fold_left
      (fun tev (x,xs,_) -> 
	 let xq = qid_of_id x in 
	   add_rec_var xq (update tev xq (arrow_of_xs xs i)))
      ev
      tbs
  in    
  (* helper for checking a single binding *)
  let rec sc_typebinding ev tb =
    match tb with
      | (x,[],t) ->
	  let ts,new_tds,new_t = sc_typeexp ev t in	    
	  let _ = expect_sort (SType(i)) ts i in
	    new_tds @ [(x,[],new_t)]
      | (x,xs,t) ->
	  let ps = 
	    List.map 
	      (fun xi -> 
		 let i = get_info_id xi in
		   PDef(i, xi, SType(get_info_id xi)))
	      xs 
	  in
	  let i = info_of_typeexp t in
	    sc_typebinding ev (x,[],TExp(i, (EFun(i, ps, Some (SType(i)), EType(i,t)))))					       	    
  in
  let new_tbs = List.fold_right (fun tbi new_tbs -> (sc_typebinding tev tbi)@new_tbs) tbs []
  in
    tev,new_tbs
      
(* type check a single declaration *)
let rec sc_decl sev = function
  | DLet(i,bs) ->
      let bev, new_tds, new_bs = sc_bindings sev bs in	
	clear_rec_vars bev, new_tds, DLet(i,new_bs)
	  
  | DType(i,tbs) ->
      let tev,new_tbs = sc_typebindings i sev tbs in
	clear_rec_vars tev, [], DType(i,new_tbs)
	  
  | DMod(i,n,ds) ->
      let nqn = Syntax.qid_of_id n in	
	clear_rec_vars sev, [], DMod(i, n, sc_modl_aux sev i ds)
	  
(* type check a module *)
and sc_modl_aux sev i ds =
  let (_,new_ds) = List.fold_left
    (fun (new_sev,ds) di ->
       let new_sev, new_tds, new_di = sc_decl new_sev di in
       let new_decls = 
	 if new_tds = [] then new_di::ds 
	 else (DType(i,new_tds))::new_di::ds 
       in
	 (new_sev, new_decls))
    (sev,[])
    ds
  in
    (List.rev new_ds)
       
let sc_module = function MDef(i,m,o,ds) ->   
  let mqn = Syntax.qid_of_id m in
  let sev = set_ctx empty o in
    MDef(i,m,o,sc_modl_aux sev i ds)
      
