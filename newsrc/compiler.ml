(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - the actual compiler                            *)
(*                                                              *)
(* $Id$    *)
(*                                                              *)
(****************************************************************)

(* We assume that the checker has already been run:
   - annotating functions and bindings with sorts, 
   - flattening TName, TStar, and TBangs
   - rewriting multi-param functions to lambdas 
   - rewriting let-defs and type-defs to simple no-param binders
*)

let (@) = Safelist.append 

(* a compilation environment: a naming context and a Registry.env *)
type compile_env = Syntax.qid list * Registry.env
let empty = ([], Registry.empty)

(* coercions *)
let get_ev cev = let (_,ev) = cev in ev
let set_ev cev ev = let (os,_) = cev in (os,ev)
let set_ctx cev qs = let (os,ev) = cev in (qs@os,ev)
let get_ctx cev = let (os,_) = cev in os					  

(* standard operations on compile_envs *)
let lookup cev q = Registry.lookup_library cev get_ev get_ctx q
let update cev q r = Registry.update cev get_ev set_ev q r
let overwrite cev q r = Registry.overwrite cev get_ev set_ev q r
				     
(* expressions *)
(* compile_exp :: compile_env -> Syntax.exp -> Value.rv *)
let rec compile_exp cev e0 = match e0 with
  | Syntax.EVar(i,q) -> 
      begin match (lookup cev q) with
	| Some r -> r
	| None -> raise (Error.Run_error("Unbound variable " 
					 ^ (Syntax.string_of_qid q)
					 ^ "."))
      end
	
  | Syntax.EFun(i,xs,so,e) ->
      begin match xs, so with 
	| [p], Some s  -> 
	    let ps = Syntax.sort_of_param p in
	    let f_impl v = 
	      let fcev = 
		update 
		  cev 
		  (Syntax.qid_of_id (Syntax.id_of_param p)) 
		  (Registry.make_rv ps v) 
	      in
		Registry.value_of_rv (compile_exp fcev e)
	    in
	    let fs = Syntax.SArrow(i,ps,s) in
	      Registry.make_rv fs (Value.F(f_impl))
	| _     -> 
	    raise (Error.Run_error("Ill-formed function at " 
				   ^ Info.string_of_t i))
      end
	
  | Syntax.EMap(i,ms) ->
      let name_error i = raise (Error.Run_error("Name expected in domain of finite map at " 
						^ Info.string_of_t i))
      in
      let id_exp = Syntax.EVar(i, Registry.parse_qid "Pervasives.Native.id") in	    
      let s = Syntax.SArrow(i, Syntax.SName(i), Syntax.SLens(i)) in
      let f = Safelist.fold_left 
	(fun f (x,l) -> 
	   (fun v -> 
	      match v with
		| Value.N n -> begin 
		    (* FIX LATER: should memoize this inner compilation *)
		    let v2 = Registry.value_of_rv (compile_exp cev x) in
		      match v2 with
			| Value.N n2 -> 		    
			    if (Syntax.id_equal n n2) then
			      Registry.value_of_rv (compile_exp cev l)
			    else 
			      f v
			| _ -> name_error i
		  end
		| _ -> name_error i))	
	(fun _ -> Registry.value_of_rv (compile_exp cev id_exp))
	ms
      in
	Registry.make_rv s (Value.F(f))

  | Syntax.EApp(i,e1,e2) ->
      begin
	let rv1 = compile_exp cev e1 in
	  match 
	    Registry.value_of_rv rv1, 
	    Registry.sort_of_rv rv1 
	  with
	    | Value.F(f),Syntax.SArrow(_,ps,rs) -> 
		let rv2 = compile_exp cev e2 in
		  Registry.make_rv rs (f (Registry.value_of_rv rv2))		    
	    | _   -> 
		raise (Error.Run_error("Function expected in left-hand side of application at " 
				       ^ Syntax.string_of_exp e1 ^ " " ^ Syntax.string_of_exp e2 
				       ^ " --> " ^ Registry.string_of_rv rv1
				       ^ Info.string_of_t i))
      end
	
  | Syntax.ELet(i,bs,e) -> 
      let benv,_ = compile_bindings cev bs in
	compile_exp benv e
	  
  | Syntax.EName(i,id) -> 
      Registry.make_rv (Syntax.SName(i)) (Value.N (Syntax.name_of_id id))

  | Syntax.EType(i,t) -> compile_typeexp cev t
      
  | Syntax.EView(i,ks,is_list) -> 
      Registry.make_rv (Syntax.SView(i)) (Value.V (compile_viewbinds cev ks is_list))
      
(* types *)
(* compile_exp :: compile_env -> Syntax.typeexp -> Value.t *)
and compile_typeexp cev t = 
  let nexp2name e = match Registry.value_of_rv (compile_exp cev e) with
    | Value.N n -> n
    | _   -> raise(Error.Run_error("Name expected at " ^ Info.string_of_t (Syntax.info_of_exp e)))
  in
  let texp2type e = match Registry.value_of_rv(compile_typeexp cev e) with
    | Value.T t -> t
    | _   -> raise(Error.Run_error("Type expected at " ^ Info.string_of_t (Syntax.info_of_typeexp e)))
  in    
    match t with
      | Syntax.TEmpty(i)     -> 
	  Registry.make_rv (Syntax.SType(i)) (Value.T (Type.Empty))
      | Syntax.TName(i,n,e)  -> 
	  Registry.make_rv (Syntax.SType(i)) (Value.T (Type.Name(nexp2name n, texp2type e)))
      | Syntax.TBang(i,xl,e) -> 
	  Registry.make_rv (Syntax.SType(i)) (Value.T (Type.Bang(Safelist.map nexp2name xl, texp2type e)))
      | Syntax.TStar(i,xl,e) -> 
	  Registry.make_rv (Syntax.SType(i)) (Value.T (Type.Star(Safelist.map  nexp2name xl, texp2type e)))
      | Syntax.TCat(i,ts)    -> 
	  Registry.make_rv (Syntax.SType(i)) (Value.T (Type.Cat(Safelist.map texp2type ts)))    
      | Syntax.TUnion(i,ts)  -> 
	  Registry.make_rv (Syntax.SType(i)) (Value.T (Type.Union(Safelist.map texp2type ts)))
      | Syntax.TExp(i,e)     -> 
	  match e with 
	    | Syntax.EFun(_) -> compile_exp cev e
	    | _ -> 
		Registry.make_rv 
		  (Syntax.SType(i)) (* FIXME: this sort is BOGUS *) 
		  (Value.T (Type.Thunk (fun () -> 
					  match Registry.value_of_rv (compile_exp cev e) with
					    | Value.T t -> t
					    | _ -> raise (Error.Run_error("Type expected at : " ^ Info.string_of_t (Syntax.info_of_exp e))))))
			      
(* views *)
(* compile_viewbinds :: env -> (Info.t * Syntax.exp * Syntax.exp) list -> bool -> V.t *)
and compile_viewbinds ev bs is_list = match bs with
  | [] -> V.empty
      
  | (i,ne,ve)::rest ->
      let n =
	match Registry.value_of_rv (compile_exp ev ne) with
	  | Value.N n -> n
	  | _ -> 
	      raise (Error.Run_error("Expected a name at " ^ Info.string_of_t i))
      in
      let v = match Registry.value_of_rv (compile_exp ev ve) with
	| Value.V v -> v
	| _ -> 
	    raise (Error.Run_error("Expected a view at " ^ Info.string_of_t i))
      in
      let vrest = compile_viewbinds ev rest is_list in	
	if (is_list) then 
	  V.cons (V.set (V.empty) n (Some v)) vrest
	else
	  match (V.get vrest n) with	      
	    | Some _ -> (* simple sanity check *)
		raise (Error.Run_error("Repeated name in view at " ^ Info.string_of_t i))
	    | None   -> V.set vrest n (Some v)	      

(* bindings *)
(* compile_bindings :: compile_env -> Syntax.BDef list ->  compile_env * qid list *)
and compile_bindings cev bs = 
  (* add all the bindings in a recursion group to the environment *)
  let bev,names_rev = 
    Safelist.fold_left 
      (fun (bev,names) (Syntax.BDef(_,f,_,_,_)) -> 
	 let qf = Syntax.qid_of_id f in
	   update bev qf Registry.dummy_rv, qf::names)
      (cev,[]) 
      bs 
  in
  (* and backpatch... *)
  let _ = Safelist.map
    (fun bi -> match bi with
       | Syntax.BDef(i,f,[],so,e) ->
	   let r = compile_exp bev e in
	   let s = Registry.sort_of_rv r in
	   let v = Value.memoize (Registry.value_of_rv r) in
	     overwrite bev (Syntax.qid_of_id f) (Registry.make_rv s v)
       | Syntax.BDef(i,_,_,_,_) -> raise (Error.Run_error("Unflattened let binding at " ^ Info.string_of_t i)))
    bs
  in    
    (bev, Safelist.rev names_rev)
      
(* type bindings *)
(* compile_typebindings :: compile_env -> Syntax.typebinding list -> compile_env *)
let compile_typebindings cev ts =   
  (* add to environment and backpatch *)
  let tev,names = 
    Safelist.fold_left 
      (fun (tev,names) (x,_,_) -> 
	 let qx = Syntax.qid_of_id x in
	   update tev (Syntax.qid_of_id x) Registry.dummy_rv, qx::names)
      (cev,[]) 
      ts 
  in
  let _ = Safelist.map
    (fun ti -> match ti with
       | (x,[],t) -> 
	   overwrite 
	     tev
	     (Syntax.qid_of_id x) 
	     (compile_typeexp tev t)
       | (x,_,t)  -> 
	   raise (Error.Run_error("Unflattened type definition at " 
				  ^ Info.string_of_t (Syntax.info_of_id x))))
    ts
  in
    (tev, Safelist.rev names)
      
(* declarations *)
(* compile_decl :: compile_env -> Syntax.decl -> compile_env * qid list*)
let rec compile_decl cev = function
  | Syntax.DLet(i,bs)   -> compile_bindings cev bs       
  | Syntax.DType(i,ts)  -> compile_typebindings cev ts
  | Syntax.DMod(i,n,ds) -> 
      let nq = Syntax.qid_of_id n in
      let new_cev, names = compile_module_aux cev ds in
      let new_cev, names_rev = 
	Safelist.fold_left 
	  (fun (cev,names) q -> 
	     match Registry.lookup new_cev get_ev q with
		 None -> 
		   raise (Error.Run_error("The impossible happened! A just checked declaration vanished at " 
					  ^ Info.string_of_t i))
	       | Some r -> 
		   let nq_dot_q = Syntax.dot nq q in
		     (Registry.update cev get_ev set_ev nq_dot_q r,
		      nq_dot_q::names))
	  (cev,[])
	  names
      in
	(new_cev, Safelist.rev names_rev)
		   
(* modules *)
(* compile_module_aux :: compile_env -> Syntax.decl list -> compile_env *)
and compile_module_aux cev ds =
  Safelist.fold_left 
    (fun (cev,names) di -> 
       let new_cev,new_names = compile_decl cev di in
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
  let ast = Checker.sc_module ast in 
  let _ = compile_module ast in
  (* let _ = prerr_string (Registry.string_of_env (Registry.get_library ())) in *)
    ()
      
let _ = Registry.compile_file_impl := compile_file
