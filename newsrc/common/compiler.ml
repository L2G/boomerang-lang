(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* compiler.ml - the actual compiler                            *)
(*                                                              *)
(* $Id: compiler.ml,v 1.4 2005/04/21 03:27:42 jnfoster Exp $    *)
(*                                                              *)
(****************************************************************)

(* We assume that the checker has already been run:
   - annotating functions and bindings with sorts, 
   - flattening TName, TStar, and TBangs
   - rewriting multi-param functions to lambdas 
   - rewriting let-defs and type-defs to simple no-param binders
*)

(* a compilation environment is just a naming context and a Value.env *)
type compile_env = Syntax.qid list * Registry.env
let empty = ([], Registry.empty)

(* coercions *)
let get_ev cev = let (_,ev) = cev in ev
let set_ev cev ev = let (os,_) = cev in (os,ev)
let set_ctx cev qs = let (os,ev) = cev in (qs@os,ev)
let get_ctx cev = let (os,_) = cev in os					  

(* operations on compile_envs *)
let lookup cev q = Registry.lookup_library cev get_ev get_ctx q
let update cev q r = Registry.update cev get_ev set_ev q r
let overwrite cev q r = Registry.overwrite cev get_ev set_ev q r
    					    
(* a few global definitions *)
let id_exp = let i = Info.bogus in Syntax.EVar(i,([(i,"Pervasives")],(i,"id")))
				     
(* expressions *)
(* compile_exp :: compile_env -> Syntax.exp -> Value.rv *)
let rec compile_exp cev e0 : Registry.rv = match e0 with
  | Syntax.EVar(i,q) -> 
      begin match (lookup cev q) with
	| Some r -> r
	| None -> raise (Error.Run_error("Unbound variable " 
					  ^ (Syntax.string_of_qid q)
					  ^ "."))
      end
	
  | Syntax.EFun(i,xs,so,e) ->
      let r =
	begin
	  match xs, so with 
	    | [p], Some s  -> 
		let ps = Syntax.sort_of_param p in
		let f v = 
		  let fcev = update cev (Syntax.qid_of_id (Syntax.id_of_param p)) (Registry.make_rv ps v) in
		    Registry.value_of_rv (compile_exp fcev e)
		in
		let fs = Syntax.SArrow(i,ps,s) in
		  Registry.make_rv fs (Value.F(f))
	    | _     -> raise (Error.Run_error("Ill-formed function at " ^ Info.string_of_t i))
	end
      in r
	
  | Syntax.EMap(i,ms) ->
      let s = Syntax.SArrow(i, Syntax.SName(i), Syntax.SLens(i)) in
      let f = List.fold_left 
	(fun f (x,l) -> 
	   (fun v -> 
	      match v with
		| Value.N n -> if (Syntax.string_of_id x) = n then
		    Registry.value_of_rv (compile_exp cev l)
		  else 
		    f v
		| _ -> raise (Error.Run_error("Name expected in domain of finite map at " ^ Info.string_of_t i))))
	(fun _ -> Registry.value_of_rv (compile_exp cev id_exp))
	ms
      in
	Registry.make_rv s (Value.F(f))

  | Syntax.EApp(i,e1,e2) ->
      begin
	let rv1 = compile_exp cev e1 in
	  match Registry.value_of_rv rv1, Registry.sort_of_rv rv1 with
	      Value.F(f),Syntax.SArrow(_,ps,rs) -> 
		let rv2 = compile_exp cev e2 in
		  Registry.make_rv rs (f (Registry.value_of_rv rv2))		    
	    | _   -> raise (Error.Run_error("Function expected in left-hand side of application at " ^ Info.string_of_t i))
      end
	
  | Syntax.ELet(i,bs,e) -> compile_exp (compile_bindings cev bs) e
      
  | Syntax.EName(i,id) -> Registry.make_rv (Syntax.SName(i)) (Value.N (Syntax.name_of_id id))

  | Syntax.EType(i,t) -> Registry.make_rv (Syntax.SType(i)) (Value.T (compile_typeexp cev t))
      
  | Syntax.EView(i,ks) -> Registry.make_rv (Syntax.SView(i)) (Value.V (compile_viewbinds cev ks))
      
(* types *)
(* compile_exp :: compile_env -> Syntax.typeexp -> Type.t *)
and compile_typeexp cev = 
  let 
      type2cstate = function 
	| Syntax.TExp(_,Syntax.EVar(i,q)) -> (q,[],[])
	| t -> raise (Error.Run_error("Unflattened type: " 
				      ^ Syntax.string_of_typeexp t 
				      ^ " at "
				      ^ Info.string_of_t (Syntax.info_of_typeexp t)))
  in function
    | Syntax.TEmpty(i)       -> Type.Empty
	
    | Syntax.TName(i,e,t)    ->
	let n =
	  match Registry.value_of_rv (compile_exp cev e) with
	      Value.N n -> n
	    | _     -> raise (Error.Run_error("Name expected at " ^ Info.string_of_t i))
	in
	  Type.Name(n, type2cstate t)
	    
    | Syntax.TBang(i,xl,t)   -> 
	Type.Bang(List.map Syntax.name_of_id xl, type2cstate t)
	  
    | Syntax.TStar(i,xl,t)   -> 
	Type.Star(List.map Syntax.name_of_id xl, type2cstate t)

    | Syntax.TCat(i,ts)      -> 
	Type.Cat(List.map(compile_typeexp cev) ts) 
	  
    | Syntax.TUnion(i,ts)    -> 
	Type.Union(List.map(compile_typeexp cev) ts) 
	  
    | Syntax.TDiff(i,t1,t2)  -> 
	raise(Error.Run_error("Untranslated diff at " ^ Info.string_of_t i))
	  
    | Syntax.TInter(i,t1,t2) -> 
	raise(Error.Run_error("Untranslated inter at " ^ Info.string_of_t i))
	  
    | Syntax.TExp(i,e) ->
	begin
	  match Registry.value_of_rv (compile_exp cev e) with
	    | Value.T t -> t
	    | _     -> raise (Error.Run_error("Type expected at " ^ Info.string_of_t i))
	end
	  
(* views *)
(* compile_viewbinds :: env -> (Info.t * Syntax.exp * Syntax.exp) list -> V.t *)
and compile_viewbinds ev bs = match bs with
  | [] -> V.empty
      
  | (i,ne,ve)::rest ->
      let n =
	match Registry.value_of_rv (compile_exp ev ne) with
	  | Value.N n -> n
	  | _ -> raise (Error.Run_error("Expected a name at " ^ Info.string_of_t i))
      in
      let v = match Registry.value_of_rv (compile_exp ev ve) with
	| Value.V v -> v
	| _ -> raise (Error.Run_error("Expected a view at " ^ Info.string_of_t i))
      in
      let vrest = compile_viewbinds ev rest in	
	match (V.get vrest n) with
	    (* simple sanity check *)
	  | Some _ -> raise (Error.Run_error("Repeated name in view at " ^ Info.string_of_t i))
	  | None   -> V.set vrest n (Some v)
	      
(* bindings *)
(* compile_bindings :: compile_env -> Syntax.BDef list ->  compile_env *)
and compile_bindings cev bs = 
  (* add all the bindings in a recursion group to the environment *)
  let bev = 
    List.fold_left 
      (fun bev (Syntax.BDef(_,f,_,_,_)) -> update bev (Syntax.qid_of_id f) Registry.dummy_rv) 
      cev 
      bs 
  in
  (* and backpatch... *)
  let _ = List.map
    (fun bi -> match bi with
       | Syntax.BDef(i,f,[],so,e) ->
	   let r = compile_exp bev e in
	   let s = Registry.sort_of_rv r in
	   let v = Value.memoize (Registry.value_of_rv r) in
	     overwrite bev (Syntax.qid_of_id f) (Registry.make_rv s v)
       | Syntax.BDef(i,_,_,_,_) -> raise (Error.Run_error("Unflattened let binding at " ^ Info.string_of_t i)))
    bs
  in    
    bev
      
(* type bindings *)
(* compile_typebindings :: compile_env -> Syntax.typebinding list -> compile_env *)
let compile_typebindings cev ts =   
  (* add to environment and backpatch *)
  let tev = 
    List.fold_left (fun tev (x,_,_) -> update tev (Syntax.qid_of_id x) Registry.dummy_rv) cev ts 
  in
    List.fold_left 
      (fun cev ti -> match ti with
	 | (x,[],t) -> overwrite tev (Syntax.qid_of_id x) (Registry.make_rv (Syntax.SType(Info.bogus)) (Value.T(compile_typeexp tev t)))
	 | (x,_,t)  -> raise (Error.Run_error("Unflattened type definition: " ^ Syntax.string_of_typebinding ti)))
      cev
      ts
      
(* declarations *)
(* compile_decl :: compile_env -> Syntax.decl -> compile_env *)
let rec compile_decl ev qm = function
  | Syntax.DLet(i,bs)   -> 
      let bcev = compile_bindings ev bs in
      let _    = 
	List.map 
	  (fun bi -> 
	     let bn = Syntax.qid_of_id (Syntax.id_of_binding bi) in
	     let bv = Registry.lookup bcev get_ev bn in
	       match bv with 
		 | None -> raise (Error.Run_error("The impossible happened! Missing definition in definitions at " ^ Info.string_of_t i))
		 | Some r -> Registry.register (Syntax.dot qm bn) r)
	  bs
      in
	bcev
 
  | Syntax.DType(i,ts)  -> 
      let tcev = compile_typebindings ev ts in
      let _ = List.map
	(fun ti ->
	   let tn = Syntax.qid_of_id (Syntax.id_of_typebinding ti) in
	   let tv = Registry.lookup tcev get_ev tn in
	     match tv with
	       | None -> raise (Error.Run_error("The impossible happened! Missing definition in type definitions at " ^ Info.string_of_t i))
	       | Some r -> Registry.register (Syntax.dot qm tn) r)
	ts
      in
	tcev
  | Syntax.DMod(i,n,ds) -> 
      compile_module_aux ev (Syntax.dot qm (Syntax.qid_of_id n)) ds
	
(* modules *)
(* compile_module_aux :: compile_env -> Syntax.decl list -> compile_env *)
and compile_module_aux cev qm ds =
  List.fold_left (fun cev di -> compile_decl cev qm di) cev ds 
  
(* compile_module :: Syntax.modl -> unit *)
let compile_module (Syntax.MDef(i,m,nctx,ds)) = 
  let cev = set_ctx empty nctx in
  let _ = compile_module_aux cev (Syntax.qid_of_id m) ds in 
    ()
	  
(* compile_file :: string -> unit *)
let compile_file fn = 
  (* FIXME: do this check here 
     let _ = 
     if (mn <> fn) then 
     raise (Error.Sort_error(("Module " 
     ^ (Pretty.string_of_n mn) 
     ^ " must appear in a file called " 
     ^ (Pretty.string_of_n mn) ^ ".fcl"), i))
     in *)     
  let fchan = open_in fn in
  let lex = Lexing.from_channel fchan in
  let ast = Parser.modl Lexer.token lex in
  let ast = Checker.sc_module ast in 
    compile_module ast 
    
let _ = Registry.compile_file_impl := compile_file
