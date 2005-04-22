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
   - rewriting let-defs to simple no-param binders
*)

(* a compile environment is just a naming context and the actual
   compile enviroment *)
type compile_env = Value.qn list * Value.env
let empty = ([], Value.empty)
let get_ev cev = let (_,ev) = cev in ev
let get_ctx cev = let (os,_) = cev in os					  
let put_ev cev ev = let (os,_) = cev in (os,ev)
let lookup cev q = 
  match Value.lookup (get_ev cev) q with
    | Some r -> Some (Value.t_of_rtv r)
    | None -> begin
	match Value.lookup_in_ctx (!library) (get_ctx cev) q with
	  | None -> None
	  | Some r -> Some (Value.t_of_rtv r)
      end	
let update cev q r = put_ev cev (Value.update (get_ev cev) q r)
let overwrite cev q r = put_ev cev (Value.overwrite(get_ev cev) q r)
  
let lookup_qid cev qx = lookup cev (Value.qn_of_qid qx)
let lookup_id cev x = lookup cev ([], Value.n_of_id x)  
let update_id cev x r = update cev (Value.qn_of_id x) r
let overwrite_id cev x r = overwrite cev (Value.qn_of_id x) r
  
let open_qns cev qs = let (os,ev) = cev in (qs@os,ev)

(* a few global definitions *)
let id_exp = let i = Info.bogus in Syntax.EVar(i,([(i,"Pervasives")],(i,"id")))
				     
(* expressions *)
(* compile_exp :: compile_env -> Syntax.exp -> Value.rtv *)
let rec compile_exp cev e0 = match e0 with
  | Syntax.EVar(i,q) -> 
      begin match (lookup_qid cev q) with
	| Some v -> v
	| None -> raise (Error.Run_error(("Unbound variable " 
					  ^ (Pretty.string_of_qid q)
					  ^ "."), i))
      end
	
  | Syntax.EFun(i,xs,so,e) ->
      begin
	match xs,so with 
	  | [p], Some s  -> 
	      let ps = Syntax.sort_of_param p in
	      let f (v:Value.t) : Value.t =  
		let fcev = update_id cev (Syntax.id_of_param p) (ps,v) in
		  Value.t_of_rtv (compile_exp fcev e)
	      in
	      let fs = Syntax.SArrow(i,ps,s) in
		(fs, Value.F(f))
	  | _     -> raise (Error.Run_error(("Bogus function"), i))
      end
	
  | Syntax.EMap(i,ms) ->
      let s = Syntax.SArrow(i, Syntax.SName(i), Syntax.SLens(i)) in
      let f = List.fold_left 
	(fun f (x,l) -> 
	   (fun v -> 
	      match v with
		| Value.N n -> if (n = (Value.n_of_id x)) then
		    Value.t_of_rtv (compile_exp cev l)
		  else 
		    f v
		| _ -> raise (Error.Run_error(("Not a name"), i)))) 
	(fun _ -> Value.t_of_rtv (compile_exp cev id_exp)) 
	ms
      in
	(s, Value.F(f))

  | Syntax.EApp(i,e1,e2) ->
      begin
	match Value.t_of_rtv (compile_exp cev e1) with 	  
	    Value.F(f) -> 
	      let r = compile_exp cev e2 in
		(Value.sort_of_rtv r, f (Value.t_of_rtv r))
	  | _   -> raise (Error.Run_error("Left-hand side of application is not a function.", i))
      end

  | Syntax.ELet(i,bs,e) -> compile_exp (compile_bindings cev bs) e
      
  | Syntax.EName(i,id) -> (Syntax.SName(i), Value.N (Syntax.name_of_id id))

  | Syntax.EType(i,t) -> (Syntax.SType(i), Value.T (compile_typeexp cev t))

  | Syntax.EView(i,ks) -> (Syntax.SView(i), Value.V (compile_viewbinds cev ks))
      
(* types *)
(* compile_exp :: compile_env -> Syntax.typeexp -> Type.t *)
and compile_typeexp cev = 
  let 
      type2cstate = function 
	| Syntax.TExp(_,Syntax.EVar(i,q)) -> (Value.qn_of_qid q,[],[])
	| t -> raise (Error.Run_error(("Unflattened type "
				 ^ Pretty.string_of_typeexp t),
				Syntax.info_of_typeexp t))
  in function
    | Syntax.TEmpty(i)       -> Type.Empty
	
    | Syntax.TName(i,e,t)    ->
	let n =
	  match Value.t_of_rtv (compile_exp cev e) with
	      Value.N n -> n
	    | _     -> raise (Error.Run_error("Name expected.", i))
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
	raise(Error.Run_error(("Diff not implemented."), i))
	  
    | Syntax.TInter(i,t1,t2) -> 
	raise(Error.Run_error(("Inter not implemented."), i))
	  
    | Syntax.TExp(i,e) ->
	begin
	  match Value.t_of_rtv (compile_exp cev e) with
	    | Value.T t -> t
	    | _     -> raise (Error.Run_error("Type expected.",i))
	end
	  
(* views *)
(* compile_viewbinds :: env -> (Info.t * Syntax.exp * Syntax.exp) list -> V.t *)
and compile_viewbinds ev bs = match bs with
  | [] -> V.empty
      
  | (i,ne,ve)::rest ->
      let n =
	match Value.t_of_rtv (compile_exp ev ne) with
	  | Value.N n -> n
	  | _ -> raise (Error.Run_error("Expected a name.", i))
      in
      let v = match Value.t_of_rtv (compile_exp ev ve) with
	| Value.V v -> v
	| _ -> raise (Error.Run_error("Expected a view.", i))
      in
      let vrest = compile_viewbinds ev rest in	
	match (V.get vrest n) with
	    (* simple sanity check *)
	  | Some _ -> raise (Error.Run_error("Repeated name in view.", i))
	  | None   -> V.set vrest n (Some v)
	      
(* bindings *)
(* compile_bindings :: compile_env -> qn option -> Syntax.BDef list ->  compile_env *)
and compile_bindings cev bs = 
  (* add all the bindings in a recursion group to the environment *)
  let bev = 
    List.fold_left 
      (fun bev (Syntax.BDef(_,f,_,_,_)) -> 
	 update bev f Value.dummy_rtv) 
      cev 
      bs 
  in
  (* and backpatch... *)
  let _ = List.map
    (fun bi -> match bi with
       | (Syntax.BDef(i,f,[],so,e)) ->
	   let r = compile_exp bev e in
	   let s = Value.sort_of_rtv r in
	   let v = Value.t_of_rtv r in	     	   
	     overwrite bev f (s, Value.memoize v)
       | (Syntax.BDef(i,f,_,so,e)) -> assert false)
    bs
  in    
    bev
      
(* type bindings *)
(* compile_typebindings :: compile_env * Value.qn option -> Syntax.typebinding list -> compile_env *)
let compile_typebindings cev ts =   
  (* add to environment and backpatch *)
  let tev = 
    List.fold_left 
      (fun tev (x,_,_) -> update tev x Value.dummy_rtv) 
      cev 
      ts 
  in
    List.fold_left 
      (fun cev ti -> 
	 match ti with
	   | (x,[],t) -> overwrite tev x (Syntax.SType(Info.bogus),Value.T(compile_typeexp tev t))
	   | (x,_,t)  -> assert false)
      cev
      ts
      
(* declarations *)
(* compile_decl :: compile_env -> Value.qn option -> Syntax.decl -> compile_env *)
let rec compile_decl ev = function
  | Syntax.DLet(_,bs)   -> compile_bindings ev bs
  | Syntax.DType(_,ts)  -> compile_typebindings ev ts
  | Syntax.DMod(i,n,ds) -> compile_module_aux ev n ds

(* modules *)
(* compile_module_aux :: compile_env -> Value.qn -> Syntax.decl list -> compile_env *)
and compile_module_aux cev ds =
  List.fold_left (fun cev di -> compile_decl cev di) cev ds
    
(* compile_module :: Syntax.modl -> compile_env *)
let compile_module ev = function
  | Syntax.MDef(i,m,nctx,ds) ->      
      (compile_module_aux 
	 (open_qns empty (List.map Value.qn_of_id nctx))
	 ds)
	
